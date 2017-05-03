use fact_table::FactTable;
use optimize::Adjustment;
use program::Program;
use rand::Rng;
use std::collections::hash_map::{Entry, HashMap};
use truth_value::TruthValue;
use types::{Clause, ClauseIndex, Constant, Literal, Predicate, Term, TermIndex, Variable};
use util::{cumulative_sum, un_cumulative_sum, weighted_index_cumulative_array};

#[allow(dead_code)]
#[derive(Debug)]
struct ConstraintMeasure {
    weight_per_predicate: Vec<f64>,
    cumulative_weight_per_predicate: Vec<f64>,
    unconstraint_demand: HashMap<Predicate, Vec<f64>>,
    cumulative_unconstraint_demand: HashMap<Predicate, Vec<f64>>,
    // TODO(zentner): Add constraint_demand.
}

fn compute_concrete_term_totals<T>(facts: &FactTable<T>)
                                   -> HashMap<(Predicate, TermIndex, Constant), T>
    where T: TruthValue
{
    let mut totals = HashMap::new();
    for (fact, truth) in facts.all_facts_iter() {
        for (index, constant) in fact.terms.iter().enumerate() {
            match totals.entry((fact.predicate, index, *constant)) {
                Entry::Occupied(mut pair) => {
                    let new_truth = T::sum(pair.get(), truth);
                    pair.insert(new_truth);
                }
                Entry::Vacant(pair) => {
                    pair.insert(truth.clone());
                }
            }
        }
    }
    return totals;
}

#[allow(dead_code)]
fn compute_constraint_measure<T>(program: &Program<T>,
                                 found: &FactTable<T>,
                                 adjustment: &Adjustment<T>)
                                 -> ConstraintMeasure
    where T: TruthValue
{
    // Compute a total T value for each concrete term value (predicate, index, constant).
    let presences = compute_concrete_term_totals(found);

    let latent_demand = compute_concrete_term_totals(&adjustment.latent_fact_adjustments);

    let mut latent_not_found: HashMap<(Predicate, TermIndex, Constant), f64> =
        HashMap::with_capacity(latent_demand.len());

    for (key, truth) in latent_demand {
        if let Some(found_truth) = presences.get(&key) {
            latent_not_found.insert(key, T::mag(&T::sub(&truth, found_truth)));
        } else {
            latent_not_found.insert(key, T::mag(&truth));
        }
    }

    let mut unconstraint_demand = HashMap::new();
    for ((predicate, index, _), demand) in latent_not_found {
        match unconstraint_demand.entry(predicate) {
            Entry::Occupied(mut pair) => {
                pair.get_mut()[index] += demand;
            }
            Entry::Vacant(pair) => {
                let mut clause_term_weights = vec![0.0f64; program.get_num_terms(predicate)];
                clause_term_weights[index] = demand;
                pair.insert(clause_term_weights);
            }
        }
    }
    let mut cumulative_unconstraint_demand = unconstraint_demand.clone();
    for demand in cumulative_unconstraint_demand.values_mut() {
        cumulative_sum(demand);
    }

    let weight_per_predicate: Vec<f64> = (0..program.num_predicates())
        .map(|p| {
                 cumulative_unconstraint_demand
                     .get(&p)
                     .map(|d| d[d.len() - 1])
                     .unwrap_or(0.0)
             })
        .collect();

    let mut cumulative_weight_per_predicate = weight_per_predicate.clone();
    cumulative_sum(&mut cumulative_weight_per_predicate);

    return ConstraintMeasure {
               weight_per_predicate: weight_per_predicate,
               cumulative_weight_per_predicate: cumulative_weight_per_predicate,
               unconstraint_demand: unconstraint_demand,
               cumulative_unconstraint_demand: cumulative_unconstraint_demand,
           };
}

impl ConstraintMeasure {
    #[cfg(test)]
    pub fn new(weight_per_predicate: Vec<f64>,
               unconstraint_demand: HashMap<Predicate, Vec<f64>>)
               -> Self {
        let mut cumulative_weight_per_predicate = weight_per_predicate.clone();
        cumulative_sum(&mut cumulative_weight_per_predicate);
        let mut cumulative_unconstraint_demand = unconstraint_demand.clone();
        for demand in cumulative_unconstraint_demand.values_mut() {
            cumulative_sum(demand);
        }
        return ConstraintMeasure {
                   weight_per_predicate: weight_per_predicate,
                   cumulative_weight_per_predicate: cumulative_weight_per_predicate,
                   unconstraint_demand: unconstraint_demand,
                   cumulative_unconstraint_demand: cumulative_unconstraint_demand,
               };
    }

    #[allow(dead_code)]
    pub fn choose_predicate<R>(&mut self, rng: &mut R) -> Predicate
        where R: Rng
    {
        weighted_index_cumulative_array(rng, &mut self.cumulative_weight_per_predicate)
    }

    #[allow(dead_code)]
    pub fn mutate_unconstrain_clause<R>(&mut self, rng: &mut R, clause: &mut Clause)
        where R: Rng
    {
        let term_to_mutate;
        let predicate;
        if let Some(ref head) = clause.head {
            predicate = head.predicate;
        } else {
            return;
        }
        if let Some(ref mut per_term_weight) = self.unconstraint_demand.get_mut(&predicate) {
            cumulative_sum(per_term_weight);
            term_to_mutate = weighted_index_cumulative_array(rng, per_term_weight);
            un_cumulative_sum(per_term_weight);
        } else {
            return;
        }
        let num_vars = clause.num_variables();
        let variable_to_mutate;
        let mut need_to_add_new_var = false;
        if let Some(ref mut head) = clause.head {
            match head.terms[term_to_mutate] {
                ref mut term @ Term::Constant(_) => {
                    variable_to_mutate = rng.gen_range(0, num_vars + 1);
                    *term = Term::Variable(variable_to_mutate);
                    if variable_to_mutate == num_vars {
                        need_to_add_new_var = true;
                    } else {
                        return;
                    }
                }
                Term::Variable(variable) => {
                    variable_to_mutate = variable;
                }
            }
        } else {
            return;
        }
        if need_to_add_new_var {
            self.mutate_add_new_var(rng, &mut clause.body, variable_to_mutate);
            return;
        }
        let num_body_variable_uses: usize = clause
            .body
            .iter()
            .map(|lit| lit.num_times_variable_appears(variable_to_mutate))
            .sum();
        let use_to_mutate: usize = rng.gen_range(0, num_body_variable_uses);
        let mut uses_so_far = 0;
        for lit in clause.body.iter_mut() {
            for term in lit.terms.iter_mut() {
                if let &mut Term::Variable(ref mut var) = term {
                    if *var == variable_to_mutate {
                        if uses_so_far == use_to_mutate {
                            *var = num_vars;
                            return;
                        }
                        uses_so_far += 1;
                    }
                }
            }
        }
    }

    fn mutate_add_new_var<R>(&mut self,
                             rng: &mut R,
                             clause_body: &mut Vec<Literal>,
                             variable: Variable)
                             -> bool
        where R: Rng
    {
        let mut lit_num_constants = vec![0.0f64; clause_body.len()];
        for (lit_idx, lit) in clause_body.iter().enumerate() {
            for term in &lit.terms {
                if let &Term::Constant(_) = term {
                    lit_num_constants[lit_idx] += 1.0;
                }
            }
        }
        cumulative_sum(&mut lit_num_constants);
        if lit_num_constants[lit_num_constants.len() - 1] == 0.0 {
            return false;
        }
        let lit_to_use = weighted_index_cumulative_array(rng, &mut lit_num_constants);
        let num_constants = if lit_to_use == 0 {
            lit_num_constants[lit_to_use]
        } else {
            lit_num_constants[lit_to_use] - lit_num_constants[lit_to_use - 1]
        } as usize;
        let mut constant_num = rng.gen_range(0, num_constants);
        for term in clause_body[lit_to_use].terms.iter_mut() {
            if let term @ &mut Term::Constant(_) = term {
                if constant_num == 0 {
                    *term = Term::Variable(variable);
                    return true;
                }
                constant_num -= 1;
            }
        }
        unreachable!();
    }

    pub fn choose_clause<R, T>(&mut self, rng: &mut R, program: &Program<T>) -> ClauseIndex
        where R: Rng,
              T: TruthValue
    {
        loop {
            let predicate = self.choose_predicate(rng);
            if let Some(clauses) = program.clauses_for_predicate(predicate) {
                return *rng.choose(clauses).unwrap();
            }
        }
    }

    pub fn make_new_clause<R, T>(&mut self, rng: &mut R, program: &mut Program<T>) -> Clause
        where R: Rng,
              T: TruthValue
    {
        let clause_idx = self.choose_clause(rng, program);
        let mut clause = program.get_clause_by_idx(clause_idx).clone();
        self.mutate_unconstrain_clause(rng, &mut clause);
        return clause;
    }

    #[allow(dead_code)]
    pub fn insert_new_clause<R, T>(&mut self, rng: &mut R, program: &mut Program<T>)
        where R: Rng,
              T: TruthValue
    {
        let clause = self.make_new_clause(rng, program);
        program.push_clause_simple(clause);
    }
}

#[cfg(test)]
mod test {
    use super::{ConstraintMeasure, compute_constraint_measure};
    use bottom_up::evaluate_bottom_up;
    use fake_rng::FakeRng;
    use optimize::compute_adjustments;
    use parser::program;
    use rand::SeedableRng;
    use rand::XorShiftRng;
    use std::collections::HashMap;
    use std::f64;
    use truth_value::MaxFloat64;
    use types::{Clause, Literal, Term};

    #[test]
    fn choose_predicate_first() {
        let mut constraint_measure = ConstraintMeasure::new(vec![1.0, 2.0, 1.0], HashMap::new());
        println!("constraint_measure = {:#?}", constraint_measure);
        let mut rng = FakeRng::new();
        rng.push_f64(0.0);
        let pred = constraint_measure.choose_predicate(&mut rng);
        assert_eq!(pred, 0);
    }

    #[test]
    fn choose_predicate_last() {
        let mut constraint_measure = ConstraintMeasure::new(vec![1.0, 2.0, 1.0], HashMap::new());
        println!("constraint_measure = {:#?}", constraint_measure);
        let mut rng = FakeRng::new();
        rng.push_f64(1.0 - f64::EPSILON);
        let pred = constraint_measure.choose_predicate(&mut rng);
        assert_eq!(pred, 2);
    }

    #[test]
    fn choose_predicate_middle() {
        let mut constraint_measure = ConstraintMeasure::new(vec![1.0, 2.0, 1.0], HashMap::new());
        println!("constraint_measure = {:#?}", constraint_measure);
        let mut rng = FakeRng::new();
        rng.push_f64(0.5);
        let pred = constraint_measure.choose_predicate(&mut rng);
        assert_eq!(pred, 1);
    }

    #[test]
    fn choose_predicate_boundary() {
        let mut constraint_measure = ConstraintMeasure::new(vec![1.0, 2.0, 1.0], HashMap::new());
        println!("constraint_measure = {:#?}", constraint_measure);
        let mut rng = FakeRng::new();
        rng.push_f64(0.25);
        let pred = constraint_measure.choose_predicate(&mut rng);
        assert_eq!(pred, 1);
    }

    #[test]
    fn improves_single_clause() {
        let mut rng = XorShiftRng::from_seed([0xde, 0xad, 0xbe, 0xef]);
        let (mut facts, mut program, samples) = program::<MaxFloat64>(r#"
            types(0) :- a(0), b(0)
            a(3) :- b(3)
            sample
                b(1)
            output
                a(1).
            sample
                b(2)
            output
                a(2).
        "#)
                .unwrap()
                .0;
        evaluate_bottom_up(&mut facts, &program);
        let adjustments = compute_adjustments(&program, &facts, &samples, 10);
        let mut constraint = compute_constraint_measure(&program, &facts, &adjustments);
        let clause = constraint.make_new_clause(&mut rng, &mut program);
        assert_eq!(Clause::new_from_vec(Literal::new_from_vec(1, vec![Term::Variable(0)]),
                                        vec![Literal::new_from_vec(2, vec![Term::Variable(0)])]),
                   clause);
    }
}
