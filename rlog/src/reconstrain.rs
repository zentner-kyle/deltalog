use fact_table::FactTable;
use optimize::Adjustment;
use program::Program;
use rand::Rng;
use std::collections::hash_map::{Entry, HashMap};
use truth_value::TruthValue;
use types::{Clause, Constant, Predicate, Term, TermIndex};
use util::{cumulative_sum, un_cumulative_sum, weighted_index_cumulative_array};

#[allow(dead_code)]
#[derive(Debug)]
struct ConstraintMeasure {
    weight_per_predicate: Vec<f64>,
    cumulative_weight_per_predicate: Vec<f64>,
    unconstraint_demand: HashMap<Predicate, Vec<f64>>,
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
fn compute_term_overconstraint<T>(program: &Program<T>,
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
    let weight_per_predicate = vec![0.0f64; program.num_predicates()];
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

    let mut cumulative_weight_per_predicate = weight_per_predicate.clone();
    cumulative_sum(&mut cumulative_weight_per_predicate);
    return ConstraintMeasure {
               weight_per_predicate: weight_per_predicate,
               cumulative_weight_per_predicate: cumulative_weight_per_predicate,
               unconstraint_demand: unconstraint_demand,
           };
}

impl ConstraintMeasure {
    #[cfg(test)]
    pub fn new(weight_per_predicate: Vec<f64>,
               unconstraint_demand: HashMap<Predicate, Vec<f64>>)
               -> Self {
        let mut cumulative_weight_per_predicate = weight_per_predicate.clone();
        cumulative_sum(&mut cumulative_weight_per_predicate);
        return ConstraintMeasure {
                   weight_per_predicate: weight_per_predicate,
                   cumulative_weight_per_predicate: cumulative_weight_per_predicate,
                   unconstraint_demand: unconstraint_demand,
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
        if let Some(ref mut head) = clause.head {
            match head.terms[term_to_mutate] {
                ref mut term @ Term::Constant(_) => {
                    *term = Term::Variable(rng.gen_range(0, num_vars));
                    return;
                }
                Term::Variable(variable) => {
                    variable_to_mutate = variable;
                }
            }
        } else {
            return;
        }
        let new_var = num_vars + 1;
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
                            *var = new_var;
                            return;
                        }
                        uses_so_far += 1;
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::ConstraintMeasure;
    use fake_rng::FakeRng;
    use std::collections::HashMap;
    use std::f64;

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
}
