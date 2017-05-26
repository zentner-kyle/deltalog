use fact_table::FactTable;
use optimize::Adjustment;
use program::Program;
use rand::Rng;
use selector::{Selector, SelectorResult};
use std::collections::hash_map::{Entry, HashMap};
use truth_value::TruthValue;
use types::{ClauseIndex, Constant, LiteralIndex, Predicate, TermIndex, Variable};
use util::{cumulative_sum, weighted_index_cumulative_array};

#[derive(Debug)]
pub struct ConstraintMeasure {
    // How much each Predicate wants to change.
    weight_per_predicate: Vec<f64>,
    // Cumulative sum of the above.
    cumulative_weight_per_predicate: Vec<f64>,
    // How much each Term wants to be changed for a Predicate.
    unconstraint_demand: HashMap<Predicate, Vec<f64>>,
    // Cumulative sum of the above.
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

// TODO(zentner): Replace found with an iterator, so that sample results can be used.
#[allow(dead_code)]
pub fn compute_constraint_measure<T>(program: &Program<T>,
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
    for ((predicate, term_index, _), demand) in latent_not_found {
        match unconstraint_demand.entry(predicate) {
            Entry::Occupied(mut pair) => {
                pair.get_mut()[term_index] += demand;
            }
            Entry::Vacant(pair) => {
                let mut clause_term_weights = vec![0.0f64; program.get_num_terms(predicate)];
                clause_term_weights[term_index] = demand;
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
               weight_per_predicate,
               cumulative_weight_per_predicate,
               unconstraint_demand,
               cumulative_unconstraint_demand,
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
                   weight_per_predicate,
                   cumulative_weight_per_predicate,
                   unconstraint_demand,
                   cumulative_unconstraint_demand,
               };
    }

    fn gen_range<R>(&self, rng: &mut R, end: usize) -> SelectorResult<usize>
        where R: Rng
    {
        if end == 0 {
            Err("Nothing to choose from.")
        } else {
            Ok(rng.gen_range(0, end))
        }
    }
}

impl Selector for ConstraintMeasure {
    fn choose_clause<R, T>(&mut self,
                           rng: &mut R,
                           program: &Program<T>)
                           -> SelectorResult<ClauseIndex>
        where R: Rng,
              T: TruthValue
    {
        let predicate = weighted_index_cumulative_array(rng,
                                                        &mut self.cumulative_weight_per_predicate);
        let clauses = program
            .clauses_for_predicate(predicate)
            .ok_or("Chose bad predicate.")?;
        rng.choose(clauses).cloned().ok_or("No clauses.")
    }

    fn choose_predicate<R, T>(&mut self,
                              rng: &mut R,
                              _program: &Program<T>,
                              _clause: ClauseIndex)
                              -> SelectorResult<Predicate>
        where R: Rng,
              T: TruthValue
    {
        // This only really makes sense for the head.
        Ok(weighted_index_cumulative_array(rng, &mut self.cumulative_weight_per_predicate))
    }

    fn choose_literal<R, T>(&mut self,
                            rng: &mut R,
                            program: &Program<T>,
                            clause: ClauseIndex)
                            -> SelectorResult<LiteralIndex>
        where R: Rng,
              T: TruthValue
    {
        // TODO(zentner): Don't choose literal uniformly.
        self.gen_range(rng, program.get_clause_by_idx(clause).body.len())
    }

    fn choose_term<R, T>(&mut self,
                         rng: &mut R,
                         program: &Program<T>,
                         clause: ClauseIndex,
                         literal: LiteralIndex)
                         -> SelectorResult<TermIndex>
        where R: Rng,
              T: TruthValue
    {
        let clause = program.get_clause_by_idx(clause);
        let literal = &clause.body[literal];
        let predicate = literal.predicate;
        let cumulative_demand = self.cumulative_unconstraint_demand
            .get(&predicate)
            .ok_or("No demand for Predicate.")?;
        Ok(weighted_index_cumulative_array(rng, cumulative_demand))

    }

    fn choose_variable<R, T>(&mut self,
                             rng: &mut R,
                             program: &Program<T>,
                             clause: ClauseIndex,
                             _literal: LiteralIndex,
                             _term: TermIndex)
                             -> SelectorResult<Variable>
        where R: Rng,
              T: TruthValue
    {
        // TODO(zentner): Don't select variable uniformly.
        self.gen_range(rng, program.get_clause_by_idx(clause).num_variables())
    }

    fn choose_constant<R, T>(&mut self,
                             rng: &mut R,
                             program: &Program<T>,
                             clause: ClauseIndex,
                             _literal: LiteralIndex,
                             _term: TermIndex)
                             -> SelectorResult<Constant>
        where R: Rng,
              T: TruthValue
    {
        // TODO(zentner): Don't select constants uniformly.
        self.gen_range(rng, program.get_clause_by_idx(clause).max_constant())
    }

    fn choose_head_term<R, T>(&mut self,
                              rng: &mut R,
                              program: &Program<T>,
                              clause: ClauseIndex)
                              -> SelectorResult<TermIndex>
        where R: Rng,
              T: TruthValue
    {
        let clause = program.get_clause_by_idx(clause);
        let cumulative_demand = self.cumulative_unconstraint_demand
            .get(&clause.head.predicate)
            .ok_or("No demand for Predicate.")?;
        Ok(weighted_index_cumulative_array(rng, cumulative_demand))
    }

    fn choose_head_variable<R, T>(&mut self,
                                  rng: &mut R,
                                  program: &Program<T>,
                                  clause: ClauseIndex,
                                  _term: TermIndex)
                                  -> SelectorResult<Variable>
        where R: Rng,
              T: TruthValue
    {
        // TODO(zentner): Don't choose uniformly
        let num_variables = program.get_clause_by_idx(clause).num_variables();
        self.gen_range(rng, num_variables)
    }

    fn choose_head_constant<R, T>(&mut self,
                                  rng: &mut R,
                                  program: &Program<T>,
                                  clause: ClauseIndex,
                                  _term: TermIndex)
                                  -> SelectorResult<Constant>
        where R: Rng,
              T: TruthValue
    {
        let clause = program.get_clause_by_idx(clause);
        self.gen_range(rng, clause.max_constant())
    }
}

#[cfg(test)]
mod test {
    use super::ConstraintMeasure;
    use fake_rng::FakeRng;
    use program::Program;
    use selector::Selector;
    use std::collections::HashMap;
    use std::f64;

    #[test]
    fn choose_predicate_first() {
        let mut constraint_measure = ConstraintMeasure::new(vec![1.0, 2.0, 1.0], HashMap::new());
        println!("constraint_measure = {:#?}", constraint_measure);
        let mut rng = FakeRng::new();
        rng.push_f64(0.0);
        let program = Program::<()>::new();
        let pred = constraint_measure.choose_predicate(&mut rng, &program, 0);
        assert_eq!(pred, Ok(0));
    }

    #[test]
    fn choose_predicate_last() {
        let mut constraint_measure = ConstraintMeasure::new(vec![1.0, 2.0, 1.0], HashMap::new());
        println!("constraint_measure = {:#?}", constraint_measure);
        let mut rng = FakeRng::new();
        rng.push_f64(1.0 - f64::EPSILON);
        let program = Program::<()>::new();
        let pred = constraint_measure.choose_predicate(&mut rng, &program, 0);
        assert_eq!(pred, Ok(2));
    }

    #[test]
    fn choose_predicate_middle() {
        let mut constraint_measure = ConstraintMeasure::new(vec![1.0, 2.0, 1.0], HashMap::new());
        println!("constraint_measure = {:#?}", constraint_measure);
        let mut rng = FakeRng::new();
        rng.push_f64(0.5);
        let program = Program::<()>::new();
        let pred = constraint_measure.choose_predicate(&mut rng, &program, 0);
        assert_eq!(pred, Ok(1));
    }

    #[test]
    fn choose_predicate_boundary() {
        let mut constraint_measure = ConstraintMeasure::new(vec![1.0, 2.0, 1.0], HashMap::new());
        println!("constraint_measure = {:#?}", constraint_measure);
        let mut rng = FakeRng::new();
        rng.push_f64(0.25);
        let program = Program::<()>::new();
        let pred = constraint_measure.choose_predicate(&mut rng, &program, 0);
        assert_eq!(pred, Ok(1));
    }
}
