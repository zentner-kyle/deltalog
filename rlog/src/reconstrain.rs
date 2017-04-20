use fact_table::FactTable;
use optimize::Adjustment;
use program::Program;
use std::collections::hash_map::{Entry, HashMap};
use truth_value::TruthValue;
use types::{Constant, Predicate, TermIndex};

#[allow(dead_code)]
struct ConstraintMeasure {
    weight_per_predicate: Vec<f64>,
    unconstraint_demand: HashMap<(Predicate, TermIndex), f64>,
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
        match unconstraint_demand.entry((predicate, index)) {
            Entry::Occupied(mut pair) => {
                *pair.get_mut() += demand;
            }
            Entry::Vacant(pair) => {
                pair.insert(demand);
            }
        }
    }
    return ConstraintMeasure {
               weight_per_predicate: weight_per_predicate,
               unconstraint_demand: unconstraint_demand,
           };
}
