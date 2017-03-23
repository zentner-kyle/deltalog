use std::collections::hash_set::{HashSet};

use truth_value::{TruthValue};
use bottom_up::{evaluate_bottom_up};
use fact_table::{FactTable};
use program::{Program};

pub fn compute_adjustments<T>(program: &Program<T>,
                              mut facts: FactTable<T>,
                              samples: Vec<(FactTable<T>, FactTable<T>)>,
                              max_iters: usize) -> (Vec<T>, FactTable<T>) where T: TruthValue {
    let mut clause_adjustments = vec![T::zero(); program.clauses.len()];
    let mut fact_adjustments: FactTable<T> = FactTable::new();
    evaluate_bottom_up(&mut facts, program);
    for (input, expected) in samples {
        let mut result_from_sample = facts.clone();
        result_from_sample.merge_new_generation(input);
        evaluate_bottom_up(&mut result_from_sample, program);
        let mut frontier: Vec<_> = expected.all_facts().into_iter().map(|(f, t)| (f, t, HashSet::new())).collect();
        let mut iterations = 0;
        while let Some((fact, expected_confidence, entailers)) = frontier.pop() {
            if let Some(found_confidence) = result_from_sample.get(&fact).cloned() {
                let error = T::sub(&found_confidence, &expected_confidence);
                let previous_adjustment = fact_adjustments.get(&fact).cloned().unwrap_or_else(|| T::zero());
                let new_adjustment = T::sum(&previous_adjustment, &error);
                fact_adjustments.set(fact.clone(), new_adjustment);
                if iterations < max_iters {
                    iterations += 1;
                    if let Some(causes) = result_from_sample.get_causes(&fact) {
                        for (clause_idx, binds) in causes {
                            clause_adjustments[clause_idx] = T::sum(&error, &clause_adjustments[clause_idx]);
                            let literal = &program.clauses[clause_idx].head.as_ref().unwrap();
                            let causing_fact = binds.solidify(literal);
                            if !entailers.contains(&causing_fact) {
                                // Loops in the fact graph are essentially anomalous and not worth
                                // computing properly. The first time a fact is encountered in its own
                                // entailer table, we just drop it from propagation.
                                let new_expected_confidence = T::back_finalize(&program.clause_weights[clause_idx],
                                                                               &found_confidence, &error);
                                let mut new_entailers = entailers.clone();
                                new_entailers.insert(fact.clone());
                                frontier.push((causing_fact, new_expected_confidence, new_entailers));
                            }
                        }
                    }
                }
            }
        }
    }
    return (clause_adjustments, fact_adjustments);
}

#[cfg(test)]
mod tests {
    use truth_value::{MaxFloat64, MaxFloat64Dual};
    use super::{compute_adjustments};
    use program::{Program};
    use types::{Clause, Term, Literal, Fact};
    use fact_table::{FactTable};

    #[test]
    fn maximizes_good_weight() {
        let mut prg = Program::new();
        // B(X) :- A(X)
        // Where A is predicate 0, B is predicate 1, and X is variable 0.
        prg.clauses.push(Clause::new_from_vec(
            Literal::new_from_vec(1, vec![
                Term::Variable(0)
            ]),
            vec![
                Literal::new_from_vec(0, vec![
                    Term::Variable(0)
                ])
            ])
        );
        prg.clause_weights.push(MaxFloat64Dual(0.0));
        let mut facts = FactTable::<MaxFloat64>::new();
        let input_fact = Fact::new_from_vec(0, vec![
            2
        ]);
        // A(2)
        facts.add_fact(input_fact.clone(), MaxFloat64(1.0));
        // We expect B(2) to be added to the FactTable.
        let mut expected = FactTable::new();
        expected.add_fact(Fact::new_from_vec(0, vec![
            2
        ]), MaxFloat64(1.0));
        expected.add_fact(Fact::new_from_vec(1, vec![
            2
        ]), MaxFloat64(1.0));
        let (clause_diff, mut fact_diff) = compute_adjustments(&prg, facts.clone(), vec![(facts.clone(), expected)], 100);
        assert_eq!(clause_diff[0], MaxFloat64(1.0));
        assert_eq!(fact_diff.get(&input_fact), Some(&MaxFloat64(0.0)));
    }
}
