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
            let found_confidence = result_from_sample.get(&fact).cloned().unwrap_or(T::zero());
            let error = T::sub(&expected_confidence, &found_confidence);
            let previous_adjustment = fact_adjustments.get(&fact).cloned().unwrap_or_else(|| T::zero());
            let new_adjustment = T::sum(&previous_adjustment, &error);
            fact_adjustments.set(fact.clone(), new_adjustment);
            if iterations < max_iters {
                iterations += 1;
                if let Some(causes) = result_from_sample.get_causes(&fact) {
                    for (clause_idx, binds) in causes {
                        clause_adjustments[clause_idx] = T::sum(&error, &clause_adjustments[clause_idx]);
                        for literal in &program.clauses[clause_idx].body {
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
    fn maximizes_good_weight_in_single_clause() {
        let mut prg = Program::new();
        // b(X) :- a(X)
        // Where a is predicate 0, b is predicate 1, and X is variable 0.
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
        // a(2)
        let input_fact = Fact::new_from_vec(0, vec![
            2
        ]);
        facts.add_fact(input_fact.clone(), MaxFloat64(1.0));
        // We expect b(2) to be added to the FactTable.
        //let mut expected = facts.clone();
        let mut expected = FactTable::<MaxFloat64>::new();
        expected.add_fact(Fact::new_from_vec(1, vec![2]), MaxFloat64(1.0));
        let (clause_diff, mut fact_diff) = compute_adjustments(&prg, facts.clone(), vec![(facts.clone(), expected)], 100);
        assert_eq!(clause_diff[0], MaxFloat64(1.0));
        assert_eq!(fact_diff.get(&input_fact), Some(&MaxFloat64(1.0)));
    }

    #[test]
    fn maximizes_good_weight_in_sequential_clauses() {
        let mut prg = Program::new();
        // a(X) :- b(X)
        // b(X) :- c(X)
        // Where a is predicate 0, b is predicate 1, c is predicate 2, and X is variable 0.
        prg.clauses.push(Clause::new_from_vec(
            Literal::new_from_vec(0, vec![
                Term::Variable(0)
            ]),
            vec![
                Literal::new_from_vec(1, vec![
                    Term::Variable(0)
                ])
            ])
        );
        prg.clause_weights.push(MaxFloat64Dual(0.0));
        prg.clauses.push(Clause::new_from_vec(
            Literal::new_from_vec(1, vec![
                Term::Variable(0)
            ]),
            vec![
                Literal::new_from_vec(2, vec![
                    Term::Variable(0)
                ])
            ])
        );
        prg.clause_weights.push(MaxFloat64Dual(0.0));
        let mut facts = FactTable::<MaxFloat64>::new();
        // c(2)
        let input_fact = Fact::new_from_vec(2, vec![2]);
        facts.add_fact(input_fact.clone(), MaxFloat64(1.0));
        // We expect a(2) to be added to the FactTable.
        let mut expected = FactTable::<MaxFloat64>::new();
        expected.add_fact(Fact::new_from_vec(0, vec![2]), MaxFloat64(1.0));
        let (clause_diff, mut fact_diff) = compute_adjustments(&prg, facts.clone(), vec![(facts.clone(), expected)], 100);
        assert_eq!(clause_diff[0], MaxFloat64(1.0));
        assert_eq!(clause_diff[1], MaxFloat64(1.0));
        assert_eq!(fact_diff.get(&input_fact), Some(&MaxFloat64(1.0)));
    }
}
