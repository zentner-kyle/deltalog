
use bottom_up::evaluate_bottom_up;
use fact_table::FactTable;
use program::Program;
use std::collections::hash_set::HashSet;

use truth_value::TruthValue;

/// The following math probably needs to be cleaned up in a variety of ways.
/// There's j indices everywhere, but they're not really meaningful, since they're literally
/// everywhere. They were supposed to indicate the current clause or something.
/// If computing the original truth values uses the following formulae:
///
/// i is the index of the literal from which the fact is drawn.
/// j is the index of the cause of the fact.
/// w[j] is the confidence in the fact after seeing the first j causes.
///
/// each literal i: v[i, j] = both v[i - 1, j], u[i - 1, j]
/// w[j] = finalize q, v[LAST, j]
/// each cause j: x[j] = either x[j - 1], w[j]
/// z = x[LAST]
///
/// Then the adjoints are computed using the following formulae:
///
/// gx[LAST] = gz
/// each cause j: gx[j - 1], gw[j] = back_either x[j - 1], w[j], gx[j]
/// gq[j], gv[LAST, j] = back_finalize q, v[LAST, j], gw[j]
/// each literal i: gv[i - 1, j], gu[i - 1] = back_both v[i - 1, j], u[i - 1, j], gv[i, j]
///
/// w[j] is binds.truth()
/// v[LAST, j] is binds.unfinalized_truth()
/// u[i] is facts.get(&binds.solidify(clause.body[i])), i.e. the truth of the i'th clause input
/// q is, program.clause_weights[clause_idx], the weight of the clause
/// z is facts.get(&fact)
/// x[LAST] is facts.get(&fact)
/// x[j - 1] is T::either(x_last, binds.truth()) (either x[j], w[j])
/// v[i, j] is T::both(v_last, facts.get(&binds.solidify(clause.body[i])))
///            (both v[i - 1, j], u[i - 1])
/// sum gq is clause_adjustments
/// gu is apparent_fact_adjustments
/// gz is error
#[allow(unused_mut)]
pub fn gradient_step<T>(program: &Program<T>,
                        mut facts: FactTable<T>,
                        expected: &FactTable<T>,
                        clause_adjustments: &mut Vec<T::Dual>,
                        apparent_fact_adjustments: &mut FactTable<T>,
                        latent_fact_adjustments: &mut FactTable<T>,
                        max_iters: usize)
    where T: TruthValue
{
    // Convert expected truth values into initial adjoints.
    let mut frontier: Vec<_> = expected
        .all_facts_iter()
        .map(|(fact, truth)| {
                 let z = facts.get(fact).cloned().unwrap_or_else(|| T::zero());
                 let gz = T::sub(truth, &z);
                 (fact.clone(), z, gz, HashSet::new())
             })
        .collect();
    let mut iterations = 0;
    while let Some((fact, z, gz, entailers)) = frontier.pop() {
        if iterations > max_iters {
            return;
        } else {
            iterations += 1;
        }
        if let Some(causes) = facts.get_causes_unmut(&fact) {
            let mut x_last = z;
            let mut gx_last = gz.clone();
            for (clause_idx, binds) in causes {
                let q = &program.clause_weights[clause_idx];
                let w = binds.truth();
                let v_last = binds.unfinalized_truth();
                let x = T::either(&x_last, &w);
                let (gw, gx) = T::back_either(&w, &x, &gx_last);
                x_last = x;
                gx_last = gx;
                let (gq, mut gv_last) = T::back_finalize(&q, &v_last, &gw);
                clause_adjustments[clause_idx] = T::dual_sum(&clause_adjustments[clause_idx], &gq);
                for literal in &program.clauses[clause_idx].body {
                    // TODO(zentner): Remove this allocation in the innermost loop.
                    let causing_fact = binds.solidify(literal);
                    if entailers.contains(&causing_fact) {
                        // Loops in the fact graph are essentially anomalous and not worth
                        // computing properly. The first time a fact is encountered in its own
                        // entailer table, we just drop it from propagation.
                        continue;
                    }
                    let mut new_entailers = entailers.clone();
                    new_entailers.insert(fact.clone());
                    let u = facts.get_unmut(&causing_fact).unwrap().clone();
                    let v = T::both(&v_last, &u);
                    let (gu, gv) = T::back_both(&u, &v, &gv_last);
                    frontier.push((causing_fact, u, gu, new_entailers));
                    gv_last = gv;
                }
            }
            let old_adjustment = apparent_fact_adjustments
                .get(&fact)
                .cloned()
                .unwrap_or_else(|| T::zero());
            let new_adjustment = T::sum(&gz, &old_adjustment);
            apparent_fact_adjustments.set(fact, new_adjustment);
        } else {
            let old_adjustment = latent_fact_adjustments
                .get(&fact)
                .cloned()
                .unwrap_or_else(|| T::zero());
            let new_adjustment = T::sum(&gz, &old_adjustment);
            latent_fact_adjustments.set(fact, new_adjustment);
        }
    }
}

pub struct Adjustment<T>
    where T: TruthValue
{
    pub clause_adjustments: Vec<T::Dual>,
    pub apparent_fact_adjustments: FactTable<T>,
    pub latent_fact_adjustments: FactTable<T>,
}


pub fn compute_adjustments<T>(program: &Program<T>,
                              facts: &FactTable<T>,
                              samples: &Vec<(FactTable<T>, FactTable<T>)>,
                              max_iters: usize)
                              -> Adjustment<T>
    where T: TruthValue
{
    let mut clause_adjustments = vec![T::dual_zero(); program.clauses.len()];
    let mut latent_fact_adjustments: FactTable<T> = FactTable::new();
    let mut apparent_fact_adjustments: FactTable<T> = FactTable::new();
    let mut facts = facts.clone();
    evaluate_bottom_up(&mut facts, program);
    for &(ref input, ref expected) in samples {
        let mut result_from_sample = facts.clone();
        result_from_sample.merge_new_generation(input.clone());
        evaluate_bottom_up(&mut result_from_sample, program);
        gradient_step(program,
                      result_from_sample,
                      expected,
                      &mut clause_adjustments,
                      &mut apparent_fact_adjustments,
                      &mut latent_fact_adjustments,
                      max_iters);
    }
    return Adjustment {
               clause_adjustments: clause_adjustments,
               apparent_fact_adjustments: apparent_fact_adjustments,
               latent_fact_adjustments: latent_fact_adjustments,
           };
}

#[cfg(test)]
mod tests {
    use super::compute_adjustments;
    use fact_table::FactTable;
    use program::Program;
    use truth_value::{MaxFloat64, MaxFloat64Dual};
    use types::{Clause, Fact, Literal, Term};

    #[test]
    fn maximizes_good_weight_in_single_clause() {
        let mut prg = Program::new();
        // b(X) :- a(X)
        // Where a is predicate 0, b is predicate 1, and X is variable 0.
        prg.clauses
            .push(Clause::new_from_vec(Literal::new_from_vec(1, vec![Term::Variable(0)]),
                                       vec![Literal::new_from_vec(0, vec![Term::Variable(0)])]));
        prg.clause_weights.push(MaxFloat64Dual(0.0));
        let mut facts = FactTable::<MaxFloat64>::new();
        // a(2)
        let input_fact = Fact::new_from_vec(0, vec![2]);
        facts.add_fact(input_fact.clone(), MaxFloat64(1.0));
        // We expect b(2) to be added to the FactTable.
        let mut expected = FactTable::<MaxFloat64>::new();
        expected.add_fact(Fact::new_from_vec(1, vec![2]), MaxFloat64(1.0));
        let res = compute_adjustments(&prg, &facts, &vec![(facts.clone(), expected)], 100);
        let (clause_diff, mut fact_diff) = (res.clause_adjustments, res.latent_fact_adjustments);
        assert!(clause_diff[0].0 > 0.0);
        assert_eq!(clause_diff[0], MaxFloat64Dual(1.0));
        // The following isn't a very important property.
        // But it would be bad to change it accidentally.
        assert_eq!(fact_diff.get(&input_fact), None);
    }

    #[test]
    fn maximizes_good_weight_in_sequential_clauses() {
        let mut prg = Program::new();
        // a(X) :- b(X)
        // b(X) :- c(X)
        // Where a is predicate 0, b is predicate 1, c is predicate 2, and X is variable 0.
        prg.clauses
            .push(Clause::new_from_vec(Literal::new_from_vec(0, vec![Term::Variable(0)]),
                                       vec![Literal::new_from_vec(1, vec![Term::Variable(0)])]));
        prg.clause_weights.push(MaxFloat64Dual(0.1));
        prg.clauses
            .push(Clause::new_from_vec(Literal::new_from_vec(1, vec![Term::Variable(0)]),
                                       vec![Literal::new_from_vec(2, vec![Term::Variable(0)])]));
        prg.clause_weights.push(MaxFloat64Dual(0.1));
        let mut facts = FactTable::<MaxFloat64>::new();
        // c(2)
        let input_fact = Fact::new_from_vec(2, vec![2]);
        facts.add_fact(input_fact.clone(), MaxFloat64(1.0));
        // We expect a(2) to be added to the FactTable.
        let mut expected = FactTable::<MaxFloat64>::new();
        expected.add_fact(Fact::new_from_vec(0, vec![2]), MaxFloat64(1.0));
        let res = compute_adjustments(&prg, &facts, &vec![(facts.clone(), expected)], 100);
        let (clause_diff, mut fact_diff) = (res.clause_adjustments, res.latent_fact_adjustments);
        assert!(clause_diff[0].0 > 0.0);
        assert!(clause_diff[1].0 > 0.0);
        assert_eq!(fact_diff.get(&input_fact), None);
    }
}
