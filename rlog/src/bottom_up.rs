use bindings::Bindings;
use fact_table::FactTable;
use program::Program;
use std::mem::swap;
use truth_value::TruthValue;
use types::{Clause, ClauseIndex};

#[derive(Copy, Clone, Debug)]
enum FactSource {
    EDB,
    IDB,
    LastIter,
}

fn match_clause<T>(last_iter: Option<&FactTable<T>>,
                   idb: Option<&FactTable<T>>,
                   edb: Option<&FactTable<T>>,
                   clause: &Clause,
                   clause_idx: ClauseIndex,
                   weight: &T::Dual)
                   -> FactTable<T>
    where T: TruthValue
{
    let mut result_facts = FactTable::new();
    let literals_to_match = clause.body.len();

    let mut last_iter_source_count = 1usize;
    let mut fact_iters = Vec::with_capacity(literals_to_match);
    let default_table;
    let default_source;
    if let Some(last_iter) = last_iter {
        default_table = last_iter;
        default_source = FactSource::LastIter;
    } else if let Some(idb) = idb {
        default_table = idb;
        default_source = FactSource::IDB;
    } else if let Some(edb) = edb {
        default_table = edb;
        default_source = FactSource::EDB;
    } else {
        panic!("No input FactTable");
    }
    if last_iter.is_some() && edb.is_some() && idb.is_none() {
        panic!("Need to provide idb, or not provide edb.");
    }
    fact_iters.push((default_source,
                     default_table.iter(&clause.body[0],
                                        Bindings::with_weight(clause.num_variables(),
                                                              weight.clone()))));
    loop {
        let lit_idx = fact_iters.len() - 1;
        if let Some((binds, _fact, _)) = fact_iters[lit_idx].1.next() {
            if lit_idx + 1 < literals_to_match {
                last_iter_source_count += 1;
                fact_iters.push((default_source,
                                 default_table.iter(&clause.body[lit_idx + 1], binds)));
            } else if let Some(ref head) = clause.head {
                let fact = binds.solidify(head);
                assert!(binds.all_variables_bound_in_clause(clause));
                result_facts.add_match(fact, clause_idx, binds);
            }
        } else {
            let signed_new_lit_idx = fact_iters.len() as isize - 1;
            if signed_new_lit_idx >= 0 {
                let new_lit_idx = signed_new_lit_idx as usize;
                match fact_iters.pop() {
                    Some((FactSource::LastIter, iter)) => {
                        last_iter_source_count -= 1;
                        if last_iter_source_count != 0 || lit_idx != literals_to_match {
                            if let Some(idb) = idb {
                                fact_iters.push((FactSource::IDB,
                                                 idb.iter(&clause.body[new_lit_idx],
                                                          iter.into_bindings())));
                            }
                        }
                    }
                    Some((FactSource::IDB, iter)) => {
                        last_iter_source_count += 1;
                        if let Some(edb) = edb {
                            fact_iters.push((FactSource::EDB,
                                             edb.iter(&clause.body[new_lit_idx],
                                                      iter.into_bindings())));
                        }
                    }
                    Some((FactSource::EDB, _)) => {}
                    None => {}
                }
            }
            if fact_iters.len() == 0 {
                break;
            }
        }
    }
    return result_facts;
}

pub fn evaluate_bottom_up<T>(facts: &mut FactTable<T>, program: &Program<T>)
    where T: TruthValue
{
    let facts_to_add = compute(facts, program);
    facts.merge_new_generation(facts_to_add);
}

pub fn compute<T>(facts: &FactTable<T>, program: &Program<T>) -> FactTable<T>
    where T: TruthValue
{
    let mut result_facts = FactTable::new();
    result_facts.extend_num_predicates(program.num_predicates());
    let mut last_iter = None;
    let mut this_iter = FactTable::new();
    loop {
        let mut fact_added_last_iter = false;
        for ((clause_idx, clause), truth) in
            program
                .clauses
                .iter()
                .enumerate()
                .zip(program.clause_weights.iter().cycle()) {
            let facts_to_add = match_clause(last_iter.as_ref(),
                                            Some(&result_facts),
                                            Some(&facts),
                                            clause,
                                            clause_idx,
                                            truth);
            this_iter.merge_new_generation(facts_to_add);
        }
        if let Some(facts) = last_iter.take() {
            fact_added_last_iter |= result_facts.merge_new_generation(facts);
        } else {
            fact_added_last_iter = true;
        }
        if !fact_added_last_iter {
            return result_facts;
        } else {
            let mut next_iter = FactTable::new();
            swap(&mut this_iter, &mut next_iter);
            last_iter = Some(next_iter);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::evaluate_bottom_up;
    use fact_table::FactTable;
    use program::Program;
    use types::{Clause, Fact, Literal, Term};

    #[test]
    fn run_single_derivation() {
        let mut prg = Program::new();
        // b(X) :- a(X)
        // Where a is predicate 0, b is predicate 1, and X is variable 0.
        prg.clauses
            .push(Clause::new_from_vec(Literal::new_from_vec(1, vec![Term::Variable(0)]),
                                       vec![Literal::new_from_vec(0, vec![Term::Variable(0)])]));
        prg.clause_weights.push(());
        let mut facts = FactTable::<()>::new();
        // a(2)
        let starting_fact = Fact::new_from_vec(0, vec![2]);
        facts.add_fact(starting_fact.clone(), ());
        evaluate_bottom_up(&mut facts, &prg);
        // We expect b(2) to be added to the FactTable.
        let mut expected = FactTable::new();
        expected.add_fact(Fact::new_from_vec(0, vec![2]), ());
        expected.add_fact(Fact::new_from_vec(1, vec![2]), ());
        println!();
        println!("expected: {:#?}", expected);
        println!("received: {:#?}", facts);
        assert!(expected.eq_facts(&facts));
        assert!(facts
                    .get_causes(&starting_fact)
                    .unwrap()
                    .next()
                    .is_none());
    }

    #[test]
    fn run_chained_derivation() {
        let mut prg = Program::new();
        // A(X) :- B(X)
        // B(X) :- C(X)
        // Where A is predicate 0, B is predicate 1, and X is variable 0.
        prg.clauses
            .push(Clause::new_from_vec(Literal::new_from_vec(0, vec![Term::Variable(0)]),
                                       vec![Literal::new_from_vec(1, vec![Term::Variable(0)])]));
        prg.clause_weights.push(());
        prg.clauses
            .push(Clause::new_from_vec(Literal::new_from_vec(1, vec![Term::Variable(0)]),
                                       vec![Literal::new_from_vec(2, vec![Term::Variable(0)])]));
        prg.clause_weights.push(());
        let mut facts = FactTable::<()>::new();
        // c(2)
        let starting_fact = Fact::new_from_vec(2, vec![2]);
        facts.add_fact(starting_fact.clone(), ());
        evaluate_bottom_up(&mut facts, &prg);
        // We expect a(2) to be added to the FactTable.
        let mut expected = FactTable::new();
        expected.add_fact(Fact::new_from_vec(0, vec![2]), ());
        expected.add_fact(Fact::new_from_vec(1, vec![2]), ());
        expected.add_fact(Fact::new_from_vec(2, vec![2]), ());
        assert!(expected.eq_facts(&facts));
        assert!(facts
                    .get_causes(&starting_fact)
                    .unwrap()
                    .next()
                    .is_none());
    }
}
