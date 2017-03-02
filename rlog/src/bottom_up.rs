use matching::{Bindings};
use fact_table::{FactTable, TruthValue};
use types::{Clause};
use program::{Program};

fn match_clause<T>(facts: &FactTable<T>, clause: &Clause) -> FactTable<T> where T: TruthValue {
    let mut facts_to_add = FactTable::new();
    let literals_to_match = clause.body.len();

    let mut fact_iters = Vec::with_capacity(literals_to_match);
    fact_iters.push(facts.iter(&clause.body[0], Bindings::new(clause.num_variables())));
    loop {
        let lit_idx = fact_iters.len() - 1;
        if let Some((binds, _fact, _truth)) = fact_iters[lit_idx].next() {
            if lit_idx + 1 <= literals_to_match {
                fact_iters.push(facts.iter(&clause.body[lit_idx], binds));
            } else if let Some(ref head) = clause.head {
                facts_to_add.add_fact(binds.solidify(head), T::default());
            }
        } else {
            if lit_idx == 0 {
                break;
            } else {
                fact_iters.pop();
            }
        }
    }
    return facts_to_add;
}

pub fn evaluate_bottom_up(facts: &mut FactTable<()>, program: &Program) {
    loop {
        let mut fact_added_this_iter = false;
        for clause in &program.clauses {
            let facts_to_add = match_clause(&facts, clause);
            fact_added_this_iter |= facts.merge_new_generation(facts_to_add);
        }
        if !fact_added_this_iter {
            return;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{evaluate_bottom_up};
    use program::{Program};
    use types::{Clause, Term, Literal, Fact};
    use fact_table::{FactTable};

    #[test]
    fn run_single_derivation() {
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
        let mut facts = FactTable::<()>::new();
        // A(2)
        facts.add_fact(Fact::new_from_vec(0, vec![
            2
        ]), ());
        evaluate_bottom_up(&mut facts, &prg);
        // We expect B(2) to be added to the FactTable.
        let mut expected = FactTable::new();
        expected.add_fact(Fact::new_from_vec(0, vec![
            2
        ]), ());
        expected.add_fact(Fact::new_from_vec(1, vec![
            2
        ]), ());
        assert_eq!(expected, facts);
    }

    #[test]
    fn run_chained_derivation() {
        let mut prg = Program::new();
        // A(X) :- B(X)
        // B(X) :- C(X)
        // Where A is predicate 0, B is predicate 1, and X is variable 0.
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
        let mut facts = FactTable::<()>::new();
        // C(2)
        facts.add_fact(Fact::new_from_vec(2, vec![
            2
        ]), ());
        evaluate_bottom_up(&mut facts, &prg);
        // We expect A(2) to be added to the FactTable.
        let mut expected = FactTable::new();
        expected.add_fact(Fact::new_from_vec(0, vec![
            2
        ]), ());
        expected.add_fact(Fact::new_from_vec(1, vec![
            2
        ]), ());
        expected.add_fact(Fact::new_from_vec(2, vec![
            2
        ]), ());
        assert_eq!(expected, facts);
    }
}
