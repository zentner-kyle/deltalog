use db::{DB};
use matching::{Bindings};
use fact_table::{FactTable, FactTableIter, TruthValue};
use types::{Clause};
use program::{Program};

pub struct BottomUpEvaluator {
    pub db: DB,
    next_clause: usize,
    fact_added_this_iter: bool,
}

fn match_clause_db<T>(db: &DB, clause: &Clause) -> FactTable<T> where T: TruthValue {
    let mut facts_to_add = FactTable::new();
    let literals_to_match = clause.body.len();

    let mut fact_iters = Vec::with_capacity(literals_to_match);
    fact_iters.push(db.get_iter_for_literal(&clause.body[0], Bindings::new(clause.num_variables())));
    loop {
        let lit_idx = fact_iters.len() - 1;
        if let Some((binds, fact, _)) = fact_iters[lit_idx].next() {
            if lit_idx + 1 <= literals_to_match {
                fact_iters.push(db.get_iter_for_literal(&clause.body[lit_idx], binds));
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

fn match_clause<T>(facts: &FactTable<T>, clause: &Clause) -> FactTable<T> where T: TruthValue {
    let mut facts_to_add = FactTable::new();
    let literals_to_match = clause.body.len();

    let mut fact_iters = Vec::with_capacity(literals_to_match);
    fact_iters.push(facts.iter(&clause.body[0], Bindings::new(clause.num_variables())));
    loop {
        let lit_idx = fact_iters.len() - 1;
        if let Some((binds, fact, _)) = fact_iters[lit_idx].next() {
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

impl BottomUpEvaluator {
    pub fn new(db: DB) -> Self {
        BottomUpEvaluator {
            db: db,
            next_clause: 0,
            fact_added_this_iter: false,
        }
    }

    fn step(&mut self) -> bool {
        let facts_to_add = match_clause_db(&self.db, &self.db.program.clauses[self.next_clause]);
        self.next_clause += 1;
        return self.db.add_new_facts(facts_to_add);
    }

    fn run_iter(&mut self) {
        while self.next_clause < self.db.program.clauses.len() {
            self.fact_added_this_iter |= self.step();
        }
        self.next_clause = 0;
    }

    pub fn run(&mut self) {
        loop {
            self.fact_added_this_iter = false;
            self.run_iter();
            if !self.fact_added_this_iter {
                debug!("We've found all the facts we're ever going to find.");
                return;
            } else {
                debug!("There might be more facts to find.");
            }
        }
    }
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
    use super::{BottomUpEvaluator, evaluate_bottom_up};
    use db::{DB};
    use program::{Program};
    use types::{Clause, Term, Literal, Fact};
    use fact_table::{FactTable};

    #[test]
    fn run_single_derivation_db() {
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
        let mut db = DB::new(FactTable::new(), prg);
        // A(2)
        db.add_fact(Fact::new_from_vec(0, vec![
            2
        ]));
        let mut ev = BottomUpEvaluator::new(db);
        ev.run();
        // We expect B(2) to be added to the DB.
        let mut expected = FactTable::new();
        expected.add_fact(Fact::new_from_vec(0, vec![
            2
        ]), ());
        expected.add_fact(Fact::new_from_vec(1, vec![
            2
        ]), ());
        assert_eq!(expected, ev.db.get_fact_table());
    }

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
        // We expect B(2) to be added to the DB.
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
    fn run_chained_derivation_db() {
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
        let mut db = DB::new(FactTable::new(), prg);
        // C(2)
        db.add_fact(Fact::new_from_vec(2, vec![
            2
        ]));
        let mut ev = BottomUpEvaluator::new(db);
        ev.run();
        // We expect A(2) to be added to the DB.
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
        assert_eq!(expected, ev.db.get_fact_table());
    }
}
