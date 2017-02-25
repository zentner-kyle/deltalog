use db::{DB};
use matching::{Bindings};
use fact_table::{FactTable, FactTableIter};

pub struct BottomUpEvaluator {
    pub db: DB,
    next_clause: usize,
    fact_added_this_iter: bool,
}

impl BottomUpEvaluator {
    pub fn new(db: DB) -> Self {
        BottomUpEvaluator {
            db: db,
            next_clause: 0,
            fact_added_this_iter: false,
        }
    }

    fn step(&mut self) {
        let mut facts_to_add = FactTable::<()>::new();
        {
            let clause = &self.db.program.clauses[self.next_clause];
            self.next_clause += 1;
            assert!(clause.is_valid());
            let literals_to_match = clause.body.len();

            let mut lit_idx = 0;
            let mut fact_iters = Vec::new();
            fact_iters.push(self.db.get_iter_for_literal(&clause.body[0], Bindings::new(clause.num_variables())));
            let mut done = false;
            // The comments for this algorithm describe a motion over a 2D "table".
            // For each literal in the body of the current clause, the "table" has a row.
            // Moving right corresponds to using a different fact for the same literal.
            // Moving down corresponds to matching a different literal.
            // Note that reach row is a different length, and is actually an iterator from the DB of
            // the facts for a predicate.
            while !done {
                let lit = &clause.body[lit_idx];
                debug!("Try to move right");
                if let Some((binds, fact, _)) = fact_iters[lit_idx].next() {
                    debug!("There was something for us to move right into.");
                    // Try to move down to before the beginning of the next row.
                    if lit_idx + 1 < literals_to_match {
                        debug!("We can move down.");
                        lit_idx += 1;
                        // Remember our constraints from this cell for the next row.
                        fact_iters.push(self.db.get_iter_for_literal(lit, binds));
                    } else {
                        debug!("We're in the last row!");
                        // Output a new fact, assuming the head has any literals.
                        if let Some(ref head) = clause.head {
                            debug!("Output a fact to the temporary array.");
                            facts_to_add.add_fact(binds.solidify(head), ());
                        }
                    }
                } else {
                    debug!("But there was no new cell to move right into.");
                    debug!("We should move up.");
                    if lit_idx == 0 {
                        debug!("But we're already at the top!");
                        done = true;
                    } else {
                        debug!("We can move up.");
                        // Forget where we were on this row, since we want to restart before the
                        // beginning of the row next time.
                        fact_iters.pop();
                        lit_idx -= 1;
                    }
                }
            }
        }

        for fact in facts_to_add.all_facts() {
            self.fact_added_this_iter |= self.db.add_fact(fact);
        }

    }

    fn run_iter(&mut self) {
        while self.next_clause < self.db.program.clauses.len() {
            self.step();
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

#[cfg(test)]
mod tests {
    use super::{BottomUpEvaluator};
    use db::{DB};
    use program::{Program};
    use types::{Clause, Term, Literal, Fact};
    use fact_table::{FactTable};

    #[test]
    fn run_no_clauses() {
        let prg = Program::new();
        let db = DB::new(prg);
        let mut ev = BottomUpEvaluator::new(db);
        ev.run();
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
        let mut db = DB::new(prg);
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
        let mut db = DB::new(prg);
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
