use db::{DB};
use matching::{Bindings};

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
        let clause = &self.db.program.clauses[self.next_clause];
        self.next_clause += 1;
        assert!(clause.is_valid());
        let literals_to_match = clause.body.len();

        let mut bindings_per_lit = Vec::with_capacity(literals_to_match);
        bindings_per_lit.push(Bindings::new(clause.num_variables()));

        let mut facts_to_add = Vec::new();

        {
            let mut lit_idx = 0;
            let mut fact_iters = Vec::new();
            let mut done = false;
            // The comments for this algorithm describe a motion over a 2D "table".
            // For each literal in the body of the current clause, the "table" has a row.
            // Moving right corresponds to using a different fact for the same literal.
            // Moving down corresponds to matching a different literal.
            // Note that reach row is a different length, and is actually an iterator from the DB of
            // the facts for a predicate.
            while !done {
                let lit = &clause.body[lit_idx];
                // If we moved down to a new row, start at the beginning of a row.
                if lit_idx >= fact_iters.len() {
                    fact_iters.push(self.db.facts[lit.predicate].iter());
                } else {
                    // We shouldn't have any record of where we are below the current row.
                    assert!(fact_iters.len() == lit_idx + 1);
                }
                debug!("Try to move right");
                if let Some((fact, _)) = fact_iters[lit_idx].next() {
                    debug!("There was something for us to move right into.");
                    if let Some(binds) = bindings_per_lit[lit_idx].refine(lit, fact) {
                        debug!("And the new cell is valid.");
                        // Try to move down to before the beginning of the next row.
                        if lit_idx + 1 < literals_to_match {
                            debug!("We can move down.");
                            lit_idx += 1;
                            // Remember our constraints from this cell for the next row.
                            bindings_per_lit.push(binds);
                        } else {
                            debug!("We're in the last row!");
                            // Output a new fact, assuming the head has any literals.
                            if let Some(ref head) = clause.head {
                                debug!("Output a fact to the temporary array.");
                                facts_to_add.push(binds.solidify(head));
                            }
                        }
                    } else {
                        debug!("But the cell is not valid. Try again.");
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
                        // Also, forget our constraints from above.
                        bindings_per_lit.pop();
                        lit_idx -= 1;
                    }
                }
            }
        }

        for fact in facts_to_add {
            let mut fact_added = false;
            debug!("Try to add {:?} to the DB", fact);
            self.db.facts[fact.predicate].entry(fact).or_insert_with(|| {
                debug!("It's new!");
                fact_added = true;
                true
            });
            self.fact_added_this_iter |= fact_added;
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
        assert_eq!(ev.db.facts[1].len(), 1);
        assert_eq!(ev.db.facts[1].iter().next().unwrap().0, &Fact::new_from_vec(1, vec![2]));
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
        assert_eq!(ev.db.facts[0].len(), 1);
        assert_eq!(ev.db.facts[0].iter().next().unwrap().0, &Fact::new_from_vec(0, vec![2]));
    }
}
