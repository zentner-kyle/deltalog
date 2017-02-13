use std::collections::{HashMap};

use types::{Clause};

pub struct Program {
    pub clauses: Vec<Clause>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            clauses: Vec::new()
        }
    }

    pub fn num_predicates(&self) -> usize {
        let mut count = 0;
        for clause in self.clauses.iter() {
            let max = clause.max_predicate();
            if max + 1 > count {
                count = max + 1;
            }
        }
        return count;
    }
}
