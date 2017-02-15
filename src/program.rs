use std::collections::hash_map::{HashMap};

use types::{Clause, ClauseIndex, Predicate};

#[derive(Debug)]
pub struct Program {
    pub clauses: Vec<Clause>,
    pub predicate_names: HashMap<Predicate, String>,
    pub clause_variable_names: HashMap<ClauseIndex, Vec<String>>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            clauses: Vec::new(),
            predicate_names: HashMap::new(),
            clause_variable_names: HashMap::new(),
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
