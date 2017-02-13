use std::iter;
use std::collections::hash_map::{HashMap};

use types::{Fact};
use program::{Program};

pub struct DB {
    pub facts: Vec<HashMap<Fact, bool>>,
    pub program: Program,
}

impl DB {
    pub fn new(program: Program) -> Self {
        DB {
            facts: iter::repeat(HashMap::new()).take(program.num_predicates()).collect(),
            program: program,
        }
    }

    pub fn add_fact(&mut self, fact: Fact) {
        self.facts[fact.predicate].insert(fact, true);
    }
}
