use std::iter;
use std::collections::hash_map::{HashMap};

use types::{Fact};
use program::{Program};

#[derive(Debug)]
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
        if fact.predicate >= self.facts.len() {
            let difference = 1 + fact.predicate - self.facts.len();
            self.facts.extend(iter::repeat(HashMap::new()).take(difference));
        }
        self.facts[fact.predicate].insert(fact, true);
    }
}
