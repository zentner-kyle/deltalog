use std::iter;
use std::collections::hash_map::{HashMap};

use types::{Fact, Predicate};
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

    pub fn extend_facts_to_predicate(&mut self, predicate: Predicate) {
        if predicate >= self.facts.len() {
            let difference = 1 + predicate - self.facts.len();
            self.facts.extend(iter::repeat(HashMap::new()).take(difference));
        }
    }
 

    pub fn add_fact(&mut self, fact: Fact) {
        self.extend_facts_to_predicate(fact.predicate);
        self.facts[fact.predicate].insert(fact, true);
    }

    pub fn facts_as_string(&self) -> String {
        use std::fmt::Write;
        let mut output = String::new();
        for fact_table in &self.facts {
            for (ref fact, _) in fact_table {
                writeln!(&mut output, "{}", fact.to_display(&self.program.predicate_names));
            }
        }
        return output;
    }
}
