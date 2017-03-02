use std::iter;
use std::collections::hash_map;
use std::collections::hash_map::{HashMap};

use fact_table::{FactTable, FactTableIter};
use matching::{Bindings};
use program::{Program};
use types::{Fact, Predicate, Literal};

#[derive(Debug)]
pub struct DB {
    facts: FactTable<()>,
    pub program: Program,
}

impl DB {
    pub fn new(facts: FactTable<()>, program: Program) -> Self {
        DB {
            facts: facts,
            program: program,
        }
    }

    pub fn extend_facts_to_predicate(&mut self, predicate: Predicate) {
        self.facts.extend_num_predicates(predicate);
    }

    pub fn add_fact(&mut self, fact: Fact) -> bool {
        self.facts.add_fact(fact, ())
    }

    pub fn add_new_facts(&mut self, fact_table: FactTable<()>) -> bool {
        self.facts.merge_new_generation(fact_table)
    }

    pub fn facts_as_string(&self) -> String {
        return self.facts.as_datalog(&self.program.predicate_names);
    }

    pub fn get_iter_for_literal<'a, 'b, 'c>(&'a self, literal: &'b Literal, bindings: Bindings) -> FactTableIter<'c, ()> where 'a : 'c, 'b : 'c {
        self.facts.iter(literal, bindings)
    }

    #[cfg(test)]
    pub fn get_fact_table(&self) -> FactTable<()> {
        return self.facts.clone();
    }
}
