use std::iter;
use std::collections::hash_map;
use std::collections::hash_map::{HashMap};

use types::{Fact, Predicate, Literal};
use program::{Program};
use fact_table::{FactTable};

#[derive(Debug)]
pub struct DB {
    facts: Vec<FactTable<()>>,
    pub program: Program,
}

impl DB {
    pub fn new(program: Program) -> Self {
        DB {
            facts: iter::repeat(FactTable::new()).take(program.num_predicates()).collect(),
            program: program,
        }
    }

    pub fn extend_facts_to_predicate(&mut self, predicate: Predicate) {
        if predicate >= self.facts.len() {
            let difference = 1 + predicate - self.facts.len();
            self.facts.extend(iter::repeat(FactTable::new()).take(difference));
        }
    }
 

    pub fn add_fact(&mut self, fact: Fact) -> bool {
        self.extend_facts_to_predicate(fact.predicate);
        self.facts[fact.predicate].add_fact(fact, ())
    }

    pub fn facts_as_string(&self) -> String {
        use std::fmt::Write;
        let mut output = String::new();
        for fact_table in &self.facts {
            for fact in fact_table.all_facts() {
                writeln!(&mut output, "{}", fact.to_display(&self.program.predicate_names));
            }
        }
        return output;
    }

    pub fn get_iter_for_literal<'a>(&'a self, literal: &Literal) -> hash_map::Iter<'a, Fact, ()> {
        self.facts[literal.predicate].internal_iter()
    }

    #[cfg(test)]
    pub fn get_fact_table(&self) -> FactTable<()> {
        let mut total_table = FactTable::new();
        for table in self.facts.iter().cloned() {
            total_table.merge_new_generation(table);
        }
        return total_table;
    }
}
