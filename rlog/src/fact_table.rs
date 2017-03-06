use std::collections::hash_map;
use std::collections::hash_map::{HashMap, Entry};
use std::iter;

use bindings::{Bindings};
use name_table::{NameTable};
use types::{Fact, Predicate, Literal};

pub trait TruthValue : Clone + PartialEq {
    fn default() -> Self;
    fn join(&self, new: Self);
}

impl TruthValue for () {
    fn default() -> Self {}
    fn join(&self, _: Self) {}
}

pub struct FactTableIter<'a, T: 'a> {
    map_iter: hash_map::Iter<'a, Fact, T>,
    literal: &'a Literal,
    bindings: Bindings,
}

impl<'a, T> Iterator for FactTableIter<'a, T> {
    type Item=(Bindings, &'a Fact, &'a T);
    fn next(&mut self) -> Option<Self::Item> {
        while let Some((fact, truth)) = self.map_iter.next() {
            if let Some(bindings) = self.bindings.refine(self.literal, fact) {
                return Some((bindings, fact, truth));
            }
        }
        return None;
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FactTable<T> where T: TruthValue {
    maps: Vec<HashMap<Fact, T>>,
}

impl<T> FactTable<T> where T: TruthValue {
    pub fn new() -> Self {
        FactTable {
            maps: Vec::new(),
        }
    }

    pub fn extend_num_predicates(&mut self, predicate: Predicate) {
        if predicate >= self.maps.len() {
            let difference = 1 + predicate - self.maps.len();
            self.maps.extend(iter::repeat(HashMap::new()).take(difference));
        }
    }

    pub fn merge_new_generation(&mut self, other: Self) -> bool {
        let mut was_new_fact_added = false;
        for table in other.maps {
            for (fact, truth) in table {
                was_new_fact_added |= self.add_fact(fact, truth);
            }
        }
        return was_new_fact_added;
    }

    pub fn iter<'a, 'b, 'c>(&'a self, literal: &'b Literal, bindings: Bindings) -> FactTableIter<'c, T> where 'a : 'c, 'b : 'c {
        FactTableIter::<'c, T> {
            map_iter: self.maps[literal.predicate].iter(),
            literal: literal,
            bindings: bindings, 
        }
    }

    pub fn as_datalog(&self, predicate_names: &NameTable) -> String {
        use std::fmt::Write;
        let mut output = String::new();
        for table in &self.maps {
            for fact in table.keys() {
                writeln!(&mut output, "{}", fact.to_display(predicate_names)).unwrap();
            }
        }
        return output;
    }

    #[allow(dead_code)]
    pub fn all_facts(&self) -> Vec<Fact> {
        self.maps.iter().flat_map(|t| t.iter()).map(|(f, _)| f).cloned().collect()
    }

    // Returns true if the fact was not previously in the table.
    pub fn add_fact(&mut self, fact: Fact, truth: T) -> bool {
        self.extend_num_predicates(fact.predicate);
        let mut map = &mut self.maps[fact.predicate];
        match map.entry(fact) {
            Entry::Occupied(mut pair) => {
                pair.get_mut().join(truth);
                false
            },
            Entry::Vacant(pair) => {
                pair.insert(truth);
                true
            }
        }
    }
}
