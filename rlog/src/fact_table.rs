use types::{Fact, Literal};
use matching::{Bindings};
use std::collections::hash_map;
use std::collections::hash_map::{HashMap, Entry};

pub trait TruthValue : Clone + PartialEq {
    fn join(&self, new: Self);
}

impl TruthValue for () {
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
    map: HashMap<Fact, T>,
}

impl<T> FactTable<T> where T: TruthValue {
    pub fn new() -> Self {
        FactTable {
            map: HashMap::new(),
        }
    }

    pub fn merge_new_generation(&mut self, other: Self) -> bool {
        let mut new_fact = false;
        for (fact, truth) in other.map {
            new_fact |= self.add_fact(fact, truth);
        }
        return new_fact;
    }

    pub fn iter<'a, 'b, 'c>(&'a self, literal: &'b Literal, bindings: Bindings) -> FactTableIter<'c, T> where 'a : 'c, 'b : 'c {
        FactTableIter::<'c, T> {
            map_iter: self.map.iter(),
            literal: literal,
            bindings: bindings, 
        }
    }

    pub fn internal_iter<'a>(&'a self) -> hash_map::Iter<'a, Fact, T> {
        self.map.iter()
    }

    pub fn all_facts<'a>(&'a self) -> hash_map::Keys<'a, Fact, T> {
        self.map.keys()
    }


    // Returns true if the fact was not previously in the table.
    pub fn add_fact(&mut self, fact: Fact, truth: T) -> bool {
        match self.map.entry(fact) {
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
