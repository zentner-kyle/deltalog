use std::collections::hash_map;
use std::collections::hash_map::{HashMap, Entry};
use std::iter;

use bindings::{Bindings};
use name_table::{NameTable};
use types::{Fact, Predicate, Literal, ClauseIndex};
use truth_value::{TruthValue};

#[derive(Clone, Debug, PartialEq)]
struct FactRecord<T> where T: TruthValue {
    bindings_set: Vec<(ClauseIndex, Bindings<T>)>,
    truth: T,
}

impl<T> FactRecord<T> where T: TruthValue {
    fn new(truth: T, clause_idx: usize, bindings: Bindings<T>) -> Self {
        let mut bind_set = Vec::new();
        bind_set.push((clause_idx, bindings));
        FactRecord {
            bindings_set: bind_set,
            truth: truth,
        }
    }

    fn from_truth(truth: T) -> Self {
        FactRecord {
            bindings_set: Vec::new(),
            truth: truth,
        }
    }

    fn join(&mut self, other: Self) {
        self.truth = T::either(&self.truth, &other.truth);
        self.bindings_set.extend(other.bindings_set);
    }
}

pub struct FactTableIter<'a, T: 'a> where T: TruthValue {
    map_iter: hash_map::Iter<'a, Fact, FactRecord<T>>,
    literal: &'a Literal,
    bindings: Bindings<T>,
}

impl<'a, T> Iterator for FactTableIter<'a, T> where T: TruthValue {
    type Item=(Bindings<T>, &'a Fact, &'a T);
    fn next(&mut self) -> Option<Self::Item> {
        while let Some((fact, record)) = self.map_iter.next() {
            let truth = &record.truth;
            if let Some(bindings) = self.bindings.refine(self.literal, fact, truth) {
                return Some((bindings, fact, truth));
            }
        }
        return None;
    }
}

#[derive(Clone, Debug)]
pub struct FactTable<T> where T: TruthValue {
    maps: Vec<HashMap<Fact, FactRecord<T>>>,
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
            for (fact, record) in table {
                was_new_fact_added |= self.add_record(fact, record);
            }
        }
        return was_new_fact_added;
    }

    pub fn iter<'a, 'b, 'c>(&'a self, literal: &'b Literal, bindings: Bindings<T>) -> FactTableIter<'c, T> where 'a : 'c, 'b : 'c {
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
            for (fact, record) in table.iter() {
                let truth = &record.truth;
                writeln!(&mut output, "{}{}", truth.as_datalog(), fact.to_display(predicate_names)).unwrap();
            }
        }
        return output;
    }

    pub fn all_facts(&self) -> Vec<(Fact, T)> {
        self.maps
            .iter()
            .flat_map(|t| t.iter())
            .map(|(f, r)| (f.clone(), r.truth.clone()))
            .collect()
    }

    // Returns true if the fact was not previously in the table.
    pub fn add_fact(&mut self, fact: Fact, truth: T) -> bool {
        self.extend_num_predicates(fact.predicate);
        let mut map = &mut self.maps[fact.predicate];
        match map.entry(fact) {
            Entry::Occupied(mut pair) => {
                let mut record = pair.get_mut();
                record.truth = T::either(&record.truth, &truth);
                false
            },
            Entry::Vacant(pair) => {
                pair.insert(FactRecord::from_truth(truth));
                true
            }
        }
    }

    // Returns true if the fact was not previously in the table.
    fn add_record(&mut self, fact: Fact, record: FactRecord<T>) -> bool {
        self.extend_num_predicates(fact.predicate);
        let mut map = &mut self.maps[fact.predicate];
        match map.entry(fact) {
            Entry::Occupied(mut pair) => {
                let mut old_record = pair.get_mut();
                old_record.join(record);
                false
            },
            Entry::Vacant(pair) => {
                pair.insert(record);
                true
            }
        }
    }

    pub fn add_match(&mut self, fact: Fact, clause: ClauseIndex, bindings: Bindings<T>) -> bool {
        self.add_record(fact, FactRecord::new(bindings.get_truth(), clause, bindings))
    }

    pub fn get<'a>(&'a mut self, fact: &Fact) -> Option<&'a T> {
        self.extend_num_predicates(fact.predicate);
        self.maps[fact.predicate].get(fact).map(|r| &r.truth)
    }

    pub fn set<'a>(&'a mut self, fact: Fact, truth: T) {
        self.extend_num_predicates(fact.predicate);
        let mut map = &mut self.maps[fact.predicate];
        map.insert(fact, FactRecord::from_truth(truth));
    }

    #[cfg(test)]
    pub fn eq_facts(&self, other: &Self) -> bool {
        let mut facts = self.all_facts();
        let mut other_facts = other.all_facts();
        facts.sort();
        other_facts.sort();
        return facts == other_facts;
    }
}
