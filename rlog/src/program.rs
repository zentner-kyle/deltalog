use std::collections::hash_map::{HashMap, Entry};
use std::fmt;

use types::{Clause, ClauseIndex, Predicate, Literal};
use name_table::{NameTable};
use truth_value::{TruthValue};

#[derive(Debug)]
pub struct Program<T> where T: TruthValue {
    pub clauses: Vec<Clause>,
    pub clause_weights: Vec<T::Dual>,
    pub predicate_names: NameTable,
    pub clause_variable_names: HashMap<ClauseIndex, NameTable>,
    pub predicate_num_terms: HashMap<Predicate, usize>,
}

impl<T> Program<T> where T: TruthValue {
    pub fn new() -> Self {
        Program {
            clauses: Vec::new(),
            clause_weights: Vec::new(),
            predicate_names: NameTable::new(),
            clause_variable_names: HashMap::new(),
            predicate_num_terms: HashMap::new(),
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

    pub fn check_num_terms(&mut self, literal: &Literal) -> Result<(), &'static str> {
        let num_terms = literal.terms.len();
        match self.predicate_num_terms.entry(literal.predicate) {
            Entry::Occupied(mut pair) => {
                if num_terms != *pair.get() {
                    return Err("Wrong number of terms in predicate.");
                }
            },
            Entry::Vacant(pair) => {
                pair.insert(num_terms);
            }
        }
        return Ok(());
    }

    pub fn push_clause(&mut self, clause: Clause, weight: T::Dual) -> Result<(), &'static str> {
        if let Some(ref head) = clause.head {
            self.check_num_terms(head)?;
        }
        for literal in &clause.body {
            self.check_num_terms(literal)?;
        }
        self.clauses.push(clause);
        self.clause_weights.push(weight);
        return Ok(());
    }
}

impl<T> fmt::Display for Program<T> where T: TruthValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for (i, clause) in self.clauses.iter().enumerate() {
            let weight = self.clause_weights.get(i).unwrap();
            write!(f, "{}", T::dual_as_datalog(weight))?;
            clause.format(f, self.clause_variable_names.get(&i).unwrap(), &self.predicate_names)?;
            write!(f, "\n")?;
        }
        return Ok(());
    }
}
