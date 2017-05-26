use fact_table::FactTable;
use name_table::NameTable;
use std::collections::hash_map::{Entry, HashMap};
use std::fmt;
use std::slice;
use truth_value::TruthValue;
use types::{Clause, ClauseIndex, Fact, Literal, Predicate};

#[derive(Debug, Clone)]
pub struct Program<T>
    where T: TruthValue
{
    clauses: Vec<Clause>,
    clauses_for_predicate: HashMap<Predicate, Vec<ClauseIndex>>,
    pub clause_weights: Vec<T::Dual>,
    pub predicate_names: NameTable,
    pub clause_variable_names: HashMap<ClauseIndex, NameTable>,
    pub predicate_num_terms: HashMap<Predicate, usize>,
}

pub struct ClauseIter<'a> {
    inner: slice::Iter<'a, Clause>,
}

impl<'a> Iterator for ClauseIter<'a> {
    type Item = &'a Clause;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

impl<T> Program<T>
    where T: TruthValue
{
    pub fn new() -> Self {
        Program {
            clauses: Vec::new(),
            clauses_for_predicate: HashMap::new(),
            clause_weights: Vec::new(),
            predicate_names: NameTable::new(),
            clause_variable_names: HashMap::new(),
            predicate_num_terms: HashMap::new(),
        }
    }

    pub fn clause_weight(&self, clause_idx: ClauseIndex) -> &T::Dual {
        &self.clause_weights[clause_idx]
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

    #[allow(dead_code)]
    pub fn clauses_for_predicate(&self, predicate: Predicate) -> Option<&[ClauseIndex]> {
        self.clauses_for_predicate
            .get(&predicate)
            .map(|v| v.as_ref())
    }

    pub fn num_clauses(&self) -> usize {
        self.clauses.len()
    }

    pub fn get_clause_by_idx(&self, clause_idx: ClauseIndex) -> &Clause {
        &self.clauses[clause_idx]
    }

    pub fn clause_iter<'a>(&'a self) -> ClauseIter<'a> {
        ClauseIter { inner: self.clauses.iter() }
    }

    pub fn check_num_fact_terms(&mut self, facts: &FactTable<T>) -> Result<(), &'static str> {
        for (fact, _) in facts.all_facts_iter() {
            self.check_num_single_fact_terms(fact)?;
        }
        return Ok(());
    }

    pub fn check_num_single_fact_terms(&mut self, fact: &Fact) -> Result<(), &'static str> {
        let predicate = fact.predicate;
        let num_terms = fact.terms.len();
        match self.predicate_num_terms.entry(predicate) {
            Entry::Occupied(pair) => {
                if num_terms != *pair.get() {
                    return Err("Wrong number of terms in predicate.");
                }
            }
            Entry::Vacant(pair) => {
                pair.insert(num_terms);
            }
        }
        return Ok(());
    }

    // TODO(zentner): Rename.
    pub fn check_num_terms(&mut self, literal: &Literal) -> Result<(), &'static str> {
        let num_terms = literal.terms.len();
        match self.predicate_num_terms.entry(literal.predicate) {
            Entry::Occupied(pair) => {
                if num_terms != *pair.get() {
                    return Err("Wrong number of terms in predicate.");
                }
            }
            Entry::Vacant(pair) => {
                pair.insert(num_terms);
            }
        }
        return Ok(());
    }

    #[allow(dead_code)]
    pub fn num_terms(&self, predicate: Predicate) -> Option<usize> {
        self.predicate_num_terms.get(&predicate).cloned()
    }

    pub fn get_num_terms(&self, predicate: Predicate) -> usize {
        *self.predicate_num_terms.get(&predicate).unwrap()
    }

    pub fn push_clause_simple(&mut self, clause: Clause) {
        self.push_clause(clause, T::dual_default(), None)
            .unwrap();
    }

    pub fn push_clause(&mut self,
                       clause: Clause,
                       weight: T::Dual,
                       var_names: Option<NameTable>)
                       -> Result<(), &'static str> {
        let clause_idx = self.clauses.len();
        self.check_num_terms(&clause.head)?;
        match self.clauses_for_predicate.entry(clause.head.predicate) {
            Entry::Occupied(mut pair) => {
                pair.get_mut().push(clause_idx);
            }
            Entry::Vacant(pair) => {
                pair.insert(vec![clause_idx]);
            }
        }
        for literal in &clause.body {
            self.check_num_terms(literal)?;
        }
        self.clauses.push(clause);
        self.clause_weights.push(weight);
        if let Some(var_names) = var_names {
            self.clause_variable_names.insert(clause_idx, var_names);
        }
        return Ok(());
    }

    #[allow(dead_code)]
    pub fn mean_weight(&self) -> T::Dual {
        let num_weights = self.clause_weights.len();
        let amount_per_weight = 1f64 / num_weights as f64;
        let mut mean_weight = T::dual_zero();
        for weight in self.clause_weights.iter() {
            T::dual_adjust(&mut mean_weight, weight, amount_per_weight);
        }
        mean_weight
    }

    #[allow(dead_code)]
    pub fn clear_clauses(&mut self) {
        self.clauses.clear();
        self.clauses_for_predicate.clear();
        self.clause_weights.clear();
        self.predicate_num_terms.clear();
    }
}

impl<T> fmt::Display for Program<T>
    where T: TruthValue
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for (i, clause) in self.clauses.iter().enumerate() {
            let weight = self.clause_weights
                .get(i)
                .cloned()
                .unwrap_or_else(|| T::dual_default());
            write!(f, "{}", T::dual_as_datalog(&weight))?;
            clause
                .format(f, self.clause_variable_names.get(&i), &self.predicate_names)?;
            write!(f, "\n")?;
        }
        return Ok(());
    }
}
