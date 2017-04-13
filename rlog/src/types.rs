use name_table::{NameTable};

use std::fmt;

pub type Predicate = usize;
pub type Constant = usize;
pub type Variable = usize;
pub type ClauseIndex = usize;
pub type TermIndex = usize;

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Copy)]
pub enum Term {
    Constant(Constant),
    Variable(Variable),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Literal {
    pub predicate: Predicate,
    pub terms: Vec<Term>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Clause {
    pub head: Option<Literal>,
    pub body: Vec<Literal>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Fact {
    pub predicate: Predicate,
    pub terms: Vec<Constant>,
}

impl Clause {
    #[allow(dead_code)]
    pub fn new_from_vec(head: Literal, body: Vec<Literal>) -> Self {
        Clause {
            head: Some(head),
            body: body,
        }
    }

    pub fn is_valid(&self) -> bool {
        // TODO(zentner): num_variables() assumes validity!
        for var in 0..self.num_variables() {
            if !self.contains_variable_in_body(var) {
                return false;
            }
        }
        return true;
    }

    pub fn num_variables(&self) -> usize {
        let mut count = 0;
        for lit in &self.body {
            for term in &lit.terms {
                if let &Term::Variable(var) = term {
                    if var + 1 > count {
                        count = var + 1;
                    }
                }
            }
        }
        return count;
    }

    #[allow(dead_code)]
    pub fn contains_variable(&self, variable: Variable) -> bool {
        if let Some(ref head) = self.head {
            if head.contains_variable(variable) {
                return true;
            }
        }
        if self.contains_variable_in_body(variable) {
            return true;
        }
        return false;
    }

    pub fn contains_variable_in_body(&self, variable: Variable) -> bool {
        for lit in &self.body {
            if lit.contains_variable(variable) {
                return true;
            }
        }
        return false;
    }

    pub fn max_predicate(&self) -> Predicate {
        let mut max = 0;
        if let Some(ref head) = self.head {
            if head.predicate > max {
                max = head.predicate;
            }
        }
        for term in self.body.iter() {
            if term.predicate > max {
                max = term.predicate;
            }
        }
        return max;
    }

    pub fn format(&self, f: &mut fmt::Formatter, var_names: &NameTable, pred_names: &NameTable) -> Result<(), fmt::Error> {
        if let Some(ref head) = self.head {
            head.format(f, var_names, pred_names)?;
            write!(f, " :- ")?;
        } else {
            write!(f, "? :- ")?;
        }
        for (i, literal) in self.body.iter().enumerate() {
            literal.format(f, var_names, pred_names)?;
            if i + 1 != self.body.len() {
                write!(f, ", ")?;
            }
        }
        return Ok(());
    }
}

impl Literal {
    pub fn new_from_vec(predicate: Predicate, terms: Vec<Term>) -> Self {
        Literal {
            predicate: predicate,
            terms: terms,
        }
    }

    pub fn contains_variable(&self, variable: Variable) -> bool {
        for term in &self.terms {
            if let &Term::Variable(var) = term {
                if var == variable {
                    return true;
                }
            }
        }
        return false;
    }

    pub fn to_fact(self) -> Fact {
        let mut terms = Vec::new();
        for term in self.terms {
            match term {
                Term::Constant(cst) => {
                    terms.push(cst);
                },
                Term::Variable(_) => {
                    panic!("Literal::to_fact() called with Literal containing variables.");
                }
            }
        }
        Fact::new_from_vec(self.predicate, terms)
    }

    pub fn format(&self, f: &mut fmt::Formatter, var_names: &NameTable, pred_names: &NameTable) -> Result<(), fmt::Error> {
        if let Some(name) = pred_names.get_name(self.predicate) {
            write!(f, "{}(", name)?;
        } else {
            write!(f, "predicate#{}(", self.predicate)?;
        }
        for (i, term) in self.terms.iter().enumerate() {
            term.format(f, var_names)?;
            if i + 1 != self.terms.len() {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")?;
        return Ok(());
    }
}


impl Fact {
    pub fn new_from_vec(predicate: Predicate, terms: Vec<Constant>) -> Self {
        Fact {
            predicate: predicate,
            terms: terms,
        }
    }

    pub fn to_display<'a, 'b>(&'a self, pred_names: &'b NameTable) -> FactDisplayer<'a, 'b> {
        FactDisplayer {
            fact: self,
            name: pred_names.get_name(self.predicate).unwrap(),
        }
    }
}

pub struct FactDisplayer<'a, 'b> {
    fact: &'a Fact,
    name: &'b str,
}

impl<'a, 'b> fmt::Display for FactDisplayer<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}(", self.name)?;
        let num_terms = self.fact.terms.len();
        for (i, term) in self.fact.terms.iter().enumerate() {
            if i + 1 == num_terms {
                write!(f, "{}", term)?;
            } else {
                write!(f, "{}, ", term)?;
            }
        }
        write!(f, ").")?;
        return Ok(());
    }
}

impl Term {
    pub fn format(&self, f: &mut fmt::Formatter, var_names: &NameTable) -> Result<(), fmt::Error> {
        match self {
            &Term::Constant(cst) => {
                write!(f, "{}", cst)?;
            },
            &Term::Variable(var) => {
                if let Some(name) = var_names.get_name(var) {
                    write!(f, "{}", name)?;
                } else {
                    write!(f, "Var#{}", var);
                }
            }
        }
        return Ok(());
    }
}
