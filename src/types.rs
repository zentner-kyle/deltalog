use name_table::NameTable;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;
use util::hash_map_get_or_insert_len;

pub type Predicate = usize;
pub type Constant = usize;
pub type Variable = usize;
pub type LiteralIndex = usize;
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
    pub head: Literal,
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
            head: head,
            body: body,
        }
    }

    #[allow(dead_code)]
    pub fn num_times_head_uses_var(&self, variable: Variable) -> usize {
        let mut count = 0;
        for term in &self.head.terms {
            if &Term::Variable(variable) == term {
                count += 1;
            }
        }
        return count;
    }

    pub fn head_contains_var(&self, variable: Variable) -> bool {
        for term in &self.head.terms {
            if Term::Variable(variable) == *term {
                return true;
            }
        }
        return false;
    }

    pub fn is_valid(&self) -> bool {
        if self.body.len() == 0 {
            return false;
        }
        for term in &self.head.terms {
            if let &Term::Variable(var) = term {
                if !self.contains_variable_in_body(var) {
                    return false;
                }
            }
        }
        return true;
    }

    #[allow(dead_code)]
    pub fn max_output_variable(&self) -> usize {
        let mut max = 0;
        for term in &self.head.terms {
            if let &Term::Variable(var) = term {
                if var > max {
                    max = var;
                }
            }
        }
        return max;
    }

    pub fn num_output_variables(&self) -> usize {
        let mut count = 0;
        for term in &self.head.terms {
            if let &Term::Variable(var) = term {
                if var + 1 > count {
                    count = var + 1;
                }
            }
        }
        return count;
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
        if self.head.contains_variable(variable) {
            return true;
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

    pub fn max_constant(&self) -> Constant {
        let mut max = 0;
        for term in &self.head.terms {
            if let Term::Constant(cst) = *term {
                if cst > max {
                    max = cst;
                }
            }
        }
        for lit in &self.body {
            for term in &lit.terms {
                if let Term::Constant(cst) = *term {
                    if cst > max {
                        max = cst;
                    }
                }
            }
        }
        return max;
    }

    pub fn max_predicate(&self) -> Predicate {
        let mut max = 0;
        if self.head.predicate > max {
            max = self.head.predicate;
        }
        for lit in &self.body {
            if lit.predicate > max {
                max = lit.predicate;
            }
        }
        return max;
    }

    pub fn canonicalize_in_place(&mut self) {
        let mut var_map = HashMap::new();
        for term in &mut self.head.terms {
            if let Term::Variable(ref mut varb) = *term {
                *varb = hash_map_get_or_insert_len(&mut var_map, *varb);
            }
        }
        self.body.sort_by_key(|lit| lit.predicate);
        for lit in self.body.iter_mut() {
            for term in lit.terms.iter_mut() {
                if let Term::Variable(ref mut varb) = *term {
                    *varb = hash_map_get_or_insert_len(&mut var_map, *varb);
                }
            }
        }
    }

    pub fn format(&self,
                  f: &mut fmt::Formatter,
                  var_names: Option<&NameTable>,
                  pred_names: &NameTable)
                  -> Result<(), fmt::Error> {
        self.head.format(f, var_names, pred_names)?;
        write!(f, " :- ")?;
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

    #[allow(dead_code)]
    pub fn num_times_variable_appears(&self, variable: Variable) -> usize {
        self.terms
            .iter()
            .map(|term| match term {
                     &Term::Variable(var) if var == variable => 1,
                     _ => 0,
                 })
            .sum()
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
                }
                Term::Variable(_) => {
                    panic!("Literal::to_fact() called with Literal containing variables.");
                }
            }
        }
        Fact::new_from_vec(self.predicate, terms)
    }

    pub fn format(&self,
                  f: &mut fmt::Formatter,
                  var_names: Option<&NameTable>,
                  pred_names: &NameTable)
                  -> Result<(), fmt::Error> {
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
            name: pred_names
                .get_name(self.predicate)
                .map(|n| Cow::Borrowed(n))
                .unwrap_or_else(|| Cow::Owned(format!("Predicate#{}", self.predicate))),
        }
    }
}

pub struct FactDisplayer<'a, 'b> {
    fact: &'a Fact,
    name: Cow<'b, str>,
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
    pub fn format(&self,
                  f: &mut fmt::Formatter,
                  var_names: Option<&NameTable>)
                  -> Result<(), fmt::Error> {
        match self {
            &Term::Constant(cst) => {
                write!(f, "{}", cst)?;
            }
            &Term::Variable(var) => {
                if let Some(name) = var_names.and_then(|names| names.get_name(var)) {
                    write!(f, "{}", name)?;
                } else {
                    write!(f, "Var#{}", var)?;
                }
            }
        }
        return Ok(());
    }
}
