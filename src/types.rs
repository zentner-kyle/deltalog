pub type Predicate = usize;
pub type Constant = usize;
pub type Variable = usize;
pub type ClauseIndex = usize;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Term {
    Constant(Constant),
    Variable(Variable),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Literal {
    pub predicate: Predicate,
    pub terms: Vec<Term>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Clause {
    pub head: Option<Literal>,
    pub body: Vec<Literal>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Fact {
    pub predicate: Predicate,
    pub terms: Vec<Constant>,
}

impl Clause {
    pub fn new_from_vec(head: Literal, body: Vec<Literal>) -> Self {
        Clause {
            head: Some(head),
            body: body,
        }
    }

    pub fn is_valid(&self) -> bool {
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
}


impl Fact {
    pub fn new_from_vec(predicate: Predicate, terms: Vec<Constant>) -> Self {
        Fact {
            predicate: predicate,
            terms: terms,
        }
    }
}
