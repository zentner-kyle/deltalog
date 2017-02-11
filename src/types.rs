pub type Predicate = usize;
pub type Constant = usize;
pub type Variable = usize;

#[derive(PartialEq, Eq)]
pub enum Term {
    Constant(Constant),
    Variable(Variable),
}

#[derive(PartialEq, Eq)]
pub struct Literal {
    pub predicate: Predicate,
    pub terms: Vec<Term>,
}

#[derive(PartialEq, Eq)]
pub struct Clause {
    pub head: Option<Literal>,
    pub body: Vec<Literal>,
}

#[derive(PartialEq, Eq)]
pub struct Fact {
    pub predicate: Predicate,
    pub terms: Vec<Constant>,
}

impl Clause {
    pub fn is_valid(&self) -> bool {
        if let Some(ref head) = self.head {
            for lit in self.body.iter() {
                for term in lit.terms.iter() {
                    if let &Term::Variable(_) = term {
                        let mut found_in_head = false;
                        for h_term in head.terms.iter() {
                            if term == h_term {
                                found_in_head = true;
                                break;
                            }
                        }
                        if !found_in_head {
                            return false;
                        }
                    }
                }
            }
        }
        for vrb in 0..self.num_variables() {
            if !self.has_variable(vrb) {
                return false;
            }
        }
        return true;
    }

    pub fn num_variables(&self) -> usize {
        let mut count = 0;
        if let Some(ref head) = self.head {
            for term in head.terms.iter() {
                if let &Term::Variable(vrb) = term {
                    if vrb + 1 > count {
                        count = vrb + 1;
                    }
                }
            }
        }
        return count;
    }

    pub fn has_variable(&self, variable: Variable) -> bool {
        if let Some(ref head) = self.head {
            for term in head.terms.iter() {
                if let &Term::Variable(vrb) = term {
                    if vrb == variable {
                        return true;
                    }
                }
            }
        }
        return false;
    }
}
