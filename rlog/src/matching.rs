use std::iter;

use types::{Constant, Term, Literal, Fact};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Binding {
    Bound(Constant),
    Unbound,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Bindings {
    bindings: Vec<Binding>
}

impl Bindings {
    pub fn new(num_vars: usize) -> Self {
        Bindings {
            bindings: iter::repeat(Binding::Unbound).take(num_vars).collect(),
        }
    }

    pub fn refine(&self, literal: &Literal, fact: &Fact) -> Option<Bindings> {
        assert_eq!(literal.predicate, fact.predicate);
        assert_eq!(literal.terms.len(), fact.terms.len());
        let mut next_binds = self.clone();
        for (term, constant) in literal.terms.iter().zip(fact.terms.iter()) {
            match term {
                &Term::Constant(cst) => {
                    if cst != *constant {
                        return None;
                    }
                },
                &Term::Variable(vrb) => {
                    match &mut next_binds.bindings[vrb] {
                        &mut Binding::Bound(cst) => {
                            if cst != *constant {
                                return None;
                            }
                        },
                        binding@&mut Binding::Unbound => {
                            *binding = Binding::Bound(constant.clone());
                        }
                    }
                }
            }
        }
        return Some(next_binds);
    }

    pub fn all_variables_in_literal_bound(&self, literal: &Literal) -> bool {
        for term in literal.terms.iter() {
            if let &Term::Variable(var_idx) = term {
                if let Binding::Unbound = self.bindings[var_idx] {
                    return false;
                }
            }
        }
        return true;
    }

    pub fn solidify(&self, literal: &Literal) -> Fact {
        assert!(self.all_variables_in_literal_bound(literal));
        let mut constants = Vec::with_capacity(literal.terms.len());
        for term in literal.terms.iter() {
            match term {
                &Term::Variable(var_idx) => {
                    if let Binding::Bound(cst) = self.bindings[var_idx] {
                        constants.push(cst);
                    } else {
                        unreachable!("Variable was not bound in when trying to solidify literal.");
                    }
                },
                &Term::Constant(cst) => {
                    constants.push(cst);
                }
            }
        }
        Fact::new_from_vec(literal.predicate, constants)
    }
}

#[cfg(test)]
mod tests {
    use super::{Bindings};
    use types::{Constant, Term, Literal, Fact};

    #[test]
    fn match_no_variables() {
        let binds = Bindings::new(0);
        let lit = Literal::new_from_vec(0, vec![Term::Constant(0), Term::Constant(1), Term::Constant(2)]);
        let fact = Fact::new_from_vec(0, vec![0, 1, 2]);
        assert!(binds.refine(&lit, &fact).is_some());
    }

    #[test]
    fn match_one_variable_twice() {
        let binds = Bindings::new(1);
        let lit = Literal::new_from_vec(0, vec![Term::Variable(0), Term::Variable(0)]);
        let fact = Fact::new_from_vec(0, vec![1, 1]);
        assert!(binds.refine(&lit, &fact).is_some());
    }

    #[test]
    fn dont_match_one_variable_twice() {
        let binds = Bindings::new(1);
        let lit = Literal::new_from_vec(0, vec![Term::Variable(0), Term::Variable(0)]);
        let fact = Fact::new_from_vec(0, vec![1, 2]);
        assert!(binds.refine(&lit, &fact).is_none());
    }

    #[test]
    fn bind_two_variables() {
        let binds = Bindings::new(2);
        let lit = Literal::new_from_vec(0, vec![Term::Variable(0), Term::Variable(1)]);
        let fact = Fact::new_from_vec(0, vec![2, 3]);
        let binds = binds.refine(&lit, &fact).unwrap();

        let lit2 = Literal::new_from_vec(1, vec![Term::Variable(1), Term::Variable(0)]);
        let good_lit2_fact = Fact::new_from_vec(1, vec![3, 2]);
        assert!(binds.clone().refine(&lit2, &good_lit2_fact).is_some());
        let bad_lit2_fact = Fact::new_from_vec(1, vec![2, 3]);
        assert!(binds.clone().refine(&lit2, &bad_lit2_fact).is_none());
    }

    #[test]
    fn solidify_one_variable() {
        let binds = Bindings::new(1);
        let lit = Literal::new_from_vec(0, vec![Term::Variable(0), Term::Variable(0)]);
        let fact = Fact::new_from_vec(0, vec![1, 1]);
        let binds = binds.refine(&lit, &fact).unwrap();

        let lit2 = Literal::new_from_vec(1, vec![Term::Variable(0), Term::Constant(2)]);
        assert!(binds.all_variables_in_literal_bound(&lit2));
        let fact2 = binds.solidify(&lit2);
        assert_eq!(fact2.terms[0], 1);
        assert_eq!(fact2.terms[1], 2);
        assert_eq!(fact2.terms.len(), 2);
    }
}
