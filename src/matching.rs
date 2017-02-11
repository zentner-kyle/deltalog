use types::{Constant, Term, Literal, Fact};

enum Binding {
    Bound(Constant),
    Unbound,
}

pub struct Match {
    bindings: Vec<Binding>
}

impl Match {
    pub fn refine(mut self, literal: Literal, fact: Fact) -> Option<Match> {
        assert_eq!(literal.predicate, fact.predicate);
        assert_eq!(literal.terms.len(), fact.terms.len());
        for (term, constant) in literal.terms.iter().zip(fact.terms.iter()) {
            match term {
                &Term::Constant(cst) => {
                    if cst != *constant {
                        return None;
                    }
                },
                &Term::Variable(vrb) => {
                    match &mut self.bindings[vrb] {
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
        return Some(self);
    }
}
