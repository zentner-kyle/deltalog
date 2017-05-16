#![allow(dead_code)]

use program::Program;
use rand::Rng;
use std::collections::HashSet;
use truth_value::TruthValue;
use types::{Clause, ClauseIndex, Constant, Literal, LiteralIndex, Predicate, Term, TermIndex,
            Variable};

struct MutationState {
    out_var_supports: Vec<Vec<(LiteralIndex, TermIndex)>>,
}

impl MutationState {
    fn new(clause: &Clause) -> Self {
        let mut out_var_supports = vec![Vec::new(); clause.num_output_variables()];
        for (lit_idx, literal) in clause.body.iter().enumerate() {
            for (term_idx, term) in literal.terms.iter().enumerate() {
                if let Term::Variable(varb) = *term {
                    if clause.head_contains_var(varb) {
                        out_var_supports[varb].push((lit_idx, term_idx));
                    }
                }
            }
        }
        MutationState { out_var_supports }
    }

    fn can_use_head_mut_op(&self, head_op: HeadMutationOp, clause: &Clause) -> bool {
        if let HeadMutationOp::BindVariable(_, varb) = head_op {
            clause.contains_variable_in_body(varb)
        } else {
            true
        }
    }

    fn checked_apply_head_mut_op(&mut self, head_op: HeadMutationOp, clause: &mut Clause) -> bool {
        if self.can_use_head_mut_op(head_op, clause) {
            true
        } else {
            false
        }
    }

    fn apply_head_mut_op(&mut self, head_op: HeadMutationOp, clause: &mut Clause) {
        let head_terms = &mut clause
                                  .head
                                  .as_mut()
                                  .expect("Can only mutate clauses with heads.")
                                  .terms;
        match head_op {
            HeadMutationOp::BindConstant(term_idx, cst) => {
                head_terms[term_idx] = Term::Constant(cst);
            }
            HeadMutationOp::BindVariable(term_idx, varb) => {
                head_terms[term_idx] = Term::Variable(varb);
            }
        }
    }

    fn can_use_body_mut_op(&self, body_op: BodyMutationOp, clause: &Clause) -> bool {
        match body_op {
            BodyMutationOp::Swap(_, _) |
            BodyMutationOp::InsertLiteral(_) => true,
            BodyMutationOp::BindConstant((lit_idx, term_idx), _) |
            BodyMutationOp::BindVariable((lit_idx, term_idx), _) => {
                if let Term::Variable(varb) = clause.body[lit_idx].terms[term_idx] {
                    self.out_var_supports[varb].len() > 1
                } else {
                    true
                }
            }
            BodyMutationOp::RemoveLiteral(lit_idx) => {
                let mut variable_removals = vec![0; clause.num_output_variables()];
                for term in &clause.body[lit_idx].terms {
                    if let Term::Variable(varb) = *term {
                        variable_removals[varb] += 1;
                    }
                }
                for (varb, removals) in variable_removals.iter().enumerate() {
                    if self.out_var_supports[varb].len() <= *removals {
                        return false;
                    }
                }
                return true;
            }
        }
    }

    fn checked_apply_body_mut_op(&mut self, body_op: BodyMutationOp, clause: &mut Clause) -> bool {
        if self.can_use_body_mut_op(body_op, clause) {
            true
        } else {
            false
        }
    }

    fn apply_body_mut_op<R, T>(&mut self,
                               body_op: BodyMutationOp,
                               clause: &mut Clause,
                               rng: &mut R,
                               program: &Program<T>)
        where R: Rng,
              T: TruthValue
    {
        match body_op {
            BodyMutationOp::Swap((lit_a, term_a), (lit_b, term_b)) => {
                let a = clause.body[lit_a].terms[term_a];
                let b = clause.body[lit_b].terms[term_b];
                clause.body[lit_a].terms[term_a] = b;
                clause.body[lit_b].terms[term_b] = a;
            }
            BodyMutationOp::InsertLiteral(pred) => {
                let mut terms = (0..(1 + clause.num_variables()))
                    .map(|i| Term::Variable(i))
                    .collect::<Vec<_>>();
                rng.shuffle(&mut terms);
                terms.truncate(program.get_num_terms(pred));
                clause.body.push(Literal::new_from_vec(pred, terms));
            }
            BodyMutationOp::BindConstant((lit_idx, term_idx), cst) => {
                clause.body[lit_idx].terms[term_idx] = Term::Constant(cst);
            }
            BodyMutationOp::BindVariable((lit_idx, term_idx), varb) => {
                clause.body[lit_idx].terms[term_idx] = Term::Variable(varb);
            }
            BodyMutationOp::RemoveLiteral(lit_idx) => {
                clause.body.remove(lit_idx);
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum BodyMutationOp {
    Swap((LiteralIndex, TermIndex), (LiteralIndex, TermIndex)),
    BindConstant((LiteralIndex, TermIndex), Constant),
    BindVariable((LiteralIndex, TermIndex), Variable),
    InsertLiteral(Predicate),
    RemoveLiteral(LiteralIndex),
}


#[derive(Debug, Copy, Clone)]
enum HeadMutationOp {
    BindConstant(TermIndex, Constant),
    BindVariable(TermIndex, Variable),
}
