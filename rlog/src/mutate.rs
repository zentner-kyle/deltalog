#![allow(dead_code)]

use fact_table::FactTable;
use program::Program;
use rand::Rng;
use reconstrain::ConstraintMeasure;
use std::collections::{HashMap, HashSet};
use truth_value::TruthValue;
use types::{Clause, ClauseIndex, Constant, Literal, LiteralIndex, Predicate, Term, TermIndex,
            Variable};

struct MutationState {
    out_var_supports: Vec<Vec<(LiteralIndex, TermIndex)>>,
}

type GenResult<T> = Result<T, &'static str>;

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
                let num_terms = terms.len();
                if num_terms == 0 {
                    return;
                }
                rng.shuffle(&mut terms);
                terms.truncate(program
                                   .num_terms(pred)
                                   .unwrap_or(rng.gen_range(1, 1 + num_terms)));
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum BodyMutationOp {
    Swap((LiteralIndex, TermIndex), (LiteralIndex, TermIndex)),
    BindConstant((LiteralIndex, TermIndex), Constant),
    BindVariable((LiteralIndex, TermIndex), Variable),
    InsertLiteral(Predicate),
    RemoveLiteral(LiteralIndex),
}


#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum HeadMutationOp {
    BindConstant(TermIndex, Constant),
    BindVariable(TermIndex, Variable),
}

#[derive(Debug, Clone)]
struct MutationOpGenerator {
    num_body_ops: i64,
    num_head_ops: i64,
    body_swap_prob: f64,
    body_bind_constant_prob: f64,
    body_bind_variable_prob: f64,
    body_insert_literal_prob: f64,
    body_remove_literal_prob: f64,
    head_bind_constant_prob: f64,
    head_bind_variable_prob: f64,
}

trait Selector {
    fn choose_clause<R, T>(&mut self, rng: &mut R, program: &Program<T>) -> GenResult<ClauseIndex>
        where R: Rng,
              T: TruthValue;

    fn choose_predicate<R, T>(&mut self,
                              rng: &mut R,
                              program: &Program<T>,
                              clause: ClauseIndex)
                              -> GenResult<Predicate>
        where R: Rng,
              T: TruthValue;

    fn choose_literal<R, T>(&mut self,
                            rng: &mut R,
                            program: &Program<T>,
                            clause: ClauseIndex)
                            -> GenResult<LiteralIndex>
        where R: Rng,
              T: TruthValue;

    fn choose_literal_to_remove<R, T>(&mut self,
                                      rng: &mut R,
                                      program: &Program<T>,
                                      clause: ClauseIndex)
                                      -> GenResult<LiteralIndex>
        where R: Rng,
              T: TruthValue
    {
        self.choose_literal(rng, program, clause)
    }

    fn choose_term<R, T>(&mut self,
                         rng: &mut R,
                         program: &Program<T>,
                         clause: ClauseIndex,
                         literal: LiteralIndex)
                         -> GenResult<TermIndex>
        where R: Rng,
              T: TruthValue;

    fn choose_variable<R, T>(&mut self,
                             rng: &mut R,
                             program: &Program<T>,
                             clause: ClauseIndex,
                             literal: LiteralIndex,
                             term: TermIndex)
                             -> GenResult<Variable>
        where R: Rng,
              T: TruthValue;

    fn choose_constant<R, T>(&mut self,
                             rng: &mut R,
                             program: &Program<T>,
                             clause: ClauseIndex,
                             literal: LiteralIndex,
                             term: TermIndex)
                             -> GenResult<Constant>
        where R: Rng,
              T: TruthValue;

    fn choose_head_term<R, T>(&mut self,
                              rng: &mut R,
                              program: &Program<T>,
                              clause: ClauseIndex)
                              -> GenResult<TermIndex>
        where R: Rng,
              T: TruthValue;

    fn choose_head_variable<R, T>(&mut self,
                                  rng: &mut R,
                                  program: &Program<T>,
                                  clause: ClauseIndex,
                                  term: TermIndex)
                                  -> GenResult<Variable>
        where R: Rng,
              T: TruthValue;

    fn choose_head_constant<R, T>(&mut self,
                                  rng: &mut R,
                                  program: &Program<T>,
                                  clause: ClauseIndex,
                                  term: TermIndex)
                                  -> GenResult<Constant>
        where R: Rng,
              T: TruthValue;
}

struct UniformPlusKSelector {
    k: i64,
    max_predicate: Predicate,
    max_constant: HashMap<(Predicate, TermIndex), Constant>,
}

impl UniformPlusKSelector {
    fn new<T>(k: i64, program: &Program<T>, facts: &FactTable<T>) -> Self
        where T: TruthValue
    {
        UniformPlusKSelector {
            k,
            max_predicate: program.num_predicates(),
            max_constant: facts.max_constant_table(),
        }
    }

    fn gen_range<R>(&self, rng: &mut R, end: usize) -> GenResult<usize>
        where R: Rng
    {
        if end == 0 {
            Err("Nothing to choose from.")
        } else {
            Ok(rng.gen_range(0, end))
        }
    }

    fn gen_range_plus_k<R>(&self, rng: &mut R, base: usize) -> GenResult<usize>
        where R: Rng
    {
        let end = base as i64 + self.k;
        if end <= 0 {
            Err("Nothing to choose from.")
        } else {
            Ok(rng.gen_range(0, end) as usize)
        }
    }
}

impl Selector for UniformPlusKSelector {
    fn choose_predicate<R, T>(&mut self,
                              rng: &mut R,
                              program: &Program<T>,
                              clause: ClauseIndex)
                              -> GenResult<Predicate>
        where R: Rng,
              T: TruthValue
    {
        self.gen_range_plus_k(rng, program.num_predicates())
    }

    fn choose_clause<R, T>(&mut self, rng: &mut R, program: &Program<T>) -> GenResult<ClauseIndex>
        where R: Rng,
              T: TruthValue
    {
        self.gen_range(rng, program.num_clauses())
    }

    fn choose_literal<R, T>(&mut self,
                            rng: &mut R,
                            program: &Program<T>,
                            clause: ClauseIndex)
                            -> GenResult<LiteralIndex>
        where R: Rng,
              T: TruthValue
    {
        self.gen_range(rng, program.get_clause_by_idx(clause).body.len())
    }

    fn choose_term<R, T>(&mut self,
                         rng: &mut R,
                         program: &Program<T>,
                         clause: ClauseIndex,
                         literal: LiteralIndex)
                         -> GenResult<TermIndex>
        where R: Rng,
              T: TruthValue
    {
        self.gen_range(rng,
                       program.get_clause_by_idx(clause).body[literal]
                           .terms
                           .len())
    }

    fn choose_variable<R, T>(&mut self,
                             rng: &mut R,
                             program: &Program<T>,
                             clause: ClauseIndex,
                             literal: LiteralIndex,
                             term: TermIndex)
                             -> GenResult<Variable>
        where R: Rng,
              T: TruthValue
    {
        self.gen_range_plus_k(rng, program.get_clause_by_idx(clause).num_variables())
    }


    fn choose_constant<R, T>(&mut self,
                             rng: &mut R,
                             program: &Program<T>,
                             clause: ClauseIndex,
                             literal: LiteralIndex,
                             term: TermIndex)
                             -> GenResult<Constant>
        where R: Rng,
              T: TruthValue
    {
        let predicate = program.get_clause_by_idx(clause).body[literal].predicate;
        let max_constant = self.max_constant
            .get(&(predicate, term))
            .cloned()
            .unwrap_or(0);
        self.gen_range_plus_k(rng, 1 + max_constant)
    }

    fn choose_head_term<R, T>(&mut self,
                              rng: &mut R,
                              program: &Program<T>,
                              clause: ClauseIndex)
                              -> GenResult<TermIndex>
        where R: Rng,
              T: TruthValue
    {
        self.gen_range(rng,
                       program
                           .get_clause_by_idx(clause)
                           .head
                           .as_ref()
                           .unwrap()
                           .terms
                           .len())
    }

    fn choose_head_variable<R, T>(&mut self,
                                  rng: &mut R,
                                  program: &Program<T>,
                                  clause: ClauseIndex,
                                  term: TermIndex)
                                  -> GenResult<Variable>
        where R: Rng,
              T: TruthValue
    {
        let num_variables = program.get_clause_by_idx(clause).num_variables();
        self.gen_range(rng, num_variables)
    }

    fn choose_head_constant<R, T>(&mut self,
                                  rng: &mut R,
                                  program: &Program<T>,
                                  clause: ClauseIndex,
                                  term: TermIndex)
                                  -> GenResult<Constant>
        where R: Rng,
              T: TruthValue
    {
        let predicate = program
            .get_clause_by_idx(clause)
            .head
            .as_ref()
            .unwrap()
            .predicate;
        let max_constant = self.max_constant
            .get(&(predicate, term))
            .cloned()
            .unwrap_or(0);
        self.gen_range_plus_k(rng, 1 + max_constant)
    }
}

impl MutationOpGenerator {
    pub fn uniform() -> Self {
        let mut result = MutationOpGenerator {
            num_body_ops: 1,
            num_head_ops: 1,
            body_swap_prob: 1.0,
            body_bind_constant_prob: 1.0,
            body_bind_variable_prob: 1.0,
            body_insert_literal_prob: 1.0,
            body_remove_literal_prob: 1.0,
            head_bind_constant_prob: 1.0,
            head_bind_variable_prob: 1.0,
        };
        result.normalize();
        result
    }

    pub fn new(num_body_ops: i64,
               num_head_ops: i64,
               body_swap_prob: f64,
               body_bind_constant_prob: f64,
               body_bind_variable_prob: f64,
               body_insert_literal_prob: f64,
               body_remove_literal_prob: f64,
               head_bind_constant_prob: f64,
               head_bind_variable_prob: f64)
               -> Self {
        MutationOpGenerator {
            num_body_ops,
            num_head_ops,
            body_swap_prob,
            body_bind_constant_prob,
            body_bind_variable_prob,
            body_insert_literal_prob,
            body_remove_literal_prob,
            head_bind_constant_prob,
            head_bind_variable_prob,
        }
    }

    fn normalize(&mut self) {
        let mut head_total = 0f64;
        head_total += self.head_bind_constant_prob;
        head_total += self.head_bind_variable_prob;
        if head_total != 0.0 {
            self.head_bind_constant_prob /= head_total;
            self.head_bind_variable_prob /= head_total;
        }
        let mut body_total = 0f64;
        body_total += self.body_swap_prob;
        body_total += self.body_bind_constant_prob;
        body_total += self.body_bind_variable_prob;
        body_total += self.body_insert_literal_prob;
        body_total += self.body_remove_literal_prob;
        if body_total != 0.0 {
            self.body_swap_prob /= body_total;
            self.body_bind_constant_prob /= body_total;
            self.body_bind_variable_prob /= body_total;
            self.body_insert_literal_prob /= body_total;
            self.body_remove_literal_prob /= body_total;
        }
    }

    fn generate_head<R, S, T>(&self,
                              rng: &mut R,
                              selector: &mut S,
                              program: &Program<T>,
                              clause: ClauseIndex)
                              -> GenResult<HeadMutationOp>
        where R: Rng,
              S: Selector,
              T: TruthValue
    {
        assert!(self.head_bind_constant_prob + self.head_bind_variable_prob != 0.0);
        let d = rng.next_f64();
        if d < self.head_bind_constant_prob {
            return self.generate_head_bind_constant(rng, selector, program, clause);
        } else {
            return self.generate_head_bind_variable(rng, selector, program, clause);
        }
    }

    fn generate_head_bind_variable<R, S, T>(&self,
                                            rng: &mut R,
                                            selector: &mut S,
                                            program: &Program<T>,
                                            clause: ClauseIndex)
                                            -> GenResult<HeadMutationOp>
        where R: Rng,
              S: Selector,
              T: TruthValue
    {
        let term = selector.choose_head_term(rng, program, clause)?;
        let variable = selector
            .choose_head_variable(rng, program, clause, term)?;
        Ok(HeadMutationOp::BindVariable(term, variable))
    }

    fn generate_head_bind_constant<R, S, T>(&self,
                                            rng: &mut R,
                                            selector: &mut S,
                                            program: &Program<T>,
                                            clause: ClauseIndex)
                                            -> GenResult<HeadMutationOp>
        where R: Rng,
              S: Selector,
              T: TruthValue
    {
        let term = selector.choose_head_term(rng, program, clause)?;
        let constant = selector
            .choose_head_constant(rng, program, clause, term)?;
        Ok(HeadMutationOp::BindConstant(term, constant))
    }


    fn generate_body<R, S, T>(&self,
                              rng: &mut R,
                              selector: &mut S,
                              program: &Program<T>,
                              clause: ClauseIndex)
                              -> GenResult<BodyMutationOp>
        where R: Rng,
              S: Selector,
              T: TruthValue
    {
        assert!(self.head_bind_constant_prob + self.head_bind_variable_prob != 0.0);
        let mut d = rng.next_f64();
        if d < self.body_swap_prob {
            return self.generate_body_swap(rng, selector, program, clause);
        } else {
            d -= self.body_swap_prob;
        }
        if d < self.body_bind_constant_prob {
            return self.generate_body_bind_constant(rng, selector, program, clause);
        } else {
            d -= self.body_bind_constant_prob;
        }
        if d < self.body_bind_variable_prob {
            return self.generate_body_bind_variable(rng, selector, program, clause);
        } else {
            d -= self.body_bind_variable_prob;
        }
        if d < self.body_insert_literal_prob {
            return self.generate_body_insert_literal(rng, selector, program, clause);
        }
        return self.generate_body_remove_literal(rng, selector, program, clause);
    }

    fn generate_body_swap<R, S, T>(&self,
                                   rng: &mut R,
                                   selector: &mut S,
                                   program: &Program<T>,
                                   clause: ClauseIndex)
                                   -> GenResult<BodyMutationOp>
        where R: Rng,
              S: Selector,
              T: TruthValue
    {
        let literal_a = selector.choose_literal(rng, program, clause)?;
        let term_a = selector.choose_term(rng, program, clause, literal_a)?;
        let literal_b = selector.choose_literal(rng, program, clause)?;
        let term_b = selector.choose_term(rng, program, clause, literal_b)?;
        Ok(BodyMutationOp::Swap((literal_a, term_a), (literal_b, term_b)))
    }

    fn generate_body_bind_constant<R, S, T>(&self,
                                            rng: &mut R,
                                            selector: &mut S,
                                            program: &Program<T>,
                                            clause: ClauseIndex)
                                            -> GenResult<BodyMutationOp>
        where R: Rng,
              S: Selector,
              T: TruthValue
    {
        let literal = selector.choose_literal(rng, program, clause)?;
        let term = selector.choose_term(rng, program, clause, literal)?;
        let constant = selector
            .choose_constant(rng, program, clause, literal, term)?;
        Ok(BodyMutationOp::BindConstant((literal, term), constant))
    }

    fn generate_body_bind_variable<R, S, T>(&self,
                                            rng: &mut R,
                                            selector: &mut S,
                                            program: &Program<T>,
                                            clause: ClauseIndex)
                                            -> GenResult<BodyMutationOp>
        where R: Rng,
              S: Selector,
              T: TruthValue
    {
        let literal = selector.choose_literal(rng, program, clause)?;
        let term = selector.choose_term(rng, program, clause, literal)?;
        let variable = selector
            .choose_variable(rng, program, clause, literal, term)?;
        Ok(BodyMutationOp::BindVariable((literal, term), variable))
    }

    fn generate_body_remove_literal<R, S, T>(&self,
                                             rng: &mut R,
                                             selector: &mut S,
                                             program: &Program<T>,
                                             clause: ClauseIndex)
                                             -> GenResult<BodyMutationOp>
        where R: Rng,
              S: Selector,
              T: TruthValue
    {
        let literal = selector.choose_literal_to_remove(rng, program, clause)?;
        Ok(BodyMutationOp::RemoveLiteral(literal))
    }

    fn generate_body_insert_literal<R, S, T>(&self,
                                             rng: &mut R,
                                             selector: &mut S,
                                             program: &Program<T>,
                                             clause: ClauseIndex)
                                             -> GenResult<BodyMutationOp>
        where R: Rng,
              S: Selector,
              T: TruthValue
    {
        let predicate = selector.choose_predicate(rng, program, clause)?;
        // TODO(zentner): Select terms for new literal here?
        Ok(BodyMutationOp::InsertLiteral(predicate))
    }
}

#[cfg(test)]
mod tests {
    use super::{BodyMutationOp, HeadMutationOp, MutationOpGenerator, MutationState, Selector,
                UniformPlusKSelector};
    use parser::program;
    use rand::SeedableRng;
    use rand::XorShiftRng;
    use truth_value::MaxFloat64;

    #[test]
    fn can_use_body_good_op() {
        let (_, program, _) = program::<MaxFloat64>(r#"
        a(0) :- a(0), b(1)
        "#)
                .unwrap()
                .0;
        let clause = program.get_clause_by_idx(0);
        let mut_state = MutationState::new(&clause);
        assert!(mut_state.can_use_body_mut_op(BodyMutationOp::Swap((0, 0), (1, 0)), &clause));
        for lit in 0..2 {
            assert!(mut_state.can_use_body_mut_op(BodyMutationOp::RemoveLiteral(lit), &clause));
            for v in 0..3 {
                assert!(mut_state.can_use_body_mut_op(BodyMutationOp::BindVariable((lit, 0), v),
                                                      &clause));
                assert!(mut_state.can_use_body_mut_op(BodyMutationOp::BindConstant((lit, 0), v),
                                                      &clause));
            }
        }
        assert!(mut_state.can_use_body_mut_op(BodyMutationOp::InsertLiteral(0), &clause));
        assert!(mut_state.can_use_body_mut_op(BodyMutationOp::InsertLiteral(1), &clause));
    }

    #[test]
    fn cannot_use_body_bad_op() {
        let (_, program, _) = program::<MaxFloat64>(r#"
        a(X) :- a(0), b(X)
        "#)
                .unwrap()
                .0;
        let clause = program.get_clause_by_idx(0);
        let mut_state = MutationState::new(&clause);
        assert!(mut_state.can_use_body_mut_op(BodyMutationOp::Swap((0, 0), (1, 0)), &clause));
        assert!(mut_state.can_use_body_mut_op(BodyMutationOp::RemoveLiteral(0), &clause));
        assert!(!mut_state.can_use_body_mut_op(BodyMutationOp::RemoveLiteral(1), &clause));
        assert!(!mut_state.can_use_body_mut_op(BodyMutationOp::BindConstant((1, 0), 0), &clause));
        assert!(!mut_state.can_use_body_mut_op(BodyMutationOp::BindVariable((1, 0), 1), &clause));
    }

    #[test]
    fn can_use_head_good_op() {
        let (_, program, _) = program::<MaxFloat64>(r#"
        a(0) :- a(0), b(X)
        "#)
                .unwrap()
                .0;
        let clause = program.get_clause_by_idx(0);
        let mut_state = MutationState::new(&clause);
        assert!(mut_state.can_use_head_mut_op(HeadMutationOp::BindVariable(0, 0), &clause));
        assert!(mut_state.can_use_head_mut_op(HeadMutationOp::BindConstant(0, 1), &clause));
    }

    #[test]
    fn cannot_use_head_bad_op() {
        let (_, program, _) = program::<MaxFloat64>(r#"
        a(0) :- a(0), b(0)
        "#)
                .unwrap()
                .0;
        let clause = program.get_clause_by_idx(0);
        let mut_state = MutationState::new(&clause);
        assert!(!mut_state.can_use_head_mut_op(HeadMutationOp::BindVariable(0, 0), &clause));
    }

    #[test]
    fn can_generate_mutations() {
        let mut gen = MutationOpGenerator::uniform();
        let mut rng = XorShiftRng::from_seed([0xde, 0xad, 0xbe, 0xef]);
        let (facts, program, _) = program::<MaxFloat64>(r#"
        a(0) :- a(0)
        "#)
                .unwrap()
                .0;
        let mut selector = UniformPlusKSelector::new(0, &program, &facts);
        let ops = (0..10)
            .filter_map(|_| {
                            gen.generate_head(&mut rng, &mut selector, &program, 0)
                                .ok()
                        })
            .collect::<Vec<_>>();
        assert!(ops.contains(&HeadMutationOp::BindConstant(0, 0)));
    }

    #[test]
    fn do_mutations() {
        let mut gen = MutationOpGenerator::uniform();
        let mut rng = XorShiftRng::from_seed([0xde, 0xad, 0xbe, 0xef]);
        let (facts, mut program, _) = program::<MaxFloat64>(r#"
        a(0) :- a(0)
        "#)
                .unwrap()
                .0;
        let mut selector = UniformPlusKSelector::new(1, &program, &facts);
        for _ in 0..100 {
            let mut clause = program.get_clause_by_idx(0).clone();
            let mut mut_state = MutationState::new(&clause);
            if let Ok(body_op) = gen.generate_body(&mut rng, &mut selector, &program, 0) {
                mut_state.apply_body_mut_op(body_op, &mut clause, &mut rng, &program);
            }
            if let Ok(head_op) = gen.generate_head(&mut rng, &mut selector, &program, 0) {
                mut_state.apply_head_mut_op(head_op, &mut clause);
            }
            program.clear_clauses();
            println!("clause = {:#?}", clause);
            program.push_clause_simple(clause);
        }
    }
}
