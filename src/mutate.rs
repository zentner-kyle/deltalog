#![allow(dead_code)]

use program::Program;
#[cfg(test)]
use quickcheck::{Arbitrary, Gen};
use rand::Rng;
use selector::Selector;
use std::collections::HashSet;
use truth_value::TruthValue;
use types::{Clause, ClauseIndex, Constant, Literal, LiteralIndex, Predicate, Term, TermIndex,
            Variable};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MutationState {
    out_var_supports: Vec<HashSet<(LiteralIndex, TermIndex)>>,
    max_literals: usize,
}

impl MutationState {
    pub fn new(clause: &Clause, max_literals: usize) -> Self {
        let mut out_var_supports = vec![HashSet::new(); clause.num_output_variables()];
        for (lit_idx, literal) in clause.body.iter().enumerate() {
            for (term_idx, term) in literal.terms.iter().enumerate() {
                if let Term::Variable(varb) = *term {
                    if clause.head_contains_var(varb) {
                        out_var_supports[varb].insert((lit_idx, term_idx));
                    }
                }
            }
        }
        MutationState {
            out_var_supports,
            max_literals,
        }
    }

    fn can_use_head_mut_op(&self, head_op: HeadMutationOp, clause: &Clause) -> bool {
        match head_op {
            HeadMutationOp::BindConstant(term_idx, _) => term_idx < clause.head.terms.len(),
            HeadMutationOp::BindVariable(term_idx, varb) => {
                term_idx < clause.head.terms.len() && clause.contains_variable_in_body(varb)
            }
        }
    }

    pub fn checked_apply_head_mut_op<R, T>(&mut self,
                                           head_op: HeadMutationOp,
                                           clause: &mut Clause,
                                           _rng: &mut R,
                                           _program: &Program<T>)
                                           -> bool
        where R: Rng,
              T: TruthValue
    {
        if self.can_use_head_mut_op(head_op, clause) {
            self.apply_head_mut_op(head_op, clause);
            let fresh_self = Self::new(clause, self.max_literals);
            assert!(clause.is_valid() && *self == fresh_self);
            true
        } else {
            false
        }
    }

    fn apply_head_mut_op(&mut self, head_op: HeadMutationOp, clause: &mut Clause) {
        match head_op {
            HeadMutationOp::BindConstant(term_idx, cst) => {
                clause.head.terms[term_idx] = Term::Constant(cst);
            }
            HeadMutationOp::BindVariable(term_idx, varb) => {
                clause.head.terms[term_idx] = Term::Variable(varb);
                while varb >= self.out_var_supports.len() {
                    self.out_var_supports.push(HashSet::new());
                }
                for (lit_idx, literal) in clause.body.iter().enumerate() {
                    for (term_idx, term) in literal.terms.iter().enumerate() {
                        if let Term::Variable(varb) = *term {
                            if clause.head_contains_var(varb) {
                                self.out_var_supports[varb].insert((lit_idx, term_idx));
                            }
                        }
                    }
                }
            }
        }
        for (i, supports) in self.out_var_supports.iter_mut().enumerate() {
            if !clause.head_contains_var(i) {
                supports.clear();
            }
        }
        while self.out_var_supports.len() > 0 &&
              !clause.head_contains_var(self.out_var_supports.len() - 1) {
            self.out_var_supports.pop();
        }
    }

    fn can_use_body_mut_op(&self, body_op: BodyMutationOp, clause: &Clause) -> bool {
        match body_op {
            BodyMutationOp::Swap((lit_a, term_a), (lit_b, term_b)) => {
                if lit_a >= clause.body.len() || lit_b >= clause.body.len() {
                    false
                } else if term_a >= clause.body[lit_a].terms.len() ||
                          term_b >= clause.body[lit_b].terms.len() {
                    false
                } else {
                    true
                }
            }
            BodyMutationOp::InsertLiteral(_) => clause.body.len() < self.max_literals,
            BodyMutationOp::BindConstant((lit_idx, term_idx), _) |
            BodyMutationOp::BindVariable((lit_idx, term_idx), _) => {
                if lit_idx < clause.body.len() && term_idx < clause.body[lit_idx].terms.len() {
                    if let Term::Variable(varb) = clause.body[lit_idx].terms[term_idx] {
                        if varb < self.out_var_supports.len() {
                            self.out_var_supports[varb].len() > 1
                        } else {
                            true
                        }
                    } else {
                        true
                    }
                } else {
                    false
                }
            }
            BodyMutationOp::RemoveLiteral(lit_idx) => {
                let mut variable_removals = vec![0; clause.num_output_variables()];
                if clause.body.len() <= 1 {
                    return false;
                }
                if let Some(lit) = clause.body.get(lit_idx) {
                    for term in &lit.terms {
                        if let Term::Variable(varb) = *term {
                            if varb < variable_removals.len() {
                                variable_removals[varb] += 1;
                            } else {
                                return false;
                            }
                        }
                    }
                    for (varb, removals) in variable_removals.iter().enumerate() {
                        if varb < self.out_var_supports.len() {
                            if self.out_var_supports[varb].len() <= *removals {
                                return false;
                            }
                        } else {
                            return true;
                        }
                    }
                    return true;
                } else {
                    return false;
                }
            }
        }
    }

    pub fn checked_apply_body_mut_op<R, T>(&mut self,
                                           body_op: BodyMutationOp,
                                           clause: &mut Clause,
                                           rng: &mut R,
                                           program: &Program<T>)
                                           -> bool
        where R: Rng,
              T: TruthValue
    {
        if self.can_use_body_mut_op(body_op, clause) {
            self.apply_body_mut_op(body_op, clause, rng, program);
            let fresh_self = Self::new(clause, self.max_literals);
            assert!(clause.is_valid() && *self == fresh_self);
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
                if let Term::Variable(varb) = a {
                    if clause.head_contains_var(varb) {
                        self.out_var_supports[varb].remove(&(lit_a, term_a));
                    }
                }
                if let Term::Variable(varb) = b {
                    if clause.head_contains_var(varb) {
                        self.out_var_supports[varb].remove(&(lit_b, term_b));
                    }
                }
                clause.body[lit_a].terms[term_a] = b;
                clause.body[lit_b].terms[term_b] = a;
                if let Term::Variable(varb) = a {
                    if clause.head_contains_var(varb) {
                        self.out_var_supports[varb].insert((lit_b, term_b));
                    }
                }
                if let Term::Variable(varb) = b {
                    if clause.head_contains_var(varb) {
                        self.out_var_supports[varb].insert((lit_a, term_a));
                    }
                }
            }
            BodyMutationOp::InsertLiteral(pred) => {
                let mut terms = (0..(1 + clause.num_variables()))
                    .map(|i| Term::Variable(i))
                    .collect::<Vec<_>>();
                let num_terms = terms.len();
                if num_terms == 0 {
                    return;
                }
                if let Some(needed_num_terms) = program.num_terms(pred) {
                    while terms.len() < needed_num_terms {
                        let to_add = rng.choose(&terms).unwrap().clone();
                        terms.push(to_add);
                    }
                    rng.shuffle(&mut terms);
                    terms.truncate(needed_num_terms);
                    let lit_idx = clause.body.len();
                    for (term_idx, term) in terms.iter().enumerate() {
                        if let Term::Variable(v) = *term {
                            if clause.head_contains_var(v) {
                                self.out_var_supports[v].insert((lit_idx, term_idx));
                            }
                        }
                    }
                    clause.body.push(Literal::new_from_vec(pred, terms));
                }
            }
            BodyMutationOp::BindConstant((lit_idx, term_idx), cst) => {
                if let Term::Variable(v) = clause.body[lit_idx].terms[term_idx] {
                    if v < self.out_var_supports.len() {
                        self.out_var_supports[v].remove(&(lit_idx, term_idx));
                    }
                }
                clause.body[lit_idx].terms[term_idx] = Term::Constant(cst);
            }
            BodyMutationOp::BindVariable((lit_idx, term_idx), varb) => {
                if let Term::Variable(v) = clause.body[lit_idx].terms[term_idx] {
                    if v < self.out_var_supports.len() {
                        self.out_var_supports[v].remove(&(lit_idx, term_idx));
                    }
                }
                clause.body[lit_idx].terms[term_idx] = Term::Variable(varb);
                if clause.head_contains_var(varb) {
                    self.out_var_supports[varb].insert((lit_idx, term_idx));
                }
            }
            BodyMutationOp::RemoveLiteral(lit_idx) => {
                for (term_idx, term) in clause.body[lit_idx].terms.iter().enumerate() {
                    if let Term::Variable(varb) = *term {
                        if varb < self.out_var_supports.len() {
                            self.out_var_supports[varb].remove(&(lit_idx, term_idx));
                        }
                    }
                }
                for supports in &mut self.out_var_supports {
                    let mut out_supports = HashSet::with_capacity(supports.len());
                    for (mut l_idx, term_idx) in supports.drain() {
                        if l_idx > lit_idx {
                            l_idx -= 1;
                        }
                        out_supports.insert((l_idx, term_idx));
                    }
                    *supports = out_supports;
                }
                clause.body.remove(lit_idx);
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BodyMutationOp {
    Swap((LiteralIndex, TermIndex), (LiteralIndex, TermIndex)),
    BindConstant((LiteralIndex, TermIndex), Constant),
    BindVariable((LiteralIndex, TermIndex), Variable),
    InsertLiteral(Predicate),
    RemoveLiteral(LiteralIndex),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum HeadMutationOp {
    BindConstant(TermIndex, Constant),
    BindVariable(TermIndex, Variable),
}

#[derive(Debug, Clone)]
pub struct MutationOpGenerator {
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

pub type GenResult<T> = Result<T, &'static str>;

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

    pub fn generate_head<R, S, T>(&self,
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


    pub fn generate_body<R, S, T>(&self,
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
impl Arbitrary for HeadMutationOp {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        let size = g.size();
        match g.gen_range(0, 2) {
            0 => HeadMutationOp::BindConstant(g.gen_range(0, size), g.gen_range(0, size)),
            1 => HeadMutationOp::BindVariable(g.gen_range(0, size), g.gen_range(0, size)),
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
impl Arbitrary for BodyMutationOp {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        let size = g.size();
        match g.gen_range(0, 5) {
            0 => {
                BodyMutationOp::Swap((g.gen_range(0, size), g.gen_range(0, size)),
                                     (g.gen_range(0, size), g.gen_range(0, size)))
            }
            1 => {
                BodyMutationOp::BindConstant((g.gen_range(0, size), g.gen_range(0, size)),
                                             g.gen_range(0, size))
            }
            2 => {
                BodyMutationOp::BindVariable((g.gen_range(0, size), g.gen_range(0, size)),
                                             g.gen_range(0, size))
            }
            3 => BodyMutationOp::InsertLiteral(g.gen_range(0, size)),
            4 => BodyMutationOp::RemoveLiteral(g.gen_range(0, size)),
            _ => unreachable!(),
        }
    }
}


#[cfg(test)]
mod tests {
    use super::{BodyMutationOp, HeadMutationOp, MutationOpGenerator, MutationState};
    use parser::program;
    use rand::SeedableRng;
    use rand::XorShiftRng;
    use std::cmp::max;
    use truth_value::MaxFloat64;
    use uniform_selector::UniformPlusKSelector;

    quickcheck! {
        fn clause_valid_after_operations(head_ops: Vec<HeadMutationOp>, body_ops: Vec<BodyMutationOp>) -> () {
            let mut rng = XorShiftRng::from_seed([0xde, 0xad, 0xbe, 0xef]);
            let (_, program, _) = program::<MaxFloat64>(r#"
            a(0) :- a(0), b(1)
            "#)
                    .unwrap()
                    .0;
            let mut clause = program.get_clause_by_idx(0).clone();
            let mut state = MutationState::new(&clause, 10);
            for i in 0..max(head_ops.len(), body_ops.len()) {
                if i < head_ops.len() {
                    state.checked_apply_head_mut_op(head_ops[i], &mut clause, &mut rng, &program);
                }
                if i < body_ops.len() {
                    state.checked_apply_body_mut_op(body_ops[i], &mut clause, &mut rng, &program);
                }
            }
        }
    }

    #[test]
    fn can_use_body_good_op() {
        let (_, program, _) = program::<MaxFloat64>(r#"
        a(0) :- a(0), b(1)
        "#)
                .unwrap()
                .0;
        let clause = program.get_clause_by_idx(0);
        let mut_state = MutationState::new(&clause, 10);
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
        let mut_state = MutationState::new(&clause, 10);
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
        let mut_state = MutationState::new(&clause, 10);
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
        let mut_state = MutationState::new(&clause, 10);
        assert!(!mut_state.can_use_head_mut_op(HeadMutationOp::BindVariable(0, 0), &clause));
    }

    #[test]
    fn can_generate_mutations() {
        let gen = MutationOpGenerator::uniform();
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
        let gen = MutationOpGenerator::uniform();
        let mut rng = XorShiftRng::from_seed([0xde, 0xad, 0xbe, 0xef]);
        let (facts, mut program, _) = program::<MaxFloat64>(r#"
        a(0) :- a(0)
        "#)
                .unwrap()
                .0;
        let mut selector = UniformPlusKSelector::new(1, &program, &facts);
        for _ in 0..100 {
            let mut clause = program.get_clause_by_idx(0).clone();
            let mut mut_state = MutationState::new(&clause, 10);
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
