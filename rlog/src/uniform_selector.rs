use fact_table::FactTable;
use program::Program;
use rand::Rng;
use selector::{Selector, SelectorResult};
use std::collections::HashMap;
use truth_value::TruthValue;
use types::{ClauseIndex, Constant, LiteralIndex, Predicate, TermIndex, Variable};

pub struct UniformPlusKSelector {
    k: i64,
    max_constant: HashMap<(Predicate, TermIndex), Constant>,
}

impl UniformPlusKSelector {
    #[allow(dead_code)]
    pub fn new<T>(k: i64, _program: &Program<T>, facts: &FactTable<T>) -> Self
        where T: TruthValue
    {
        UniformPlusKSelector {
            k,
            max_constant: facts.max_constant_table(),
        }
    }

    fn gen_range<R>(&self, rng: &mut R, end: usize) -> SelectorResult<usize>
        where R: Rng
    {
        if end == 0 {
            Err("Nothing to choose from.")
        } else {
            Ok(rng.gen_range(0, end))
        }
    }

    fn gen_range_plus_k<R>(&self, rng: &mut R, base: usize) -> SelectorResult<usize>
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
                              _clause: ClauseIndex)
                              -> SelectorResult<Predicate>
        where R: Rng,
              T: TruthValue
    {
        self.gen_range_plus_k(rng, program.num_predicates())
    }

    fn choose_clause<R, T>(&mut self,
                           rng: &mut R,
                           program: &Program<T>)
                           -> SelectorResult<ClauseIndex>
        where R: Rng,
              T: TruthValue
    {
        self.gen_range(rng, program.num_clauses())
    }

    fn choose_literal<R, T>(&mut self,
                            rng: &mut R,
                            program: &Program<T>,
                            clause: ClauseIndex)
                            -> SelectorResult<LiteralIndex>
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
                         -> SelectorResult<TermIndex>
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
                             _literal: LiteralIndex,
                             _term: TermIndex)
                             -> SelectorResult<Variable>
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
                             -> SelectorResult<Constant>
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
                              -> SelectorResult<TermIndex>
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
                                  _term: TermIndex)
                                  -> SelectorResult<Variable>
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
                                  -> SelectorResult<Constant>
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
