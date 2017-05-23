use program::Program;
use rand::Rng;
use truth_value::TruthValue;
use types::{ClauseIndex, Constant, LiteralIndex, Predicate, TermIndex, Variable};

pub type SelectorResult<T> = Result<T, &'static str>;

pub trait Selector {
    fn choose_clause<R, T>(&mut self,
                           rng: &mut R,
                           program: &Program<T>)
                           -> SelectorResult<ClauseIndex>
        where R: Rng,
              T: TruthValue;

    fn choose_predicate<R, T>(&mut self,
                              rng: &mut R,
                              program: &Program<T>,
                              clause: ClauseIndex)
                              -> SelectorResult<Predicate>
        where R: Rng,
              T: TruthValue;

    fn choose_literal<R, T>(&mut self,
                            rng: &mut R,
                            program: &Program<T>,
                            clause: ClauseIndex)
                            -> SelectorResult<LiteralIndex>
        where R: Rng,
              T: TruthValue;

    fn choose_literal_to_remove<R, T>(&mut self,
                                      rng: &mut R,
                                      program: &Program<T>,
                                      clause: ClauseIndex)
                                      -> SelectorResult<LiteralIndex>
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
                         -> SelectorResult<TermIndex>
        where R: Rng,
              T: TruthValue;

    fn choose_variable<R, T>(&mut self,
                             rng: &mut R,
                             program: &Program<T>,
                             clause: ClauseIndex,
                             literal: LiteralIndex,
                             term: TermIndex)
                             -> SelectorResult<Variable>
        where R: Rng,
              T: TruthValue;

    fn choose_constant<R, T>(&mut self,
                             rng: &mut R,
                             program: &Program<T>,
                             clause: ClauseIndex,
                             literal: LiteralIndex,
                             term: TermIndex)
                             -> SelectorResult<Constant>
        where R: Rng,
              T: TruthValue;

    fn choose_head_term<R, T>(&mut self,
                              rng: &mut R,
                              program: &Program<T>,
                              clause: ClauseIndex)
                              -> SelectorResult<TermIndex>
        where R: Rng,
              T: TruthValue;

    fn choose_head_variable<R, T>(&mut self,
                                  rng: &mut R,
                                  program: &Program<T>,
                                  clause: ClauseIndex,
                                  term: TermIndex)
                                  -> SelectorResult<Variable>
        where R: Rng,
              T: TruthValue;

    fn choose_head_constant<R, T>(&mut self,
                                  rng: &mut R,
                                  program: &Program<T>,
                                  clause: ClauseIndex,
                                  term: TermIndex)
                                  -> SelectorResult<Constant>
        where R: Rng,
              T: TruthValue;
}
