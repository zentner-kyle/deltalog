#![allow(dead_code)]
//! We have a set of constant vectors.
//!
//! For the number of constraints n:
//! For the number of source terms s:
//! Create n * s variables representing source constraint i in n applying to source term j in s.
//! For each source vector:
//!   Create a variable representing that arrangement, and condition the following clauses on that
//!   variable
//!   For each forbidden constant vector:
//!     Create a cause table for that constant vector given the sources.
//!     For each cause, create a cnf clause requiring one of the constraints between two of the
//!     non-equal source terms
//!   For each required constant vector:
//!     Create a cause table for that constant vector given the sources.
//!     For each cause, create a variable representing that cause being responsible for the
//!     constant vector.
//!     Create a cnf clause requiring either that cause is not responsible, or none of the
//!     constraints prevent that cause from operating.
//!     Require that at least one of the causes is responsible for the constant vector.

use program::Program;
use std::iter::repeat;
use std::sync::Arc;
use truth_value::TruthValue;
use types::Predicate;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Source {
    predicate: Predicate,
    term_idx: usize,
}

pub struct SourceVecIter<'a, T>
    where T: 'a + TruthValue
{
    sources: Arc<Vec<Source>>,
    minimum_num_sources: usize,
    maximum_num_sources: usize,
    program: &'a Program<T>,
}

impl<'a, T> Iterator for SourceVecIter<'a, T>
    where T: 'a + TruthValue
{
    type Item = Arc<Vec<Source>>;

    fn next(&mut self) -> Option<Self::Item> {
        {
            let sources = Arc::make_mut(&mut self.sources);
            if sources.len() < self.minimum_num_sources {
                *sources = repeat(Source {
                                      predicate: 0,
                                      term_idx: 0,
                                  })
                        .take(self.minimum_num_sources)
                        .collect();
            } else {
                let num_sources: usize = sources.len();
                for i in (0..num_sources).rev() {
                    {
                        let source = &mut sources[i];
                        if source.term_idx + 1 < self.program.get_num_terms(source.predicate) {
                            source.term_idx += 1;
                            break;
                        } else if source.predicate + 1 < self.program.num_predicates() {
                            source.term_idx = 0;
                            source.predicate += 1;
                            break;
                        } else {
                            source.term_idx = 0;
                            source.predicate = 0;
                            if i > 0 {
                                continue;
                            }
                        }
                    }
                    if num_sources < self.maximum_num_sources {
                        sources.push(Source {
                                         predicate: 0,
                                         term_idx: 0,
                                     });
                    } else {
                        return None;
                    }
                }
            }
        }
        Some(self.sources.clone())
    }
}

impl<'a, T> SourceVecIter<'a, T>
    where T: 'a + TruthValue
{
    fn new(minimum_num_sources: usize,
           maximum_num_sources: usize,
           program: &'a Program<T>)
           -> Self {
        SourceVecIter {
            sources: Arc::new(Vec::new()),
            program,
            minimum_num_sources,
            maximum_num_sources,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::program_lit;

    fn check_source_tuples(mut iter: SourceVecIter<()>,
                           source_tuples: &[Vec<(Predicate, usize)>]) {
        for tuples in source_tuples {
            assert_eq!(*iter.next().unwrap(),
                       tuples
                           .iter()
                           .map(|&(predicate, term_idx)| {
                                    Source {
                                        predicate,
                                        term_idx,
                                    }
                                })
                           .collect::<Vec<Source>>());
        }
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn source_iter_only_one_choice() {
        let prg = program_lit(r"
            a(0).
        ");
        let source_tuples = [vec![(0, 0)]];
        check_source_tuples(SourceVecIter::new(1, 1, &prg), &source_tuples);
    }

    #[test]
    fn source_iter_two_terms() {
        let prg = program_lit(r"
            a(0, 0).
        ");
        let source_tuples = [vec![(0, 0)], vec![(0, 1)]];
        check_source_tuples(SourceVecIter::new(1, 1, &prg), &source_tuples);
    }

    #[test]
    fn source_iter_two_predicates() {
        let prg = program_lit(r"
            a(0).
            b(0).
        ");
        let source_tuples = [vec![(0, 0)], vec![(1, 0)]];
        check_source_tuples(SourceVecIter::new(1, 1, &prg), &source_tuples);
    }

    #[test]
    fn source_iter_two_predicates_two_terms() {
        let prg = program_lit(r"
            a(0, 0).
            b(0, 0).
        ");
        let source_tuples = [vec![(0, 0)], vec![(0, 1)], vec![(1, 0)], vec![(1, 1)]];
        check_source_tuples(SourceVecIter::new(1, 1, &prg), &source_tuples);
    }

    #[test]
    fn source_iter_two_sources() {
        let prg = program_lit(r"
            a(0, 0).
            b(0, 0).
        ");
        let source_tuples = [vec![(0, 0)],
                             vec![(0, 1)],
                             vec![(1, 0)],
                             vec![(1, 1)],
                             vec![(0, 0), (0, 0)],
                             vec![(0, 0), (0, 1)],
                             vec![(0, 0), (1, 0)],
                             vec![(0, 0), (1, 1)],
                             vec![(0, 1), (0, 0)],
                             vec![(0, 1), (0, 1)],
                             vec![(0, 1), (1, 0)],
                             vec![(0, 1), (1, 1)],
                             vec![(1, 0), (0, 0)],
                             vec![(1, 0), (0, 1)],
                             vec![(1, 0), (1, 0)],
                             vec![(1, 0), (1, 1)],
                             vec![(1, 1), (0, 0)],
                             vec![(1, 1), (0, 1)],
                             vec![(1, 1), (1, 0)],
                             vec![(1, 1), (1, 1)]];
        check_source_tuples(SourceVecIter::new(1, 2, &prg), &source_tuples);
    }

    #[test]
    fn source_iter_three_sources() {
        let prg = program_lit(r"
            a(0, 0, 0).
            b(0, 0).
            c(0).
        ");
        let source_tuples = [vec![(0, 0)],
                             vec![(0, 1)],
                             vec![(0, 2)],
                             vec![(1, 0)],
                             vec![(1, 1)],
                             vec![(2, 0)],
                             vec![(0, 0), (0, 0)],
                             vec![(0, 0), (0, 1)],
                             vec![(0, 0), (0, 2)],
                             vec![(0, 0), (1, 0)],
                             vec![(0, 0), (1, 1)],
                             vec![(0, 0), (2, 0)],
                             vec![(0, 1), (0, 0)],
                             vec![(0, 1), (0, 1)],
                             vec![(0, 1), (0, 2)],
                             vec![(0, 1), (1, 0)],
                             vec![(0, 1), (1, 1)],
                             vec![(0, 1), (2, 0)],
                             vec![(0, 2), (0, 0)],
                             vec![(0, 2), (0, 1)],
                             vec![(0, 2), (0, 2)],
                             vec![(0, 2), (1, 0)],
                             vec![(0, 2), (1, 1)],
                             vec![(0, 2), (2, 0)],
                             vec![(1, 0), (0, 0)],
                             vec![(1, 0), (0, 1)],
                             vec![(1, 0), (0, 2)],
                             vec![(1, 0), (1, 0)],
                             vec![(1, 0), (1, 1)],
                             vec![(1, 0), (2, 0)],
                             vec![(1, 1), (0, 0)],
                             vec![(1, 1), (0, 1)],
                             vec![(1, 1), (0, 2)],
                             vec![(1, 1), (1, 0)],
                             vec![(1, 1), (1, 1)],
                             vec![(1, 1), (2, 0)],
                             vec![(2, 0), (0, 0)],
                             vec![(2, 0), (0, 1)],
                             vec![(2, 0), (0, 2)],
                             vec![(2, 0), (1, 0)],
                             vec![(2, 0), (1, 1)],
                             vec![(2, 0), (2, 0)]];
        check_source_tuples(SourceVecIter::new(1, 2, &prg), &source_tuples);
    }
}
