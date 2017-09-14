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

use cryptominisat::{Lbool, Lit, Solver};
use fact_table::{FactTable, PredicateFactIter};
use program::Program;
use std::collections::HashSet;
use std::collections::hash_map::{Entry, HashMap};
use std::iter::repeat;
use std::sync::Arc;
use std::time::Instant;
use truth_value::TruthValue;
use types::{Clause, Constant, Fact, Literal, Predicate, Term};

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

struct ConstantVecIter<'p, 'f, T>
    where T: 'p + 'f + TruthValue
{
    source_iter: SourceVecIter<'p, T>,
    state: Option<(Arc<Vec<Source>>, FactsFromSources<'f, T>)>,
    fact_table: &'f FactTable<T>,
}

impl<'p, 'f, T> ConstantVecIter<'p, 'f, T>
    where T: 'p + 'f + TruthValue
{
    pub fn new(minimum_num_sources: usize,
               maximum_num_sources: usize,
               program: &'p Program<T>,
               fact_table: &'f FactTable<T>)
               -> Self {
        ConstantVecIter {
            source_iter: SourceVecIter::new(minimum_num_sources, maximum_num_sources, program),
            state: None,
            fact_table,
        }
    }
}

impl<'p, 'f, T> Iterator for ConstantVecIter<'p, 'f, T>
    where T: 'p + 'f + TruthValue
{
    type Item = (Arc<Vec<Source>>, Arc<Vec<&'f Fact>>);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((ref sources, ref mut constants_iter)) = self.state {
            if let Some(facts) = constants_iter.next() {
                return Some((sources.clone(), facts));
            }
        }
        while let Some(sources) = self.source_iter.next() {
            if let Some(facts_iter) = FactsFromSources::from_sources(sources.clone(),
                                                                     self.fact_table) {
                self.state = Some((sources, facts_iter));
                return self.next();
            }
        }
        return None;
    }
}

struct FactsFromSources<'f, T>
    where T: 'f + TruthValue
{
    sources: Arc<Vec<Source>>,
    fact_table: &'f FactTable<T>,
    fact_iters: Vec<PredicateFactIter<'f, T>>,
    facts: Arc<Vec<&'f Fact>>,
    skip_update: bool,
}

impl<'f, T> FactsFromSources<'f, T>
    where T: 'f + TruthValue
{
    fn from_sources(sources: Arc<Vec<Source>>, fact_table: &'f FactTable<T>) -> Option<Self> {
        let mut facts = Vec::new();
        let mut fact_iters = Vec::new();
        for source in sources.iter() {
            let mut fact_iter = fact_table.predicate_facts(source.predicate);
            if let Some((fact, _truth)) = fact_iter.next() {
                facts.push(fact);
                fact_iters.push(fact_iter);
            } else {
                return None;
            }
        }
        Some(FactsFromSources {
                 sources,
                 fact_table,
                 fact_iters,
                 facts: Arc::new(facts),
                 skip_update: true,
             })
    }
}

impl<'f, T> Iterator for FactsFromSources<'f, T>
    where T: 'f + TruthValue
{
    type Item = Arc<Vec<&'f Fact>>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut should_return_facts = false;
        if !self.skip_update {
            let facts = Arc::make_mut(&mut self.facts);

            for i in (0..self.fact_iters.len()).rev() {
                if let Some((fact, _truth)) = self.fact_iters[i].next() {
                    facts[i] = fact;
                    should_return_facts = true;
                    break;
                } else if i > 0 {
                    self.fact_iters[i] = self.fact_table
                        .predicate_facts(self.sources[i].predicate);
                }
            }
        } else {
            self.skip_update = false;
            should_return_facts = true;
        }
        if should_return_facts {
            return Some(self.facts.clone());
        } else {
            return None;
        }
    }
}


#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SourceTerm {
    source_idx: usize,
    term_idx: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct EqualityConstraint(SourceTerm, SourceTerm);

fn constant_vec_from_sources_facts(sources: &[Source], facts: &[&Fact]) -> Vec<Constant> {
    let mut result = Vec::new();
    for (source, fact) in sources.iter().zip(facts) {
        result.push(fact.terms[source.term_idx]);
    }
    return result;
}

fn violated_constraint_literals(solver: &mut Solver,
                                equality_constraints: &mut HashMap<EqualityConstraint, Lit>,
                                facts: &[&Fact])
                                -> Vec<Lit> {

    let mut result = Vec::new();
    for (source_idx_a, fact_a) in facts.iter().enumerate() {
        for (source_idx_b, fact_b) in facts.iter().enumerate().skip(source_idx_a) {
            for (term_idx_a, a) in fact_a.terms.iter().enumerate() {
                let to_skip;
                if source_idx_a == source_idx_b {
                    to_skip = term_idx_a + 1;
                } else {
                    to_skip = 0;
                }
                for (term_idx_b, b) in fact_b.terms.iter().enumerate().skip(to_skip) {
                    if a != b {
                        let source_term_a = SourceTerm {
                            source_idx: source_idx_a,
                            term_idx: term_idx_a,
                        };
                        let source_term_b = SourceTerm {
                            source_idx: source_idx_b,
                            term_idx: term_idx_b,
                        };
                        let constraint = EqualityConstraint(source_term_a, source_term_b);
                        let lit = match equality_constraints.entry(constraint) {
                            Entry::Occupied(pair) => pair.get().clone(),
                            Entry::Vacant(pair) => pair.insert(solver.new_var()).clone(),
                        };
                        result.push(lit);
                    }
                }
            }
        }
    }
    return result;
}

struct SynthesisState {
    source_permutation_vars: HashMap<Vec<Source>, Lit>,
    equality_constraint_vars: HashMap<EqualityConstraint, Lit>,
    solver: Solver,
}

impl SynthesisState {
    fn new() -> Self {
        let mut solver = Solver::new();
        solver.set_num_threads(4);
        SynthesisState {
            source_permutation_vars: HashMap::new(),
            equality_constraint_vars: HashMap::new(),
            solver,
        }
    }

    fn require_constant_vector<T>(&mut self,
                                  required: &[Constant],
                                  forbidden: &HashSet<Vec<Constant>>,
                                  program: &Program<T>,
                                  facts: &FactTable<T>,
                                  min_sources: usize,
                                  max_sources: usize)
        where T: TruthValue
    {
        let start_of_source_iteration = Instant::now();
        for sources in SourceVecIter::new(min_sources, max_sources, program) {
            let use_sources = match self.source_permutation_vars
                      .entry(sources.as_ref().clone()) {
                Entry::Occupied(entry) => entry.get().clone(),
                Entry::Vacant(entry) => entry.insert(self.solver.new_var()).clone(),
            };
            let mut had_required = false;
            if let Some(facts_iter) = FactsFromSources::from_sources(sources.clone(), facts) {
                for facts in facts_iter {
                    let violated_constraints =
                        violated_constraint_literals(&mut self.solver,
                                                     &mut self.equality_constraint_vars,
                                                     &facts);
                    let constant_vec = constant_vec_from_sources_facts(&sources, &facts);
                    let constant_vec_prefix = &constant_vec[0..required.len()];
                    if required == constant_vec_prefix {
                        // if use_sources, no violated EqualityConstraint
                        for constraint_lit in violated_constraints.iter() {
                            // Either:
                            // - Don't use the source
                            // - Don't violate the constraint
                            self.solver
                                .add_clause(&[!use_sources, !constraint_lit.clone()]);
                        }
                        had_required = true;
                    }
                    if forbidden.contains(constant_vec_prefix) {
                        let mut clause = violated_constraints;
                        if clause.len() == 0 {
                            self.solver.add_clause(&[!use_sources]);
                            break;
                        } else {
                            clause.push(!use_sources);
                            // Either:
                            // - Don't use the source
                            // - Violate one of the constraints
                            self.solver.add_clause(&clause);
                        }
                    }
                }
            }
            if !had_required {
                self.solver.add_clause(&[!use_sources]);
            }
        }
        println!("number of variables: {}", self.solver.nvars());
        println!("source iteration took {} seconds",
                 start_of_source_iteration.elapsed().as_secs());
    }

    fn solve(mut self) -> Option<(Vec<Source>, Vec<EqualityConstraint>)> {
        if self.source_permutation_vars.len() == 0 {
            return None;
        }

        let use_at_least_one_set_of_sources = self.source_permutation_vars
            .iter()
            .map(|(_, &lit)| lit)
            .collect::<Vec<_>>();
        self.solver.add_clause(&use_at_least_one_set_of_sources);

        let start_of_solving = Instant::now();
        let solve_result = self.solver.solve();
        println!("solving took {} seconds",
                 start_of_solving.elapsed().as_secs());
        if solve_result != Lbool::True {
            return None;
        }

        let solver = &self.solver;
        let constraints = self.equality_constraint_vars
            .iter()
            .filter_map(|(&c, &v)| if solver.is_true(v) { Some(c) } else { None })
            .collect();
        let sources = self.source_permutation_vars
            .iter()
            .filter_map(|(s, &v)| if solver.is_true(v) {
                            Some(s.clone())
                        } else {
                            None
                        })
            .collect::<Vec<_>>();
        println!("sources = {:?}", sources);

        for (sources, v) in self.source_permutation_vars.into_iter() {
            if self.solver.is_true(v) {
                return Some((sources.to_owned(), constraints));
            }
        }
        return None;
    }
}

fn source_constant_vector<T>(required: Vec<Constant>,
                             forbidden: HashSet<Vec<Constant>>,
                             program: &Program<T>,
                             facts: &FactTable<T>,
                             max_sources: usize)
                             -> Option<(Vec<Source>, Vec<EqualityConstraint>)>
    where T: TruthValue
{
    let mut state = SynthesisState::new();
    state.require_constant_vector(&required,
                                  &forbidden,
                                  program,
                                  facts,
                                  max_sources,
                                  max_sources);
    return state.solve();
}

fn create_clause<T>(program: &Program<T>,
                    output_predicate: Predicate,
                    output_num_terms: usize,
                    sources: &[Source],
                    equality_constraints: &[EqualityConstraint])
                    -> Clause
    where T: TruthValue
{
    let num_output_vars = output_num_terms;
    let mut head = Literal::new_from_vec(output_predicate,
                                         (0..num_output_vars).map(|t| Term::Variable(t)).collect());
    let mut back_refs = HashMap::new();
    for EqualityConstraint(first, second) in equality_constraints.iter().cloned() {
        back_refs.insert(second, first);
    }
    println!("back_refs = {:?}", back_refs);

    let mut num_variables_so_far = sources.len();
    let mut body: Vec<Literal> = Vec::new();
    for (source_idx, source) in sources.iter().cloned().enumerate() {
        let terms = Vec::with_capacity(program.get_num_terms(source.predicate));
        body.push(Literal::new_from_vec(source.predicate, terms));
        for term_idx in 0..program.get_num_terms(source.predicate) {
            if term_idx == source.term_idx {
                if let Some(back_ref) = back_refs.get(&SourceTerm {
                                                           source_idx,
                                                           term_idx,
                                                       }) {
                    let referred_to = body[back_ref.source_idx].terms[back_ref.term_idx];
                    head.terms[source_idx] = referred_to;
                } else {
                    body.last_mut()
                        .unwrap()
                        .terms
                        .push(Term::Variable(source_idx));
                }

            } else if let Some(back_ref) = back_refs.get(&SourceTerm {
                                                              source_idx,
                                                              term_idx,
                                                          }) {
                let referred_to = body[back_ref.source_idx].terms[back_ref.term_idx];
                body.last_mut().unwrap().terms.push(referred_to);
            } else {
                body.last_mut()
                    .unwrap()
                    .terms
                    .push(Term::Variable(num_variables_so_far));
                num_variables_so_far += 1;
            }
        }
    }
    Clause::new_from_vec(head, body)
}

fn synthesize_clause<T>(program: &mut Program<T>,
                        output_predicate: usize,
                        output_num_terms: usize,
                        output_num_sources: usize,
                        io_pairs: &[(FactTable<T>, (Vec<Constant>, Vec<Vec<Constant>>))])
                        -> Clause
    where T: TruthValue
{
    let mut state = SynthesisState::new();
    for &(ref input, ref output) in io_pairs {
        let required = &output.0;
        let forbidden = output.1.iter().cloned().collect();
        state.require_constant_vector(required,
                                      &forbidden,
                                      program,
                                      input,
                                      output_num_sources,
                                      output_num_sources);
    }
    let (sources, constraints) = state.solve().unwrap();

    println!("sources = {:?}", sources);
    println!("constraints = {:?}", constraints);
    let result_clause = create_clause(program,
                                      output_predicate,
                                      output_num_terms,
                                      &sources,
                                      &constraints);
    program.push_clause_simple(result_clause.clone());
    return result_clause;
}

#[cfg(test)]
mod tests {
    use super::*;
    use bottom_up::evaluate_bottom_up;
    use parser::{datalog, facts_and_program, program_lit};

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

    #[test]
    fn constant_vec_iter_simple() {
        let (facts, prg) = facts_and_program(r"
            a(0).
            a(1).
            b(0, 0).
            b(0, 1).
        ");
        let mut iter = ConstantVecIter::new(1, 1, &prg, &facts);
        // Unfortunately, the order of facts is non-deterministic, since FactTable iteration is
        // dependent on HashMap ordering.
        let expected_fact_vec_tuples = vec![(0, 0, vec![vec![0], vec![1]]),
                                            (0, 0, vec![vec![0], vec![1]]),
                                            (1, 0, vec![vec![0, 0], vec![0, 1]]),
                                            (1, 0, vec![vec![0, 0], vec![0, 1]]),
                                            (1, 1, vec![vec![0, 0], vec![0, 1]]),
                                            (1, 1, vec![vec![0, 0], vec![0, 1]])];
        for ((predicate, term_idx, term_choices), (sources, facts)) in
            expected_fact_vec_tuples.into_iter().zip(&mut iter) {
            assert_eq!(vec![Source {
                                predicate,
                                term_idx,
                            }],
                       *sources);
            let fact_choices: HashSet<Fact> = term_choices
                .into_iter()
                .map(|terms| Fact { predicate, terms })
                .collect();
            assert!(fact_choices.contains(&facts[0]));
        }
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn explain_copy() {
        let (facts, prg) = facts_and_program(r"
            a(0).
        ");
        let required = vec![0];
        let forbidden = HashSet::new();
        let (sources, constraints) = source_constant_vector(required, forbidden, &prg, &facts, 1)
            .unwrap();
        assert_eq!(sources,
                   &[Source {
                         predicate: 0,
                         term_idx: 0,
                     }]);
        assert_eq!(constraints, &[]);
    }

    #[test]
    fn choose_correct_predicate() {
        let (facts, prg) = facts_and_program(r"
            a(0).
            b(1).
            c(2).
            d(3).
        ");
        let required = vec![2];
        let forbidden = HashSet::new();
        let (sources, constraints) = source_constant_vector(required, forbidden, &prg, &facts, 1)
            .unwrap();
        assert_eq!(sources,
                   &[Source {
                         predicate: 2,
                         term_idx: 0,
                     }]);
        assert_eq!(constraints, &[]);
    }

    #[test]
    fn require_equality_constraint() {
        let (facts, prg) = facts_and_program(r"
            a(0).
            b(0).
            a(1).
            b(2).
        ");
        let required = vec![0];
        let forbidden = [vec![1], vec![2]].iter().cloned().collect();
        let (sources, constraints) = source_constant_vector(required, forbidden, &prg, &facts, 2)
            .unwrap();
        assert_eq!(sources,
                   &[Source {
                         predicate: 0,
                         term_idx: 0,
                     },
                     Source {
                         predicate: 1,
                         term_idx: 0,
                     }]);
        assert_eq!(constraints,
                   &[EqualityConstraint(SourceTerm {
                                            source_idx: 0,
                                            term_idx: 0,
                                        },
                                        SourceTerm {
                                            source_idx: 1,
                                            term_idx: 0,
                                        })]);

    }

    #[test]
    fn synth_clause_with_equality_constraint() {
        let (facts, mut prg) = facts_and_program(r"
            a(0).
            b(0).
            a(1).
            b(2).
        ");
        let required = vec![0];
        let forbidden = [vec![1], vec![2]].iter().cloned().collect();
        let (sources, constraints) = source_constant_vector(required, forbidden, &prg, &facts, 2)
            .unwrap();
        let result_clause = create_clause(&prg, 2, 1, &sources, &constraints);
        println!("clause = {:?}", result_clause);
        prg.push_clause_simple(result_clause.clone());
        println!("{}", prg);
        // The order of the predicates is determined by the SAT solver randomized starting
        // conditions.
        let expected_clauses =
            [Clause::new_from_vec(Literal::new_from_vec(2, vec![Term::Variable(0)]),
                                  vec![Literal::new_from_vec(0, vec![Term::Variable(0)]),
                                       Literal::new_from_vec(1, vec![Term::Variable(0)])]),
             Clause::new_from_vec(Literal::new_from_vec(2, vec![Term::Variable(0)]),
                                  vec![Literal::new_from_vec(1, vec![Term::Variable(0)]),
                                       Literal::new_from_vec(0, vec![Term::Variable(0)])])];
        assert!(expected_clauses.contains(&result_clause));
    }

    #[test]
    fn synth_piece_down() {
        let (input_0, mut prg) = facts_and_program(r"
            player(0).
            move(1).
            board(0, 2).
            board(1, 2).
        ");

        let output_0 = (vec![1, 0], vec![vec![1, 2], vec![1, 1]]);


        let (input_1, _) = datalog(r"
            player(1).
            move(1).
            board(0, 2).
            board(1, 2).
        ",
                                   &mut prg)
                .unwrap();

        let output_1 = (vec![1, 1], vec![vec![1, 0], vec![1, 2]]);

        let (input_2, _) = datalog(r"
            player(1).
            move(0).
            board(0, 2).
            board(1, 2).
        ",
                                   &mut prg)
                .unwrap();

        let output_2 = (vec![0, 1], vec![vec![0, 0], vec![0, 2]]);

        let (input_3, _) = datalog(r"
            player(0).
            move(0).
            board(0, 2).
            board(1, 2).
        ",
                                   &mut prg)
                .unwrap();

        let output_3 = (vec![0, 0], vec![vec![0, 1], vec![0, 2]]);

        let result_clause = synthesize_clause(&mut prg,
                                              3,
                                              2,
                                              2,
                                              &[(input_0, output_0),
                                                (input_1, output_1),
                                                (input_2, output_2),
                                                (input_3, output_3)]);
        let expected_clause =
            Clause::new_from_vec(Literal::new_from_vec(3,
                                                       vec![Term::Variable(0), Term::Variable(1)]),
                                 vec![Literal::new_from_vec(1, vec![Term::Variable(0)]),
                                      Literal::new_from_vec(0, vec![Term::Variable(1)])]);
        println!("clause = {:?}", result_clause);
        println!("correct clause = {:?}", expected_clause);
        println!("{}", prg);
        assert_eq!(result_clause, expected_clause);
    }

    #[test]
    fn synth_mini_ttt_no_rule_breaking() {
        let (input_0, mut prg) = facts_and_program(r"
            player(0).
            move(1, 1).
            board(0, 0, 2).
            board(0, 1, 2).
            board(1, 0, 2).
            board(1, 1, 2).
            blank(2).
        ");

        let output_0 = (vec![1, 1, 0], vec![vec![1, 1, 1], vec![1, 1, 2]]);


        let (input_1, _) = datalog(r"
            player(1).
            move(0, 1).
            board(0, 0, 2).
            board(0, 1, 2).
            board(1, 0, 2).
            board(1, 1, 2).
            blank(2).
        ",
                                   &mut prg)
                .unwrap();

        let output_1 = (vec![0, 1, 1], vec![vec![0, 1, 0], vec![0, 1, 2]]);

        let (input_2, _) = datalog(r"
            player(1).
            move(0, 0).
            board(0, 0, 2).
            board(0, 1, 2).
            board(1, 0, 2).
            board(1, 1, 2).
            blank(2).
        ",
                                   &mut prg)
                .unwrap();

        let output_2 = (vec![0, 0, 1], vec![vec![0, 0, 0], vec![0, 1, 2]]);

        let (input_3, _) = datalog(r"
            player(0).
            move(0, 1).
            board(0, 0, 2).
            board(0, 1, 2).
            board(1, 0, 2).
            board(1, 1, 2).
            blank(2).
        ",
                                   &mut prg)
                .unwrap();

        let output_3 = (vec![0, 1, 0], vec![vec![0, 1, 1], vec![0, 1, 2]]);

        let result_clause = synthesize_clause(&mut prg,
                                              4,
                                              3,
                                              3,
                                              &[(input_0, output_0),
                                                (input_1, output_1),
                                                (input_2, output_2),
                                                (input_3, output_3)]);
        let expected_clause = Clause::new_from_vec(Literal::new_from_vec(4,
                                                                         vec![Term::Variable(0),
                                                                              Term::Variable(1),
                                                                              Term::Variable(2)]),
                                                   vec![Literal::new_from_vec(1,
                                                            vec![Term::Variable(0),
                                                                 Term::Variable(3)]),
                                      Literal::new_from_vec(1,
                                                            vec![Term::Variable(4),
                                                                 Term::Variable(1)]),
                                      Literal::new_from_vec(0, vec![Term::Variable(2)])]);
        println!("clause = {:?}", result_clause);
        println!("correct clause = {:?}", expected_clause);
        println!("{}", prg);
        assert_eq!(result_clause, expected_clause);
    }
}
