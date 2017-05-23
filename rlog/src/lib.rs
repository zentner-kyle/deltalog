#[macro_use]
extern crate log;
extern crate unicode_xid;
extern crate rand;

mod bindings;
mod bottom_up;
mod fact_table;
mod fake_rng;
mod generate;
mod mutate;
mod name_table;
mod optimize;
mod parser;
mod program;
mod reconstrain;
mod refine;
mod types;
mod util;
mod selector;
mod uniform_selector;
pub mod truth_value;

use fact_table::FactTable;
use rand::{Rng, XorShiftRng, thread_rng};
use refine::Refiner;

pub struct Context<T>
    where T: truth_value::TruthValue
{
    facts: fact_table::FactTable<T>,
    program: program::Program<T>,
    samples: Vec<(FactTable<T>, FactTable<T>)>,
}

#[derive(Debug)]
pub enum Error<'a> {
    Internal {},
    SourceMsg { msg: &'static str, source: &'a str },
}

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;

const STEP_ITERATIONS: usize = std::usize::MAX;

impl<T> Context<T>
    where T: truth_value::TruthValue
{
    pub fn new_from_source(source: &str) -> Result<Self> {
        parser::program(source)
            .map(|((facts, program, samples), _)| {
                     Context {
                         facts: facts,
                         program: program,
                         samples: samples,
                     }
                 })
            .map_err(|e| match e {
                         parser::Error::Msg { msg, rest } => {
                             Error::SourceMsg {
                                 msg: msg,
                                 source: rest,
                             }
                         }
                     })
    }

    pub fn facts_as_string(&mut self) -> String {
        bottom_up::evaluate_bottom_up(&mut self.facts, &self.program);
        return self.facts.as_datalog(&self.program.predicate_names);
    }

    pub fn program_as_string(&self) -> String {
        use std::fmt::Write;
        let mut output = String::new();
        write!(&mut output, "{}", self.program).unwrap();
        return output;
    }

    pub fn optimize(&mut self, learning_rate: f64, iterations: usize) {
        for _ in 0..iterations {
            let res = optimize::compute_adjustments(&self.program,
                                                    &self.facts,
                                                    &self.samples,
                                                    STEP_ITERATIONS);
            for (clause_idx, adjustment) in res.clause_adjustments.iter().enumerate() {
                T::dual_adjust(&mut self.program.clause_weights[clause_idx],
                               adjustment,
                               learning_rate);
            }
        }
    }

    pub fn refine(&mut self, iterations: usize) {
        let mut refiner = Refiner::new(thread_rng().gen::<XorShiftRng>(),
                                       self.facts.clone(),
                                       self.program.clone(),
                                       self.samples.clone());
        refiner.iterate(iterations);
        self.program = refiner.to_program();
    }
}
