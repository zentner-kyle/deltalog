#[macro_use]
extern crate log;
extern crate unicode_xid;
extern crate rand;

mod bottom_up;
mod bindings;
mod name_table;
mod parser;
mod program;
mod types;
mod fact_table;
mod optimize;
pub mod generate;
pub mod refine;
pub mod truth_value;

use fact_table::{FactTable};

pub struct Context<T> where T: truth_value::TruthValue {
    facts: fact_table::FactTable<T>,
    program: program::Program<T>,
    samples: Vec<(FactTable<T>, FactTable<T>)>,
}

#[derive(Debug)]
pub enum Error<'a> {
    Internal {
    },
    SourceMsg {
        msg: &'static str,
        source: &'a str,
    },
}

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;

const STEP_ITERATIONS: usize = 1000000000usize;

impl<T> Context<T> where T: truth_value::TruthValue {
    pub fn new_from_source(source: &str) -> Result<Self> {
        parser::program(source)
            .map(|((facts, program, samples), _)| Context {
                facts: facts,
                program: program,
                samples: samples,
            })
            .map_err(|e| {
                match e {
                    parser::Error::Msg{msg, rest} => {
                        Error::SourceMsg{
                            msg: msg,
                            source: rest,
                        }
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
            let res = optimize::compute_adjustments(&self.program, &self.facts, &self.samples, STEP_ITERATIONS);
            for (clause_idx, adjustment) in res.clause_adjustments.iter().enumerate() {
                T::dual_adjust(&mut self.program.clause_weights[clause_idx], adjustment, learning_rate);
            }
        }
    }
}
