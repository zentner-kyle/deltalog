#[macro_use]
extern crate log;
extern crate unicode_xid;

mod bottom_up;
mod bindings;
mod name_table;
mod parser;
mod program;
mod types;
mod fact_table;
pub mod truth_value;

pub struct Context<T> where T: truth_value::TruthValue {
    facts: fact_table::FactTable<T>,
    program: program::Program<T>,
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

impl<T> Context<T> where T: truth_value::TruthValue {
    pub fn new_from_source(source: &str) -> Result<Self> {
        parser::program(source)
            .map(|((facts, program), _)| Context {
                facts: facts,
                program: program,
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
}
