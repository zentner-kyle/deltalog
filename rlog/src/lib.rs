#[macro_use]
extern crate log;
extern crate unicode_xid;

mod bottom_up;
mod db;
mod matching;
mod name_table;
mod parser;
mod program;
mod types;
mod fact_table;

pub struct Context {
    evaluator: bottom_up::BottomUpEvaluator,
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

impl Context {
    pub fn new_from_source(source: &str) -> Result<Self> {
        parser::db(source)
            .map(|(db, _)| Context {
                evaluator: bottom_up::BottomUpEvaluator::new(db),
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
        self.evaluator.run();
        return self.evaluator.db.facts_as_string();
    }
}
