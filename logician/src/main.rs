extern crate rlog;
extern crate clap;

use std::fs;
use clap::{Arg, App};

fn main() {
    use std::io::Read;
    let matches = App::new("logician")
        .version("0.1.0")
        .author("Kyle R. Zentner")
        .about("Datalog implementation.")
        .arg(Arg::with_name("INPUT")
            .help("Sets the input file to use")
            .required(true)
            .index(1))
        .get_matches();
    let input = matches.value_of("INPUT").expect("Need input");
    let mut source = String::new();
    let mut file = fs::File::open(input).expect("Could not open file");
    file.read_to_string(&mut source).expect("Error reading input");
    let mut ctxt = rlog::Context::new_from_source(&source).expect("Error creating context");
    println!("Program:\n{}", ctxt.program_as_string());
    println!("Meaning of program:\n{}", ctxt.facts_as_string());
}
