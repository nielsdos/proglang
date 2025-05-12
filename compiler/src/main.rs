extern crate core;

use crate::driver::driver;
use clap::Parser;
use clap_derive::Parser;
use clap_num::number_range;
use std::process::ExitCode;
use std::rc::Rc;

pub mod analysis;
pub mod codegen;
mod driver;
pub mod mid_ir;
pub mod syntax;
pub mod types;
pub mod util;

fn parse_optimization_level(s: &str) -> Result<u32, String> {
    number_range(s, 0, 3)
}

#[derive(Parser)]
struct Args {
    #[arg(required = true, value_name = "filename")]
    filename: String,

    #[arg(long, help = "Dumps the AST to standard output")]
    dump_ast: bool,

    #[arg(long, help = "Dumps the tokens to standard output")]
    dump_tokens: bool,

    #[arg(long, help = "Generate machine-friendly output")]
    machine_friendly_output: bool,

    #[arg(short, long, required = false, default_value = "2", help = "Optimization level", value_parser = parse_optimization_level)]
    optimization_level: u32,
}

fn main() -> ExitCode {
    let args = Args::parse();

    let filename: Rc<str> = args.filename.into();
    let input: Rc<str> = std::fs::read_to_string(&*filename).expect("File must be readable").into();

    driver(filename, input, args.dump_ast, args.dump_tokens, args.machine_friendly_output, args.optimization_level)
}
