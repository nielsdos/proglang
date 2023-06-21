use crate::parser::parse;

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod token;

fn main() {
    // TODO: nicer arg parsing
    let filename = std::env::args().nth(1).unwrap().into_boxed_str();
    let src = std::fs::read_to_string(&*filename)
        .unwrap()
        .into_boxed_str();

    parse(filename.into(), src.into());
}
