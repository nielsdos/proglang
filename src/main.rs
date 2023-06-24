use crate::parser::{parse, ParserOptions};
use clap::Parser;
use clap_derive::Parser;

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod token;

#[derive(Parser)]
struct Args {
    #[arg(index = 1, required = true, value_name = "filename")]
    filename: String,

    #[arg(long, help = "Dumps the AST to standard output")]
    dump_ast: bool,

    #[arg(long, help = "Dumps the token tree to standard output")]
    dump_token_tree: bool,
}

fn main() {
    let args = Args::parse();

    let filename = args.filename.into_boxed_str();
    let src = std::fs::read_to_string(&*filename)
        .expect("File must be readable")
        .into_boxed_str();

    let ast = parse(
        filename.into(),
        &src,
        ParserOptions {
            dump_token_tree: args.dump_token_tree,
        },
    );
    if let Some(ast) = ast {
        if args.dump_ast {
            println!("{:#?}", ast);
        }
    }
}
