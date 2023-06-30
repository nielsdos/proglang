use crate::analysis::semantic_analysis::SemanticAnalyser;
use crate::codegen::CodeGen;
use crate::codegen_llvm::CodeGenContext;
use crate::parser::{parse, ParserOptions};
use ariadne::{sources, Color, Label, Report, ReportKind};
use clap::Parser;
use clap_derive::Parser;
use std::process::ExitCode;
use std::rc::Rc;
use clap_num::number_range;

pub mod analysis;
pub mod ast;
pub mod codegen;
pub mod codegen_llvm;
pub mod function_info;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod token;
pub mod type_system;

fn parse_optimization_level(s: &str) -> Result<u32, String> {
    number_range(s, 0, 3)
}

#[derive(Parser)]
struct Args {
    #[arg(required = true, value_name = "filename")]
    filename: String,

    #[arg(long, help = "Dumps the AST to standard output")]
    dump_ast: bool,

    #[arg(long, help = "Dumps the token tree to standard output")]
    dump_token_tree: bool,

    #[arg(long, help = "Generate machine-friendly output")]
    machine_friendly_output: bool,

    #[arg(short, long, required = false, default_value = "2", help = "Optimization level", value_parser = parse_optimization_level)]
    optimization_level: u32,
}

fn main() -> ExitCode {
    let args = Args::parse();

    let filename: Rc<str> = args.filename.into();
    let input: Rc<str> = std::fs::read_to_string(&*filename).expect("File must be readable").into();

    let ast = parse(
        filename.clone(),
        &input,
        ParserOptions {
            dump_token_tree: args.dump_token_tree,
        },
    );
    if let Some(ast) = ast {
        if args.dump_ast {
            println!("{:#?}", ast);
        }

        let mut semantic_analyser = SemanticAnalyser::new(&ast);
        semantic_analyser.analyse();

        if args.machine_friendly_output {
            for error in semantic_analyser.errors() {
                println!("{}:{:?}: {}", filename, error.span(), error.error_text());
                if let Some(note) = error.note() {
                    println!("Note: {}:{:?}: {}", filename, note.span(), note.error_text());
                }
            }
        } else {
            for error in semantic_analyser.errors() {
                let report = Report::build(ReportKind::Error, filename.clone(), error.span().start)
                    .with_label(Label::new((filename.clone(), error.span().into_range())).with_message(error.error_text()).with_color(Color::Red));
                let report = if let Some(note) = error.note() {
                    report.with_label(Label::new((filename.clone(), note.span().into_range())).with_message(note.error_text()).with_color(Color::Yellow))
                } else {
                    report
                };
                let _ = report.finish().print(sources([(filename.clone(), input.clone())]));
            }
        }

        if semantic_analyser.errors().is_empty() {
            let codegen_context = CodeGenContext::default();
            let mut codegen = CodeGen::new(&semantic_analyser, &codegen_context, args.optimization_level);
            println!();
            codegen.codegen_program();
            codegen.dump();
            ExitCode::from(0)
        } else {
            ExitCode::from(1)
        }
    } else {
        ExitCode::from(1)
    }
}
