extern crate core;

use crate::analysis::semantic_analysis::SemanticAnalyser;
use crate::codegen::codegen_driver::CodeGen;
use crate::codegen::codegen_llvm::CodeGenContext;
use crate::mid_ir::construction::Construction;
use crate::syntax::parser::{parse, ParserOptions};
use ariadne::{sources, Color, Label, Report, ReportKind};
use clap::Parser;
use clap_derive::Parser;
use clap_num::number_range;
use std::process::ExitCode;
use std::rc::Rc;

pub mod analysis;
pub mod codegen;
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

    let ast = parse(
        filename.clone(),
        &input,
        ParserOptions {
            dump_tokens: args.dump_tokens,
            machine_friendly_output: args.machine_friendly_output,
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
                if let Some(note) = error.note() {
                    eprintln!("Note: {}:{:?}: {}", filename, note.span(), note.error_text());
                }
                eprintln!("{}:{:?}: {}", filename, error.span(), error.error_text());
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
            let mut mid_functions = Vec::new();
            for func in semantic_analyser.function_list_iter() {
                let construction = Construction::new(&semantic_analyser, func);
                let ir = construction.construct_from_function_declaration();
                println!("{:#?}", ir);
                mid_functions.push(ir);
            }

            // TODO: would be great to free up memory if we could drop semantic_analyser before full-on codegen
            let codegen_context = CodeGenContext::default();
            let mut codegen = CodeGen::new(&codegen_context, &mid_functions, args.optimization_level);
            println!();
            codegen.codegen_program(semantic_analyser.class_map());
            codegen.dump();
            ExitCode::from(0)
        } else {
            ExitCode::from(1)
        }
    } else {
        ExitCode::from(1)
    }
}
