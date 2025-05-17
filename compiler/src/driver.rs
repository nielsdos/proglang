use crate::analysis::semantic_analysis::SemanticAnalyser;
use crate::codegen::codegen_driver::CodeGen;
use crate::codegen::codegen_llvm::CodeGenContext;
use crate::mid_ir::construction::Construction;
use crate::syntax::lexer::lexer;
use crate::syntax::parser::parser;
use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::*;
use std::process::ExitCode;
use std::rc::Rc;

pub fn driver(filename: Rc<str>, input: Rc<str>, dump_tokens: bool, dump_ast: bool, machine_friendly_output: bool, optimization_level: u32) -> ExitCode {
    let (token_stream, lexer_errors) = lexer().parse(&*input).into_output_errors();

    if dump_tokens {
        println!("{:#?}", token_stream);
    }

    let (ast, parse_errors) = if let Some(token_stream) = &token_stream {
        parser().parse(token_stream.as_slice().map((input.len()..input.len()).into(), |(t, s)| (t, s))).into_output_errors()
    } else {
        (None, vec![])
    };

    lexer_errors
        .into_iter()
        .map(|e| e.map_token(|character| character.to_string()))
        .chain(parse_errors.into_iter().map(|e| e.map_token(|token| token.to_string())))
        .for_each(|e| {
            if machine_friendly_output {
                eprintln!("{}:{:?}: {}", filename, e.span(), e);
            } else {
                Report::build(ReportKind::Error, filename.clone(), e.span().start)
                    .with_message(e.to_string())
                    .with_label(Label::new((filename.clone(), e.span().into_range())).with_color(Color::Red))
                    .finish()
                    .eprint(sources([(filename.clone(), input.clone())]))
                    .expect("should be able to write to stderr")
            }
        });

    if let Some(ast) = ast {
        if dump_ast {
            println!("{:#?}", ast);
        }

        let mut semantic_analyser = SemanticAnalyser::new(&ast);
        semantic_analyser.analyse();

        if machine_friendly_output {
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
                //println!("{:#?}", ir);
                mid_functions.push(ir);
            }

            // TODO: would be great to free up memory if we could drop semantic_analyser before full-on codegen
            let codegen_context = CodeGenContext::default();
            let mut codegen = CodeGen::new(&codegen_context, &mid_functions, optimization_level);
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
