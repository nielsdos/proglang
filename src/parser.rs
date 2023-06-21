// Based on the sample code from https://github.com/zesterer/chumsky/blob/main/examples/nano_rust.rs

use crate::ast::{Ast, BinaryOperationKind, UnaryOperationKind};
use crate::lexer::lexer;
use crate::token::Token;
use crate::types::{Span, Spanned};
use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::input::SpannedInput;
use chumsky::prelude::*;
use std::rc::Rc;

type ParserInput<'tokens, 'src> = SpannedInput<Token<'src>, Span, &'tokens [Spanned<Token<'src>>]>;

type ParserExtra<'tokens, 'src> = extra::Err<Rich<'tokens, Token<'src>, Span>>;

fn parse_expression<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Ast<'src>>, ParserExtra<'tokens, 'src>>
{
    recursive(|expression| {
        let literal = select! {
            Token::LiteralInt(int) => Ast::LiteralInt(int),
        };

        let identifier = select! {
            Token::Identifier(ident) => Ast::Identifier(ident),
        };

        let atom = literal
            .or(identifier)
            .map_with_span(|ast, span: Span| (ast, span))
            .or(expression
                .clone()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen)));

        let map_binary_operation =
            |lhs: Spanned<Ast<'src>>, (op, rhs): (BinaryOperationKind, Spanned<Ast<'src>>)| {
                let span = lhs.1.start..rhs.1.end;
                (
                    Ast::BinaryOperation(Box::new(lhs), op, Box::new(rhs)),
                    span.into(),
                )
            };

        let unary_expression = recursive(
            |unary_expression: Recursive<
                dyn Parser<
                    'tokens,
                    ParserInput<'tokens, 'src>,
                    Spanned<Ast<'src>>,
                    ParserExtra<'tokens, 'src>,
                >,
            >| {
                let unary_operation = just(Token::Operator('-'))
                    .to(UnaryOperationKind::Minus)
                    .or(just(Token::Operator('+')).to(UnaryOperationKind::Plus))
                    .map_with_span(|kind, span: Span| (kind, span))
                    .then(unary_expression.clone())
                    .map(|(kind, rhs)| {
                        let span = kind.1.start..rhs.1.end;
                        (Ast::UnaryOperation(kind.0, Box::new(rhs)), span.into())
                    });

                let power = atom.clone().foldl(
                    just(Token::DoubleStar)
                        .to(BinaryOperationKind::Power)
                        .then(unary_expression)
                        .repeated(),
                    map_binary_operation,
                );

                unary_operation.or(power)
            },
        );

        let product_or_divide = unary_expression.clone().foldl(
            just(Token::Operator('*'))
                .to(BinaryOperationKind::Product)
                .or(just(Token::Operator('/')).to(BinaryOperationKind::Division))
                .then(unary_expression)
                .repeated(),
            map_binary_operation,
        );

        let addition_or_subtraction = product_or_divide.clone().foldl(
            just(Token::Operator('+'))
                .to(BinaryOperationKind::Addition)
                .or(just(Token::Operator('-')).to(BinaryOperationKind::Subtraction))
                .then(product_or_divide)
                .repeated(),
            map_binary_operation,
        );

        addition_or_subtraction
    })
}

fn parse_statement<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Ast<'src>>, ParserExtra<'tokens, 'src>>
{
    let identifier = select! {
        Token::Identifier(ident) => ident,
    };

    let assignment = identifier
        .then_ignore(just(Token::Operator('=')))
        .then(parse_expression())
        .map_with_span(|(ident, expr), span: Span| (ident, expr, span))
        .map(|(ident, expr, span)| (Ast::Assignment(ident, Box::new(expr)), span.into()));

    assignment
}

fn parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Ast<'src>>, ParserExtra<'tokens, 'src>>
{
    // TODO: zorgt dit dan effectief dat we geen dingen op 1 lijn kunnen hebben?
    parse_statement().then_ignore(just(Token::Newline).repeated())
}

pub fn parse(filename: Rc<str>, input: Rc<str>) {
    // TODO: "a = " crasht de parser
    let (tokens, lexer_errors) = lexer().parse(&*input).into_output_errors();
    let parse_errors = if let Some(tokens) = &tokens {
        let (ast, parse_errors) = parser()
            .parse(tokens.as_slice().spanned((input.len()..input.len()).into()))
            .into_output_errors();
        println!("{:#?}", ast);
        parse_errors
    } else {
        vec![]
    };

    lexer_errors
        .into_iter()
        .map(|e| e.map_token(|character| character.to_string()))
        .chain(
            parse_errors
                .into_iter()
                .map(|e| e.map_token(|token| token.to_string())),
        )
        .for_each(|e| {
            let _ = Report::build(ReportKind::Error, filename.clone(), e.span().start)
                .with_message(e.to_string())
                .with_label(
                    Label::new((filename.clone(), e.span().into_range())).with_color(Color::Red),
                )
                .finish()
                .print(sources([(filename.clone(), input.clone())]));
        });
}
