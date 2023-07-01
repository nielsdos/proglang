// Based on the sample code from https://github.com/zesterer/chumsky/blob/main/examples/nano_rust.rs

use crate::syntax::ast::{
    Assignment, Ast, BinaryOperation, BinaryOperationKind, FunctionDeclaration, Identifier, IfStatement, LiteralBool, LiteralFloat, LiteralInt, ReturnStatement, StatementList, UnaryOperation,
    UnaryOperationKind,
};
use crate::syntax::lexer::lexer;
use crate::syntax::span::{Span, Spanned};
use crate::syntax::token::{Token, TokenTree};
use crate::types::type_system::Type;
use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::input::{BoxedStream, SpannedInput, Stream};
use chumsky::prelude::*;
use std::collections::VecDeque;
use std::iter;
use std::rc::Rc;
use crate::types::function_info::ArgumentInfo;

pub struct ParserOptions {
    pub dump_token_tree: bool,
}

type ParserInput<'tokens, 'src> = SpannedInput<Token<'src>, Span, BoxedStream<'tokens, Spanned<Token<'src>>>>;

type ParserExtra<'tokens, 'src> = extra::Err<Rich<'tokens, Token<'src>, Span>>;

fn compute_span_over_slice(slice: &[Spanned<Ast>]) -> Span {
    if slice.is_empty() {
        SimpleSpan::new(0, 0)
    } else {
        (slice[0].1.start..slice[slice.len() - 1].1.end).into()
    }
}

fn parse_expression<'tokens, 'src: 'tokens>() -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Ast<'src>>, ParserExtra<'tokens, 'src>> + Clone {
    recursive(|expression| {
        let literal = select! {
            Token::LiteralInt(int) => Ast::LiteralInt(LiteralInt(int)),
            Token::LiteralFloat(dbl) => Ast::LiteralFloat(LiteralFloat(dbl)),
            Token::LiteralBool(dbl) => Ast::LiteralBool(LiteralBool(dbl)),
        };

        let identifier = select! {
            Token::Identifier(ident) => Ast::Identifier(Identifier(ident)),
        };

        let atom = literal
            .or(identifier)
            .map_with_span(|ast, span: Span| (ast, span))
            .or(expression.clone().delimited_by(just(Token::LeftParen), just(Token::RightParen)));

        let map_binary_operation = |lhs: Spanned<Ast<'src>>, (op, rhs): (BinaryOperationKind, Spanned<Ast<'src>>)| {
            let span = lhs.1.start..rhs.1.end;
            (Ast::BinaryOperation(BinaryOperation(Box::new(lhs), op, Box::new(rhs))), span.into())
        };

        let unary_expression = recursive(
            |unary_expression: Recursive<dyn Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Ast<'src>>, ParserExtra<'tokens, 'src>>>| {
                let unary_operation = just(Token::Operator('-'))
                    .to(UnaryOperationKind::Minus)
                    .or(just(Token::Operator('+')).to(UnaryOperationKind::Plus))
                    .map_with_span(|kind, span: Span| (kind, span))
                    .then(unary_expression.clone())
                    .map(|(kind, rhs)| {
                        let span = kind.1.start..rhs.1.end;
                        (Ast::UnaryOperation(UnaryOperation(kind.0, Box::new(rhs))), span.into())
                    });

                let power = atom
                    .clone()
                    .foldl(just(Token::DoubleStar).to(BinaryOperationKind::Power).then(unary_expression).repeated(), map_binary_operation);

                unary_operation.or(power)
            },
        );

        let product_or_divide = unary_expression.clone().foldl(
            choice((
                just(Token::Operator('*')).to(BinaryOperationKind::Product),
                just(Token::Operator('/')).to(BinaryOperationKind::DoubleDivision),
                just(Token::DoubleSlash).to(BinaryOperationKind::WholeDivision),
            ))
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

        let comparison_operation = addition_or_subtraction.clone().foldl(
            choice((
                just(Token::Operator('<')).to(BinaryOperationKind::LessThan),
                just(Token::Operator('>')).to(BinaryOperationKind::GreaterThan),
                just(Token::DoubleEqual).to(BinaryOperationKind::Equal),
                just(Token::NotEqual).to(BinaryOperationKind::NotEqual),
                just(Token::LessThanEqual).to(BinaryOperationKind::LessThanEqual),
                just(Token::GreaterThanEqual).to(BinaryOperationKind::GreaterThanEqual),
            ))
            .then(addition_or_subtraction)
            .repeated(),
            map_binary_operation,
        );

        comparison_operation
    })
}

fn parse_statement_list<'tokens, 'src: 'tokens>() -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Ast<'src>>, ParserExtra<'tokens, 'src>> {
    recursive(|statement_list| {
        let identifier = select! {
            Token::Identifier(ident) => ident,
        };

        let if_check = just(Token::If)
            .ignored()
            .then(parse_expression())
            .then_ignore(just(Token::BlockStart))
            .then(statement_list)
            .then_ignore(just(Token::BlockEnd))
            .map_with_span(|((_, condition), statements), span: Span| (condition, statements, span))
            .map(|(condition, statements, span)| {
                (
                    Ast::IfStatement(IfStatement {
                        condition: Box::new(condition),
                        statements: Box::new(statements),
                    }),
                    span,
                )
            });

        let assignment = identifier
            .then_ignore(just(Token::Operator('=')))
            .then(parse_expression())
            .then_ignore(just(Token::StatementEnd))
            .map_with_span(|(ident, expr), span: Span| (ident, expr, span))
            .map(|(ident, expr, span)| (Ast::Assignment(Assignment(ident, Box::new(expr))), span));

        let return_ = just(Token::Return)
            .ignore_then(parse_expression().or_not())
            .map_with_span(|expression, span| (expression, span))
            .then_ignore(just(Token::StatementEnd))
            .map(|(expression, span)| (Ast::ReturnStatement(ReturnStatement { value: expression.map(Box::new) }), span));

        let statement = choice((assignment, if_check, return_));

        statement.repeated().at_least(1).collect::<Vec<_>>().map(|list| {
            let span = compute_span_over_slice(&list);
            (Ast::StatementList(StatementList(list)), span)
        })
    })
}

fn parse_type<'tokens, 'src: 'tokens>() -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Type, ParserExtra<'tokens, 'src>> {
    let ty_name = select! {
        Token::Identifier("float") => Type::Double,
        Token::Identifier("int") => Type::Int,
        Token::Identifier("bool") => Type::Bool,
    };
    ty_name //.map_with_span(|a, b| (a, b))
}

fn parse_declarations<'tokens, 'src: 'tokens>() -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Ast<'src>>, ParserExtra<'tokens, 'src>> {
    let identifier = select! {
        Token::Identifier(ident) => ident,
    };

    let return_type = just(Token::Arrow).ignore_then(parse_type());

    just(Token::Fn)
        .map_with_span(|_, span: Span| span)
        .then(identifier)
        .then_ignore(just(Token::LeftParen))
        .then(
            parse_type()
                .then(identifier)
                .map(|(ty, ident)| ArgumentInfo::new(ident, ty))
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::RightParen))
        .then(return_type.or_not())
        .then_ignore(just(Token::BlockStart))
        .then(parse_statement_list())
        .then_ignore(just(Token::BlockEnd))
        .map(|((((fn_span, fn_name), args), return_type), statements)| {
            let span = fn_span.start..statements.1.end;
            let declaration = FunctionDeclaration {
                name: fn_name,
                statements: Box::new(statements),
                return_type: return_type.unwrap_or(Type::Void),
                args,
            };
            (Ast::FunctionDeclaration(declaration), span.into())
        })
}

fn parser<'tokens, 'src: 'tokens>() -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Ast<'src>>, ParserExtra<'tokens, 'src>> {
    parse_declarations().repeated().collect::<Vec<_>>().map(|declarations| {
        let span = compute_span_over_slice(&declarations);
        (Ast::StatementList(StatementList(declarations)), span)
    })
}

pub fn parse(filename: Rc<str>, input: &str, options: ParserOptions) -> Option<Spanned<Ast>> {
    let (token_tree, lexer_errors) = lexer().parse(input).into_output_errors();

    if options.dump_token_tree {
        println!("{:#?}", token_tree);
    }

    let (parse_errors, ast) = if let Some(token_tree) = token_tree {
        // Convert token tree into token stream.
        let mut queue = VecDeque::from_iter(token_tree.into_iter());
        let iterator = iter::from_fn(move || loop {
            let token_tree = queue.pop_front()?;
            match token_tree {
                TokenTree::Leaf(token) => break Some(token),
                TokenTree::Tree(tree) => {
                    let span: Span = (0..0).into(); // TODO
                    queue.push_front(TokenTree::Leaf((Token::BlockEnd, span)));
                    for entry in tree.into_iter().rev() {
                        queue.push_front(entry);
                    }
                    queue.push_front(TokenTree::Leaf((Token::BlockStart, span)));
                }
            }
        });
        let token_stream = Stream::from_iter(Box::new(iterator)).boxed();
        // Feed converted token stream into parser
        let (ast, parse_errors) = parser().parse(token_stream.spanned((input.len()..input.len()).into())).into_output_errors();
        (parse_errors, ast)
    } else {
        (vec![], None)
    };

    lexer_errors
        .into_iter()
        .map(|e| e.map_token(|character| character.to_string()))
        .chain(parse_errors.into_iter().map(|e| e.map_token(|token| token.to_string())))
        .for_each(|e| {
            let _ = Report::build(ReportKind::Error, filename.clone(), e.span().start)
                .with_message(e.to_string())
                .with_label(Label::new((filename.clone(), e.span().into_range())).with_color(Color::Red))
                .finish()
                .print(sources([(filename.clone(), input)]));
        });

    ast
}
