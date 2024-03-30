// Based on the sample code from https://github.com/zesterer/chumsky/blob/main/examples/nano_rust.rs

use crate::syntax::ast::{Assignment, Ast, BinaryOperation, BinaryOperationKind, BindingType, Class, ClassField, Declaration, FunctionCall, FunctionDeclaration, Identifier, IfStatement, LiteralBool, LiteralFloat, LiteralInt, ReturnStatement, StatementList, UnaryOperation, UnaryOperationKind};
use crate::syntax::lexer::lexer;
use crate::syntax::span::{Span, Spanned};
use crate::syntax::token::{Token, TokenTree};
use crate::types::function_info::ArgumentInfo;
use crate::types::type_system::{FunctionType, Type};
use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::input::{BoxedStream, SpannedInput, Stream};
use chumsky::prelude::*;
use std::collections::VecDeque;
use std::iter;
use std::rc::Rc;

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

        let atom_no_call = literal
            .or(identifier)
            .map_with(|ast, extra| (ast, extra.span()))
            .or(expression.clone().delimited_by(just(Token::LeftParen), just(Token::RightParen)));

        let call_expression = atom_no_call
            .clone()
            .then(
                expression
                    .clone()
                    .map(|expr: (Ast<'src>, _)| expr)
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            )
            .map(|(identifier, args)| Ast::FunctionCall(FunctionCall { callee: Box::new(identifier), args }));

        let atom = call_expression.map_with(|ast, extra| (ast, extra.span())).or(atom_no_call);

        let map_binary_operation = |lhs: Spanned<Ast<'src>>, (op, rhs): (BinaryOperationKind, Spanned<Ast<'src>>)| {
            let span = lhs.1.start..rhs.1.end;
            (Ast::BinaryOperation(BinaryOperation(Box::new(lhs), op, Box::new(rhs))), span.into())
        };

        let unary_expression = recursive(
            |unary_expression: Recursive<dyn Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Ast<'src>>, ParserExtra<'tokens, 'src>>>| {
                let unary_operation = just(Token::Operator('-'))
                    .to(UnaryOperationKind::Minus)
                    .or(just(Token::Operator('+')).to(UnaryOperationKind::Plus))
                    .map_with(|kind, extra| {
                        let span: Span = extra.span();
                        (kind, span)
                    })
                    .then(unary_expression.clone())
                    .map(|(kind, rhs)| {
                        let span: Span = (kind.1.start..rhs.1.end).into();
                        (Ast::UnaryOperation(UnaryOperation(kind.0, Box::new(rhs))), span)
                    });

                let power = atom
                    .clone()
                    .foldl(just(Token::DoubleStar).to(BinaryOperationKind::Power).then(unary_expression).repeated(), map_binary_operation);

                unary_operation.or(power)
            },
        )
        .boxed();

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
            .ignore_then(parse_expression())
            .then_ignore(just(Token::BlockStart))
            .then(statement_list.clone())
            .then_ignore(just(Token::BlockEnd))
            .then(
                just(Token::Else)
                    .ignore_then(just(Token::BlockStart))
                    .ignore_then(statement_list)
                    .then_ignore(just(Token::BlockEnd))
                    .or_not(),
            )
            .map_with(|((condition, then_statements), else_statements), extra| (condition, then_statements, else_statements, extra.span()))
            .map(|(condition, then_statements, else_statements, span)| {
                (
                    Ast::IfStatement(IfStatement {
                        condition: Box::new(condition),
                        then_statements: Box::new(then_statements),
                        else_statements: else_statements.map(Box::new),
                    }),
                    span,
                )
            });

        let assignment = identifier
            .then_ignore(just(Token::Operator('=')))
            .then(parse_expression())
            .then_ignore(just(Token::StatementEnd))
            .map_with(|(ident, expr), extra| (ident, expr, extra.span()))
            .map(|(ident, expr, span)| (Ast::Assignment(Assignment(ident, Box::new(expr))), span));

        let declaration = choice((just(Token::Let).to(BindingType::ImmutableVariable), just(Token::Mut).to(BindingType::MutableVariable)))
            .then(identifier)
            .then_ignore(just(Token::Operator('=')))
            .then(parse_expression())
            .then_ignore(just(Token::StatementEnd))
            .map_with(|((binding, ident), expr), extra| (binding, ident, expr, extra.span()))
            .map(|(binding, ident, expr, span)| {
                (
                    Ast::Declaration(Declaration {
                        assignment: Assignment(ident, Box::new(expr)),
                        binding,
                    }),
                    span,
                )
            });

        let return_ = just(Token::Return)
            .ignore_then(parse_expression().or_not())
            .map_with(|expression, extra| (expression, extra.span()))
            .then_ignore(just(Token::StatementEnd))
            .map(|(expression, span)| (Ast::ReturnStatement(ReturnStatement { value: expression.map(Box::new) }), span));

        let expression_statement = parse_expression().then_ignore(just(Token::StatementEnd));

        let statement = choice((declaration, assignment, if_check, return_, expression_statement));

        statement.repeated().at_least(1).collect::<Vec<_>>().map(|list| {
            let span = compute_span_over_slice(&list);
            (Ast::StatementList(StatementList(list)), span)
        })
    })
}

fn parse_type<'tokens, 'src: 'tokens>() -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Type, ParserExtra<'tokens, 'src>> {
    recursive(|parse_type| {
        let predefined_ty_name = select! {
            Token::Identifier("float") => Type::Double,
            Token::Identifier("int") => Type::Int,
            Token::Identifier("bool") => Type::Bool,
        };

        let function_type = just(Token::Fn)
            .ignore_then(
                parse_type
                    .clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<Type>>()
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            )
            .then(just(Token::Arrow).ignore_then(parse_type).or_not())
            .map(|(arg_types, return_type)| {
                Type::Function(Rc::new(FunctionType {
                    arg_types,
                    return_type: return_type.unwrap_or(Type::Void),
                }))
            });

        predefined_ty_name.or(function_type)
    })
}

fn parse_declarations<'tokens, 'src: 'tokens>() -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Ast<'src>>, ParserExtra<'tokens, 'src>> {
    let identifier = select! {
        Token::Identifier(ident) => ident,
    };

    let return_type = just(Token::Arrow).ignore_then(parse_type());

    let function_declaration = just(Token::Fn)
        .map_with(|_, extra| extra.span())
        .then(identifier)
        .then_ignore(just(Token::LeftParen))
        .then(
            just(Token::Mut)
                .to(BindingType::MutableVariable)
                .or_not()
                .map(|mutability| mutability.unwrap_or(BindingType::ImmutableVariable))
                .then(parse_type())
                .then(identifier)
                .map_with(|((binding, ty), ident), extra| (ArgumentInfo::new(ident, ty, binding), extra.span()))
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::RightParen))
        .then(return_type.or_not())
        .then_ignore(just(Token::BlockStart))
        .then(parse_statement_list())
        .then_ignore(just(Token::BlockEnd))
        .map(move |((((fn_span, fn_name), args), return_type), statements)| {
            let span = fn_span.start..statements.1.end;
            let declaration = FunctionDeclaration {
                name: fn_name,
                statements: Box::new(statements),
                return_type: return_type.unwrap_or(Type::Void),
                args,
            };
            (Ast::FunctionDeclaration(declaration), span.into())
        });

    let class_field_declarations = parse_type()
        .then(identifier)
        .then_ignore(just(Token::StatementEnd))
        .map_with(|(ty, name), extra| {
            let class_field = ClassField { name, ty };
            (class_field, extra.span())
        })
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>();

    let class_declaration = just(Token::Class)
        .ignore_then(identifier)
        .then_ignore(just(Token::BlockStart))
        .then(class_field_declarations)
        .map_with(|data, extra| (data, extra.span()))
        .then_ignore(just(Token::BlockEnd))
        .map(|((name, fields), span)| {
            (Ast::Class(Class { name, fields }), span)
        });

    function_declaration.or(class_declaration)
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
        let mut queue = VecDeque::from_iter(token_tree);
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
