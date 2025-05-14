// Based on the sample code from https://github.com/zesterer/chumsky/blob/main/examples/nano_rust.rs

use crate::syntax::ast::{
    Assignment, Ast, BinaryOperation, BinaryOperationKind, BindingType, Declaration, FunctionCall, FunctionCallArg, FunctionDeclaration, Identifier, IfStatement, LiteralBool, LiteralFloat,
    LiteralInt, MemberAccess, ReturnStatement, StatementList, TableConstructor, TableField, UnaryOperation, UnaryOperationKind, WhileLoop,
};
use crate::syntax::span::compute_span_over_slice;
use crate::syntax::span::{Span, Spanned};
use crate::syntax::token::Token;
use crate::types::function_info::ArgumentInfo;
use crate::types::type_system::{FunctionType, Type};
use chumsky::input::ValueInput;
use chumsky::prelude::*;
use std::rc::Rc;

type ParserExtra<'src> = extra::Err<Rich<'src, Token<'src>, Span>>;

fn parse_expression<'src, I>() -> impl Parser<'src, I, Spanned<Ast<'src>>, ParserExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    recursive(|expression| {
        let literal = select! {
            Token::LiteralInt(int) => Ast::LiteralInt(LiteralInt(int)),
            Token::LiteralFloat(dbl) => Ast::LiteralFloat(LiteralFloat(dbl)),
            Token::LiteralBool(dbl) => Ast::LiteralBool(LiteralBool(dbl)),
        };

        let identifier_raw = select! {
            Token::Identifier(ident) => Identifier(ident),
        };

        let identifier = identifier_raw.map(Ast::Identifier);

        // TODO: also support [] notation and optional naming
        let table_field = identifier_raw
            .map_with(|identifier, extra| (identifier, extra.span()))
            .then_ignore(just(Token::Operator('=')))
            .then(expression.clone())
            .map(|(name, initializer)| TableField {
                name,
                initializer: Box::new(initializer),
            });

        let table_constructor = just(Token::LeftBrace)
            .ignore_then(table_field.separated_by(just(Token::Comma)).allow_trailing().collect::<Vec<_>>())
            .then_ignore(just(Token::RightBrace))
            .map(|fields| Ast::TableConstructor(TableConstructor { fields }));

        let atom_no_call = literal
            .or(identifier)
            .or(table_constructor)
            .map_with(|ast, extra| (ast, extra.span()))
            .or(expression.clone().delimited_by(just(Token::LeftParen), just(Token::RightParen)));

        let call_expression = atom_no_call
            .clone()
            .then(
                // (optional) Named argument part
                identifier_raw
                    .map_with(|identifier, extra| (identifier, extra.span()))
                    .then_ignore(just(Token::Operator('=')))
                    .or_not()
                    // Argument value part
                    .then(expression.clone().map(|expr: (Ast<'src>, _)| expr))
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            )
            .map(|(callee, args): (Spanned<Ast<'src>>, Vec<(Option<Spanned<Identifier<'src>>>, Spanned<Ast<'src>>)>)| {
                let args = args.into_iter().map(|(name, value)| FunctionCallArg { name, value }).collect::<Vec<_>>();
                Ast::FunctionCall(FunctionCall { callee: Box::new(callee), args })
            });

        let atom = call_expression.map_with(|ast, extra| (ast, extra.span())).or(atom_no_call);

        let member_access = atom.clone().foldl(
            just(Token::Dot).ignore_then(identifier_raw.map_with(|ident, extra| (ident, extra.span()))).repeated(),
            |lhs: Spanned<Ast<'src>>, rhs: Spanned<Identifier<'src>>| {
                let span = lhs.1.start..rhs.1.end;
                (Ast::MemberAccess(MemberAccess { lhs: Box::new(lhs), rhs }), span.into())
            },
        );
        /*let member_access = atom.clone().then_ignore(
        just(Token::Dot)).then(identifier_raw.map_with(|ident, extra| (ident, extra.span())))
        .map(|(lhs, rhs): (Spanned<Ast<'src>>, Spanned<Identifier<'src>>)| {
            let span = lhs.1.start..rhs.1.end;
            (Ast::MemberAccess(MemberAccess { lhs: Box::new(lhs), rhs }), span.into())
        });*/

        // TODO: call should probably be moved to this?
        let primary = member_access.or(atom);

        let map_binary_operation = |lhs: Spanned<Ast<'src>>, (op, rhs): (BinaryOperationKind, Spanned<Ast<'src>>)| {
            let span = lhs.1.start..rhs.1.end;
            (Ast::BinaryOperation(BinaryOperation(Box::new(lhs), op, Box::new(rhs))), span.into())
        };

        let unary_expression = recursive(|unary_expression: Recursive<dyn Parser<'src, I, Spanned<Ast<'src>>, ParserExtra<'src>>>| {
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

            let power = primary
                .clone()
                .foldl(just(Token::DoubleStar).to(BinaryOperationKind::Power).then(unary_expression).repeated(), map_binary_operation);

            unary_operation.or(power)
        })
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

fn parse_statement_list<'src, I>() -> impl Parser<'src, I, Spanned<Ast<'src>>, ParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    recursive(|statement_list| {
        let identifier = select! {
            Token::Identifier(ident) => ident,
        };

        let if_check = just(Token::If)
            .ignore_then(parse_expression())
            .then_ignore(just(Token::Then))
            .then(statement_list.clone())
            .then_ignore(just(Token::End))
            .then(just(Token::Else).ignore_then(statement_list.clone()).then_ignore(just(Token::End)).or_not())
            .map_with(|((condition, then_statements), else_statements), extra| {
                (
                    Ast::IfStatement(IfStatement {
                        condition: Box::new(condition),
                        then_statements: Box::new(then_statements),
                        else_statements: else_statements.map(Box::new),
                    }),
                    extra.span(),
                )
            });

        let while_loop = just(Token::While)
            .ignore_then(parse_expression())
            .then_ignore(just(Token::Do))
            .then(statement_list.clone())
            .then_ignore(just(Token::End))
            .map_with(|(condition, body_statements), extra| {
                (
                    Ast::WhileLoop(WhileLoop {
                        condition: Box::new(condition),
                        body_statements: Box::new(body_statements),
                        check_condition_first: true,
                    }),
                    extra.span(),
                )
            });

        let do_while_loop = just(Token::Do)
            .ignore_then(statement_list.clone())
            .then_ignore(just(Token::While))
            .then(parse_expression())
            .map_with(|(body_statements, condition), extra| {
                (
                    Ast::WhileLoop(WhileLoop {
                        condition: Box::new(condition),
                        body_statements: Box::new(body_statements),
                        check_condition_first: false,
                    }),
                    extra.span(),
                )
            });

        let infinite_loop = just(Token::Loop).ignore_then(statement_list).then_ignore(just(Token::End)).map_with(|body_statements, extra| {
            (
                Ast::WhileLoop(WhileLoop {
                    condition: Box::new((Ast::LiteralBool(LiteralBool(true)), (0..0).into())),
                    body_statements: Box::new(body_statements),
                    check_condition_first: true,
                }),
                extra.span(),
            )
        });

        // TODO: left-factoring
        let member_expr = parse_expression()
            .then_ignore(just(Token::Dot))
            .then(identifier.map_with(|ident, extra| (Identifier(ident), extra.span())))
            .map_with(|(base_expr, identifier), extra| {
                println!("{:?}", base_expr);
                (
                    Ast::MemberAccess(MemberAccess {
                        lhs: Box::new(base_expr),
                        rhs: identifier,
                    }),
                    extra.span(),
                )
            });

        let assignment = member_expr
            .or(identifier.map_with(|ident, extra| (Ast::Identifier(Identifier(ident)), extra.span())))
            .then_ignore(just(Token::Operator('=')))
            .then(parse_expression())
            .map_with(|(base, expr), extra| match base {
                (Ast::Identifier(Identifier(ident)), ident_span) => (Ast::Assignment(Assignment((ident, ident_span), Box::new(expr))), extra.span()),
                _ => todo!(),
            });

        let declaration = choice((just(Token::Let).to(BindingType::ImmutableVariable), just(Token::Mut).to(BindingType::MutableVariable)))
            .then(identifier.map_with(|ident, extra| (ident, extra.span())))
            .then_ignore(just(Token::Operator('=')))
            .then(parse_expression())
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
            .map_with(|expression, extra| (Ast::ReturnStatement(ReturnStatement { value: expression.map(Box::new) }), extra.span()));

        let expression_statement = parse_expression();

        let statement = choice((declaration, assignment, while_loop, do_while_loop, infinite_loop, if_check, return_, expression_statement));

        statement.repeated().at_least(1).collect::<Vec<_>>().map(|list| {
            let span = compute_span_over_slice(&list);
            (Ast::StatementList(StatementList(list)), span)
        })
    })
}

fn parse_type<'src, I>() -> impl Parser<'src, I, Type<'src>, ParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    recursive(|parse_type| {
        let predefined_ty_name = select! {
            Token::Identifier("float") => Type::Double,
            Token::Identifier("int") => Type::Int,
            Token::Identifier("bool") => Type::Bool,
            Token::Identifier("table") => Type::Table,
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
            .then(just(Token::Colon).ignore_then(parse_type).or_not())
            .map(|(arg_types, return_type)| {
                Type::Function(Rc::new(FunctionType {
                    arg_types,
                    return_type: return_type.unwrap_or(Type::Void),
                }))
            });

        choice((predefined_ty_name, function_type))
    })
}

fn parse_declarations<'src, I>() -> impl Parser<'src, I, Spanned<Ast<'src>>, ParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    let identifier = select! {
        Token::Identifier(ident) => ident,
    };

    let return_type = just(Token::Colon).ignore_then(parse_type());

    let function_declaration = just(Token::Fn)
        .map_with(|_, extra| -> Span { extra.span() })
        .then(identifier)
        .then_ignore(just(Token::LeftParen))
        .then(
            just(Token::Mut)
                // Argument type and name
                .to(BindingType::MutableVariable)
                .or_not()
                .map(|mutability| mutability.unwrap_or(BindingType::ImmutableVariable))
                .then(parse_type())
                .then(identifier)
                // Default value
                .then(just(Token::Operator('=')).ignore_then(parse_expression()).or_not())
                .map_with(|(((binding, ty), ident), default_value), extra| (ArgumentInfo::new(ident, ty, binding, default_value), extra.span()))
                // Multiple of them
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::RightParen))
        .then(return_type.map_with(|ty, extra| (ty, extra.span())).or_not())
        .then(parse_statement_list())
        .then_ignore(just(Token::End))
        .map(move |((((fn_span, fn_name), args), return_type), statements)| {
            let span = fn_span.start..statements.1.end;
            let declaration = FunctionDeclaration {
                name: fn_name,
                statements: Box::new(statements),
                return_type: return_type.unwrap_or((Type::Void, (0..0).into())),
                args,
            };
            (Ast::FunctionDeclaration(declaration), span.into())
        });

    function_declaration
}

pub fn parser<'src, I>() -> impl Parser<'src, I, Spanned<Ast<'src>>, ParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    parse_declarations().repeated().collect::<Vec<_>>().map(|declarations| {
        let span = compute_span_over_slice(&declarations);
        (Ast::StatementList(StatementList(declarations)), span)
    })
}
