// Based on the sample code from https://github.com/zesterer/chumsky/blob/main/examples/nano_rust.rs

use crate::syntax::ast::{
    Ast, BinaryOperation, BinaryOperationKind, BindingType, ComplexAssignment, Declaration, FunctionCall, FunctionCallArg, FunctionDeclaration, Identifier, IfStatement, LiteralBool, LiteralFloat,
    LiteralInt, MemberAccess, ReturnStatement, StatementList, TableConstructor, TableField, UnaryOperation, UnaryOperationKind, VariableAssignment, WhileLoop,
};
use crate::syntax::span::compute_span_over_slice;
use crate::syntax::span::{Span, Spanned};
use crate::syntax::token::Token;
use crate::types::function_info::ArgumentInfo;
use crate::types::type_system::{FunctionType, Type};
use chumsky::input::ValueInput;
use chumsky::pratt;
use chumsky::prelude::*;
use std::rc::Rc;

type ParserExtra<'src> = extra::Err<Rich<'src, Token<'src>, Span>>;

type CallArgList<'src> = Vec<(Option<Spanned<Identifier<'src>>>, Spanned<Ast<'src>>)>;

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
            .map(|(callee, args): (Spanned<Ast<'src>>, CallArgList<'src>)| {
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

        // TODO: call should probably be moved to this?
        let primary = member_access.or(atom);

        let arith_mul_prod_op = choice((
            just(Token::Operator('*')).to(BinaryOperationKind::Product),
            just(Token::Operator('/')).to(BinaryOperationKind::DoubleDivision),
            just(Token::DoubleSlash).to(BinaryOperationKind::WholeDivision),
        ));

        let comparison_op = choice((
            just(Token::Operator('<')).to(BinaryOperationKind::LessThan),
            just(Token::Operator('>')).to(BinaryOperationKind::GreaterThan),
            just(Token::DoubleEqual).to(BinaryOperationKind::Equal),
            just(Token::NotEqual).to(BinaryOperationKind::NotEqual),
            just(Token::LessThanEqual).to(BinaryOperationKind::LessThanEqual),
            just(Token::GreaterThanEqual).to(BinaryOperationKind::GreaterThanEqual),
        ));

        primary.pratt((
            // Power
            pratt::infix(pratt::right(100), just(Token::DoubleStar), |lhs, _, rhs, extra| {
                (Ast::BinaryOperation(BinaryOperation(Box::new(lhs), BinaryOperationKind::Power, Box::new(rhs))), extra.span())
            }),
            // Unary operators
            pratt::prefix(90, just(Token::Operator('+')), |_, ast, extra| {
                (Ast::UnaryOperation(UnaryOperation(UnaryOperationKind::Plus, Box::new(ast))), extra.span())
            }),
            pratt::prefix(90, just(Token::Operator('-')), |_, ast, extra| {
                (Ast::UnaryOperation(UnaryOperation(UnaryOperationKind::Minus, Box::new(ast))), extra.span())
            }),
            // Binary operators (arithmetic)
            pratt::infix(pratt::left(80), arith_mul_prod_op, |lhs, op, rhs, extra| {
                (Ast::BinaryOperation(BinaryOperation(Box::new(lhs), op, Box::new(rhs))), extra.span())
            }),
            pratt::infix(pratt::left(70), just(Token::Operator('+')), |lhs, _, rhs, extra| {
                (Ast::BinaryOperation(BinaryOperation(Box::new(lhs), BinaryOperationKind::Addition, Box::new(rhs))), extra.span())
            }),
            pratt::infix(pratt::left(70), just(Token::Operator('-')), |lhs, _, rhs, extra| {
                (Ast::BinaryOperation(BinaryOperation(Box::new(lhs), BinaryOperationKind::Subtraction, Box::new(rhs))), extra.span())
            }),
            // Binary operators (comparisons)
            pratt::infix(pratt::left(60), comparison_op, |lhs, op, rhs, extra| {
                (Ast::BinaryOperation(BinaryOperation(Box::new(lhs), op, Box::new(rhs))), extra.span())
            }),
            // Binary operators (logical)
            pratt::infix(pratt::left(30), just(Token::DoubleAmpersand), |lhs, _, rhs, extra| {
                (Ast::BinaryOperation(BinaryOperation(Box::new(lhs), BinaryOperationKind::LogicalAnd, Box::new(rhs))), extra.span())
            }),
            pratt::infix(pratt::left(29), just(Token::DoubleBar), |lhs, _, rhs, extra| {
                (Ast::BinaryOperation(BinaryOperation(Box::new(lhs), BinaryOperationKind::LogicalOr, Box::new(rhs))), extra.span())
            }),
            // Assignment
            pratt::infix(pratt::right(10), just(Token::Operator('=')), |lhs, _, rhs, extra| match lhs {
                (Ast::Identifier(Identifier(ident)), ident_span) => (Ast::VariableAssignment(VariableAssignment((ident, ident_span), Box::new(rhs))), extra.span()),
                _ => (Ast::ComplexAssignment(ComplexAssignment(Box::new(lhs), Box::new(rhs))), extra.span()),
            }),
        ))
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

        let declaration = choice((just(Token::Let).to(BindingType::ImmutableVariable), just(Token::Mut).to(BindingType::MutableVariable)))
            .then(identifier.map_with(|ident, extra| (ident, extra.span())))
            .then_ignore(just(Token::Operator('=')))
            .then(parse_expression())
            .map_with(|((binding, ident), expr), extra| (binding, ident, expr, extra.span()))
            .map(|(binding, ident, expr, span)| {
                (
                    Ast::Declaration(Declaration {
                        assignment: VariableAssignment(ident, Box::new(expr)),
                        binding,
                    }),
                    span,
                )
            });

        let return_ = just(Token::Return)
            .ignore_then(parse_expression().or_not())
            .map_with(|expression, extra| (Ast::ReturnStatement(ReturnStatement { value: expression.map(Box::new) }), extra.span()));

        let expression_statement = parse_expression();

        let statement = choice((declaration, while_loop, do_while_loop, infinite_loop, if_check, return_, expression_statement));

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
