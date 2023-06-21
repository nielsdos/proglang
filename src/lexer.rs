use crate::span::Span;
use crate::token::Token;
use chumsky::prelude::*;

pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
    let num = text::int(10).from_str().unwrapped().map(Token::LiteralInt);

    let newline = text::newline().to(Token::Newline);

    let multi_operator = just("**")
        .to(Token::DoubleStar)
        .or(just("==").to(Token::DoubleEqual));

    let single_operator = one_of("+-*/=").map(Token::Operator);

    let parens = just('(')
        .to(Token::LeftParen)
        .or(just(')').to(Token::RightParen));

    let identifier = text::ascii::ident().map(|identifier| match identifier {
        "for" => Token::For,
        "while" => Token::While,
        "do" => Token::Do,
        "true" => Token::True,
        "false" => Token::False,
        _ => Token::Identifier(identifier),
    });

    let token = num
        .or(newline)
        .or(parens)
        .or(multi_operator)
        .or(single_operator)
        .or(identifier);

    token
        .map_with_span(|token, span| (token, span))
        .padded_by(text::inline_whitespace())
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}
