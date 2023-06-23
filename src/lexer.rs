use crate::span::Span;
use crate::token::Token;
use chumsky::prelude::*;

pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
    let num = text::int(10).from_str().unwrapped().map(Token::LiteralInt);

    let newline = text::newline().to(Token::Newline);

    let multi_operator = choice((
        just("**").to(Token::DoubleStar),
        just("==").to(Token::DoubleEqual),
        just("!=").to(Token::NotEqual),
        just("//").to(Token::DoubleSlash),
    ));

    let compound_assignment = choice((
        just("+=").to(Token::PlusEqual),
        just("-=").to(Token::MinusEqual),
        just("*=").to(Token::StarEqual),
        just("**=").to(Token::DoubleStarEqual),
        just("/=").to(Token::SlashEqual),
        just("%=").to(Token::PercentEqual),
        just("//=").to(Token::DoubleSlashEqual),
    ));

    let single_operator = one_of("+-*/%=").map(Token::Operator);

    let parens = choice((
        just('(').to(Token::LeftParen),
        just(')').to(Token::RightParen),
    ));

    let block_control = just(':').to(Token::Colon);

    let keyword_or_identifier = text::ascii::ident().map(|ident| match ident {
        "if" => Token::If,
        "for" => Token::For,
        "while" => Token::While,
        "do" => Token::Do,
        "true" => Token::True,
        "false" => Token::False,
        "fn" => Token::Fn,
        "return" => Token::Return,
        "pub" => Token::Pub,
        "prot" => Token::Prot,
        "priv" => Token::Priv,
        "class" => Token::Class,
        "interface" => Token::Interface,
        "import" => Token::Import,
        _ => Token::Identifier(ident),
    });

    let token = choice((
        newline,
        num,
        parens,
        multi_operator,
        compound_assignment,
        single_operator,
        block_control,
        keyword_or_identifier,
    ));

    token
        .map_with_span(|token, span| (token, span))
        .padded_by(text::inline_whitespace())
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}
