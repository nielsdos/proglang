use crate::syntax::span::{Span, Spanned};
use crate::syntax::token::Token;
use chumsky::prelude::*;
use std::str::FromStr;

pub fn lexer<'src>() -> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Rich<'src, char, Span>>> {
    // TODO: this can fail and should support '_' and e-notation
    let dbl = text::int(10).then(just('.')).then(text::digits(10)).to_slice().map(|x| Token::LiteralFloat(f64::from_str(x).unwrap()));

    let lex_digits = move |radix: u32, forbid_leading_zeros: bool| {
        any()
            .filter(move |c: &char| c.is_digit(radix))
            .then(just('_').or_not().ignore_then(any().filter(move |&c: &char| c.is_digit(radix) || c == '_')).repeated())
            .to_slice()
            // A failure here isn't fatal for the parsing process, try to continue
            .validate(move |value: &str, extra, emitter| {
                if forbid_leading_zeros && value.starts_with('0') && value.len() > 1 {
                    emitter.emit(Rich::<'src, char, Span>::custom(
                        extra.span(),
                        "leading zeros in integer literals are not allowed, use an 0o prefix for octal integers",
                    ));
                    return Token::LiteralInt(0);
                }

                let parse_result = if value.contains('_') {
                    let value: String = value.chars().filter(move |&c: &char| c != '_').collect();
                    i64::from_str_radix(&value, radix)
                } else {
                    i64::from_str_radix(value, radix)
                };

                match parse_result {
                    Err(_) => {
                        emitter.emit(Rich::<'src, char, Span>::custom(extra.span(), "integer literal overflow"));
                        Token::LiteralInt(0)
                    }
                    Ok(number) => Token::LiteralInt(number),
                }
            })
    };

    let int = choice((
        just("0x").ignore_then(lex_digits(16, false)),
        just("0o").ignore_then(lex_digits(8, false)),
        just("0b").ignore_then(lex_digits(2, false)),
        lex_digits(10, true),
    ))
    .boxed();

    let multi_operator = choice((
        just("**").to(Token::DoubleStar),
        just("==").to(Token::DoubleEqual),
        just("!=").to(Token::NotEqual),
        just("//").to(Token::DoubleSlash),
        just("<=").to(Token::LessThanEqual),
        just(">=").to(Token::GreaterThanEqual),
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

    let single_operator = one_of("+-*/%=<>").map(Token::Operator);

    let ampersand = just('&').to(Token::Ampersand);
    let colon = just(':').to(Token::Colon);
    let dot = just('.').to(Token::Dot);
    let comma = just(',').to(Token::Comma);

    let parens = choice((just('(').to(Token::LeftParen), just(')').to(Token::RightParen)));

    let braces = choice((just('{').to(Token::LeftBrace), just('}').to(Token::RightBrace)));

    let keyword_or_identifier = text::ascii::ident().map(|ident| match ident {
        "if" => Token::If,
        "else" => Token::Else,
        "for" => Token::For,
        "while" => Token::While,
        "do" => Token::Do,
        "then" => Token::Then,
        "end" => Token::End,
        "loop" => Token::Loop,
        "let" => Token::Let,
        "mut" => Token::Mut,
        "true" => Token::LiteralBool(true),
        "false" => Token::LiteralBool(false),
        "fn" => Token::Fn,
        "return" => Token::Return,
        "class" => Token::Class,
        _ => Token::Identifier(ident),
    });

    let comment = just('#').then(any().and_is(text::newline().not()).repeated()).padded();

    let token = choice((dbl, int, ampersand, colon, dot, comma, parens, braces, multi_operator, compound_assignment, single_operator, keyword_or_identifier));

    token
        .padded_by(comment.repeated())
        .padded()
        .map_with(|tok, e| (tok, e.span()))
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}
