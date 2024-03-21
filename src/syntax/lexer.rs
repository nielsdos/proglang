use crate::syntax::span::Span;
use crate::syntax::token::{Token, TokenTree};
use chumsky::container::Container;
use chumsky::prelude::*;
use std::str::FromStr;

struct FlatVec<T> {
    inner: Vec<T>,
}

impl<T> Default for FlatVec<T> {
    fn default() -> Self {
        Self { inner: Vec::new() }
    }
}

impl<T> Container<Vec<T>> for FlatVec<T> {
    fn with_capacity(n: usize) -> Self {
        Self { inner: Vec::with_capacity(n) }
    }

    fn push(&mut self, item: Vec<T>) {
        self.inner.extend(item);
    }
}

pub fn lexer<'src>() -> impl Parser<'src, &'src str, Vec<TokenTree<'src>>, extra::Err<Rich<'src, char, Span>>> {
    // TODO: these two can fail, handle them gracefully
    // TODO: https://github.com/zesterer/chumsky/pull/462
    let dbl = text::int(10).then(just('.')).then(text::digits(10)).to_slice().map(|x| Token::LiteralFloat(f64::from_str(x).unwrap()));
    let int = text::int(10).from_str().unwrapped().map(Token::LiteralInt);

    let multi_operator = choice((
        just("**").to(Token::DoubleStar),
        just("==").to(Token::DoubleEqual),
        just("!=").to(Token::NotEqual),
        just("//").to(Token::DoubleSlash),
        just("<=").to(Token::LessThanEqual),
        just(">=").to(Token::GreaterThanEqual),
        just("->").to(Token::Arrow),
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

    let comma = just(',').to(Token::Comma);

    let parens = choice((just('(').to(Token::LeftParen), just(')').to(Token::RightParen)));

    let keyword_or_identifier = text::ascii::ident().map(|ident| match ident {
        "if" => Token::If,
        "else" => Token::Else,
        "for" => Token::For,
        "while" => Token::While,
        "do" => Token::Do,
        "let" => Token::Let,
        "mut" => Token::Mut,
        "true" => Token::LiteralBool(true),
        "false" => Token::LiteralBool(false),
        "fn" => Token::Fn,
        "return" => Token::Return,
        "pub" => Token::Pub,
        "prot" => Token::Prot,
        "priv" => Token::Priv,
        "class" => Token::Class,
        "import" => Token::Import,
        _ => Token::Identifier(ident),
    });

    let comment = just('#').padded_by(text::inline_whitespace()).then(any().and_is(text::newline().not()).repeated());

    let token = choice((dbl, int, comma, parens, multi_operator, compound_assignment, single_operator, keyword_or_identifier));

    let block = recursive(|block| {
        let indentation = text::inline_whitespace().configure(|cfg, &parent_indentation| cfg.exactly(parent_indentation));
        let tokens_base = token
            .map_with(|token, extra| (token, extra.span()))
            .padded_by(text::inline_whitespace())
            .map(TokenTree::Leaf)
            .repeated()
            .at_least(1);
        let tokens_stop = tokens_base
            .clone()
            .collect::<Vec<TokenTree>>()
            .then_ignore(comment.or_not())
            .then(text::newline().to(Token::StatementEnd).map_with(|token, extra| {
                let span: Span = extra.span();
                (token, Span::new(span.start, span.end - 1))
            }))
            .map(|(mut before_end_token, end_token)| {
                before_end_token.push(TokenTree::Leaf(end_token));
                before_end_token
            });
        let empty = comment.or_not().ignore_then(text::newline()).map(|_| vec![]);
        let new_block = tokens_base
            .collect::<Vec<TokenTree>>()
            .then(just(':').then_ignore(comment.or_not()).then(text::newline()))
            .then(block)
            .map(|((mut line, _), block)| {
                line.push(TokenTree::Tree(block));
                line
            });

        text::inline_whitespace()
            .count()
            .ignore_with_ctx(tokens_stop.or(new_block).or(empty).separated_by(indentation).collect::<FlatVec<TokenTree>>().map(|result| result.inner))
    });

    block.with_ctx(0)
}
