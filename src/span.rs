use chumsky::span::SimpleSpan;

pub type Spanned<T> = (T, Span);
pub type Span = SimpleSpan<usize>;
