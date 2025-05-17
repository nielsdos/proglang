use crate::syntax::ast::Ast;
use chumsky::span::SimpleSpan;

pub type Spanned<T> = (T, Span);
pub type Span = SimpleSpan<usize>;

pub fn compute_span_over_slice(slice: &[Spanned<Ast>]) -> Span {
    if slice.is_empty() {
        (0..0).into()
    } else {
        (slice[0].1.start..slice[slice.len() - 1].1.end).into()
    }
}

pub fn combine_span(span1: Span, span2: Span) -> Span {
    (span1.start..span2.end).into()
}
