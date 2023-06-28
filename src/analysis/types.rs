use crate::span::Span;

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct UniqueFunctionIdentifier<'ast>(pub &'ast str);

pub struct SemanticError {
    error_text: String,
    span: Span,
}

#[derive(Default)]
pub struct SemanticErrorList(Vec<SemanticError>);

impl<'f> UniqueFunctionIdentifier<'f> {
    pub fn as_str(&self) -> &'f str {
        self.0
    }
}

impl SemanticError {
    pub fn new(error_text: String, span: Span) -> Self {
        Self { error_text, span }
    }

    pub fn error_text(&self) -> &str {
        &self.error_text
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl SemanticErrorList {
    pub fn report_error(&mut self, span: Span, error_text: String) {
        self.0.push(SemanticError::new(error_text, span));
    }

    pub fn into_vec(self) -> Vec<SemanticError> {
        self.0
    }
}
