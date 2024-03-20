use crate::syntax::span::Span;

pub struct SemanticErrorNote {
    error_text: String,
    span: Span,
}

pub struct SemanticError {
    error_text: String,
    span: Span,
    note: Option<SemanticErrorNote>,
}

#[derive(Default)]
pub struct SemanticErrorList(Vec<SemanticError>);

impl SemanticError {
    pub fn new(error_text: String, span: Span) -> Self {
        Self { error_text, span, note: None }
    }

    pub fn new_with_note(error_text: String, span: Span, note_text: String, note_span: Span) -> Self {
        Self {
            error_text,
            span,
            note: Some(SemanticErrorNote {
                error_text: note_text,
                span: note_span,
            }),
        }
    }

    pub fn error_text(&self) -> &str {
        &self.error_text
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn note(&self) -> Option<&SemanticErrorNote> {
        self.note.as_ref()
    }
}

impl SemanticErrorNote {
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

    pub fn report_error_with_note(&mut self, span: Span, error_text: String, note_span: Span, note_text: String) {
        self.0.push(SemanticError::new_with_note(error_text, span, note_text, note_span));
    }

    pub fn into_vec(self) -> Vec<SemanticError> {
        self.0
    }

    pub fn has_errors(&self) -> bool {
        !self.0.is_empty()
    }
}
