#[derive(Debug, Eq, Hash, PartialEq)]
pub struct UniqueFunctionIdentifier<'ast>(pub &'ast str);

impl<'f> UniqueFunctionIdentifier<'f> {
    pub fn as_str(&self) -> &'f str {
        self.0
    }
}
