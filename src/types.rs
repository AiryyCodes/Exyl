#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I32,
    String,
    Void,
}

impl Type {
    /// Checks if this type is assignable to another
    pub fn is_assignable_to(&self, target: &Type) -> bool {
        match (self, target) {
            (source, target) => source == target,
        }
    }
}
