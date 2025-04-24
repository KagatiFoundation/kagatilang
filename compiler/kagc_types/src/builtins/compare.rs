use super::builtin::TypeId;

/// Trait for types that can be compared based on their variant and equality.
pub trait BuiltinTypeComparable {
    /// Compares `self` with another instance of the same type for equality.
    ///
    /// # Parameters
    /// - `other`: Another instance of the same type to compare with.
    ///
    /// # Returns
    /// - `true` if the types are equal, `false` otherwise.
    fn cmp(&self, other: &Self) -> bool;

    /// Returns the variant of the type.
    ///
    /// # Returns
    /// - The `LitTypeVariant` representing the type's variant.
    fn variant(&self) -> TypeId;
}