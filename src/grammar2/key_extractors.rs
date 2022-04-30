use crate::utils::svec::KeyExtractor;

use super::traits::{NonTerm, Prod};

/// Key extractor for `Rule`, using the `NonTerm`'s `Key`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub struct NonTermKeyExtractor;

impl<T> KeyExtractor<T> for NonTermKeyExtractor
where
  T: NonTerm,
{
  type Key = T::Key;

  fn extract_key<'a>(&self, rule: &'a T) -> &'a Self::Key {
    rule.key()
  }
}

/// Key extractor for `Rule`, using the `NonTerm`'s `Key`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub struct ProdKeyExtractor;

impl<T> KeyExtractor<T> for ProdKeyExtractor
where
  T: Prod,
{
  type Key = T::ProdId;

  fn extract_key<'a>(&self, rule: &'a T) -> &'a Self::Key {
    rule.action_id()
  }
}
