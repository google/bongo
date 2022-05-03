use std::{fmt::Debug, ops::Deref};

pub trait KeyExtractor<V> {
  type Key;
  fn extract_key<'a>(&self, value: &'a V) -> &'a Self::Key;
}

impl<K, V, F> KeyExtractor<V> for F
where
  F: Fn(&V) -> &K,
{
  type Key = K;

  fn extract_key<'a>(&self, value: &'a V) -> &'a K {
    self(value)
  }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SVec<V, F> {
  extractor: F,
  vec: Box<[V]>,
}

impl<V, F> SVec<V, F> {
  pub fn iter(&self) -> impl Iterator<Item = &V> {
    self.vec.iter()
  }

  pub fn as_slice(&self) -> &[V] {
    &self.vec
  }
}

impl<V, F> SVec<V, F>
where
  F: KeyExtractor<V>,
  F::Key: Ord,
{
  pub fn new<I>(extractor: F, iter: I) -> Self
  where
    I: IntoIterator<Item = V>,
  {
    let mut vec: Vec<_> = iter.into_iter().collect();
    vec.sort_by(|a, b| {
      let a = extractor.extract_key(a);
      let b = extractor.extract_key(b);
      a.cmp(b)
    });
    SVec {
      extractor,
      vec: vec.into_boxed_slice(),
    }
  }

  pub fn get(&self, key: &F::Key) -> Option<&V> {
    self
      .vec
      .binary_search_by(|v| {
        let v = self.extractor.extract_key(v);
        v.cmp(key)
      })
      .ok()
      .map(|i| &self.vec[i])
  }

  pub fn get_index_of(&self, key: &F::Key) -> Option<usize> {
    self
      .vec
      .binary_search_by(|v| {
        let v = self.extractor.extract_key(v);
        v.cmp(key)
      })
      .ok()
  }

  pub fn get_by_index(&self, index: usize) -> Option<&V> {
    self.vec.get(index)
  }
}

impl<V, F> From<Vec<V>> for SVec<V, F>
where
  F: KeyExtractor<V> + Default,
  F::Key: Ord,
{
  fn from(mut vec: Vec<V>) -> Self {
    let extractor = F::default();
    vec.sort_by(|a, b| {
      let a = extractor.extract_key(a);
      let b = extractor.extract_key(b);
      a.cmp(b)
    });
    SVec {
      extractor,
      vec: vec.into_boxed_slice(),
    }
  }
}

impl<V, F> IntoIterator for SVec<V, F>
where
  F: KeyExtractor<V>,
  F::Key: Ord,
{
  type Item = V;
  type IntoIter = std::vec::IntoIter<V>;

  fn into_iter(self) -> Self::IntoIter {
    Vec::from(self.vec).into_iter()
  }
}

impl<V, F> FromIterator<V> for SVec<V, F>
where
  F: KeyExtractor<V> + Default,
  F::Key: Ord,
{
  fn from_iter<I>(iter: I) -> Self
  where
    I: IntoIterator<Item = V>,
  {
    Self::new(F::default(), iter)
  }
}

impl<V, F> Deref for SVec<V, F>
where
  F: KeyExtractor<V>,
  F::Key: Ord,
{
  type Target = [V];

  fn deref(&self) -> &Self::Target {
    &self.vec
  }
}

impl<V, F> Debug for SVec<V, F>
where
  V: Debug,
{
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_list().entries(self.iter()).finish()
  }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct IdentityKeyExtractor;

impl<V> KeyExtractor<V> for IdentityKeyExtractor {
  type Key = V;

  fn extract_key<'a>(&self, value: &'a V) -> &'a V {
    value
  }
}
