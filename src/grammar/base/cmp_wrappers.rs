use std::{cmp, ops};

fn ref_eq<T>(a: &T, b: &T) -> bool {
  std::ptr::eq(a, b)
}

fn ref_cmp<T>(a: &T, b: &T) -> cmp::Ordering {
  (a as *const T).cmp(&(b as *const T))
}

// ------------

/// A simple deref wrapper that ensures that two references _must_ be the same during
/// comparison. This ensures that we can't accidentally incorporate refs from different parents together.
#[derive(Debug)]
pub struct ParentRef<'a, T>(&'a T);

impl<'a, T> ParentRef<'a, T> {
  pub fn new(r: &'a T) -> Self {
    ParentRef(r)
  }
}

impl<T> cmp::PartialEq for ParentRef<'_, T> {
  fn eq(&self, other: &Self) -> bool {
    assert!(ref_eq(self.0, other.0));
    true
  }
}

impl<T> cmp::Eq for ParentRef<'_, T> {}

impl<T> cmp::PartialOrd for ParentRef<'_, T> {
  fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl<T> cmp::Ord for ParentRef<'_, T> {
  fn cmp(&self, other: &Self) -> cmp::Ordering {
    assert!(ref_eq(self.0, other.0));
    cmp::Ordering::Equal
  }
}

impl<'a, T> ops::Deref for ParentRef<'a, T> {
  type Target = &'a T;
  fn deref(&self) -> &&'a T {
    &self.0
  }
}

impl<T> Clone for ParentRef<'_, T> {
  fn clone(&self) -> Self {
    ParentRef(self.0)
  }
}

impl<T> Copy for ParentRef<'_, T> {}

// ------------

#[derive(Clone, Copy, Debug)]
pub struct NoCompare<T>(T);

impl<T> NoCompare<T> {
  pub fn new(value: T) -> Self {
    NoCompare(value)
  }
}

impl<T> cmp::PartialEq for NoCompare<T> {
  fn eq(&self, _: &Self) -> bool {
    true
  }
}

impl<T> cmp::Eq for NoCompare<T> {}

impl<T> cmp::PartialOrd for NoCompare<T> {
  fn partial_cmp(&self, _: &Self) -> Option<cmp::Ordering> {
    Some(cmp::Ordering::Equal)
  }
}

impl<T> cmp::Ord for NoCompare<T> {
  fn cmp(&self, _: &Self) -> cmp::Ordering {
    cmp::Ordering::Equal
  }
}

impl<T> ops::Deref for NoCompare<T> {
  type Target = T;
  fn deref(&self) -> &T {
    &self.0
  }
}

// ------------

#[derive(Debug)]
pub struct RefCompare<'a, T>(&'a T);

impl<'a, T> RefCompare<'a, T> {
  pub fn new(r: &'a T) -> Self {
    RefCompare(r)
  }
}

impl<T> cmp::PartialEq for RefCompare<'_, T> {
  fn eq(&self, other: &Self) -> bool {
    ref_eq(self.0, other.0)
  }
}

impl<T> cmp::Eq for RefCompare<'_, T> {}

impl<T> cmp::PartialOrd for RefCompare<'_, T> {
  fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl<T> cmp::Ord for RefCompare<'_, T> {
  fn cmp(&self, other: &Self) -> cmp::Ordering {
    ref_cmp(self.0, other.0)
  }
}

impl<'a, T> ops::Deref for RefCompare<'a, T> {
  type Target = &'a T;
  fn deref(&self) -> &&'a T {
    &self.0
  }
}

impl<T> Clone for RefCompare<'_, T> {
  fn clone(&self) -> Self {
    RefCompare(self.0)
  }
}

impl<T> Copy for RefCompare<'_, T> {}
