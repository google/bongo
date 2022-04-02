//! Helper functions for running formatting rule.

use std::fmt::{Debug, Formatter, Result};

#[derive(Clone)]
struct DebugListFormatted<C>(C);

impl<'a, C: IntoIterator + Clone> Debug for DebugListFormatted<C>
where
  C::Item: Debug,
{
  fn fmt(&self, fmt: &mut Formatter) -> Result {
    let mut list = fmt.debug_list();
    list.entries(self.0.clone().into_iter());
    list.finish()
  }
}

#[derive(Clone)]
struct DebugSetFormatted<C>(C);

impl<'a, C: IntoIterator + Clone> Debug for DebugSetFormatted<C>
where
  C::Item: Debug,
{
  fn fmt(&self, fmt: &mut Formatter) -> Result {
    let mut dbg_set = fmt.debug_set();
    dbg_set.entries(self.0.clone().into_iter());
    dbg_set.finish()
  }
}

#[derive(Clone)]
struct DebugTupleFormatted<N, C>(N, C);

impl<N, C> Debug for DebugTupleFormatted<N, C>
where
  N: AsRef<str>,
  C: IntoIterator + Clone,
  C::Item: Debug,
{
  fn fmt(&self, fmt: &mut Formatter) -> Result {
    let mut tuple = fmt.debug_tuple(self.0.as_ref());
    for field in self.1.clone().into_iter() {
      tuple.field(&field);
    }
    tuple.finish()
  }
}

#[derive(Clone)]
struct DebugStructFormatted<N, C>(N, C);

impl<N, C, K, V> Debug for DebugStructFormatted<N, C>
where
  N: AsRef<str>,
  C: IntoIterator<Item = (K, V)> + Clone,
  K: AsRef<str>,
  V: Debug,
{
  fn fmt(&self, fmt: &mut Formatter) -> Result {
    let mut dbg_struct = fmt.debug_struct(self.0.as_ref());
    for (k, v) in self.1.clone().into_iter() {
      dbg_struct.field(k.as_ref(), &v);
    }
    dbg_struct.finish()
  }
}

pub fn list_fmt<C>(items: C) -> impl Debug
where
  C: IntoIterator + Clone,
  C::Item: Debug,
{
  DebugListFormatted(items)
}

pub fn set_fmt<C>(items: C) -> impl Debug
where
  C: IntoIterator + Clone,
  C::Item: Debug,
{
  DebugSetFormatted(items)
}

pub fn tuple_fmt<N, C>(name: N, items: C) -> impl Debug
where
  N: AsRef<str> + Clone,
  C: IntoIterator + Clone,
  C::Item: Debug,
{
  DebugTupleFormatted(name, items)
}

pub fn struct_fmt<N, C, K, V>(name: N, items: C) -> impl Debug
where
  N: AsRef<str>,
  C: IntoIterator<Item = (K, V)> + Clone,
  K: AsRef<str>,
  V: Debug,
{
  DebugStructFormatted(name, items)
}
