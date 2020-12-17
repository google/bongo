use std::collections::{btree_map, BTreeMap};

use super::WasChanged;

pub trait Mergable {
  fn merge(&mut self, other: &Self) -> WasChanged;
}

#[derive(Copy, Clone, Debug)]
pub struct Value<K, V> {
  key: K,
  value: V,
}

impl<K, V> Value<K, V> {
  pub fn key(&self) -> &K {
    &self.key
  }
  pub fn value(&self) -> &V {
    &self.value
  }
  pub fn value_mut(&mut self) -> &mut V {
    &mut self.value
  }
}

#[derive(Debug)]
pub struct ValueRef<'a, K, V> {
  key: &'a K,
  value: &'a V,
}

impl<'a, K, V> ValueRef<'a, K, V> {
  pub fn key(&self) -> &'a K {
    self.key
  }
  pub fn value(&self) -> &'a V {
    self.value
  }
}

impl<'a, K, V> ValueRef<'a, K, V>
where
  K: Clone,
  V: Clone,
{
  pub fn to_value(&self) -> Value<K, V> {
    Value {
      key: self.key.clone(),
      value: self.value.clone(),
    }
  }
}

impl<'a, K, V> Clone for ValueRef<'a, K, V> {
  fn clone(&self) -> Self {
    ValueRef {
      key: self.key,
      value: self.value,
    }
  }
}

impl<'a, K, V> Copy for ValueRef<'a, K, V> {}

#[derive(Copy, Clone, Debug)]
pub enum ValueCow<'a, K, V> {
  Owned(Value<K, V>),
  Borrowed(ValueRef<'a, K, V>),
}

impl<'a, K, V> ValueCow<'a, K, V> {
  pub fn as_ref<'b>(&'b self) -> ValueRef<'a, K, V>
  where
    'b: 'a,
  {
    match self {
      ValueCow::Owned(owned) => ValueRef {
        key: &owned.key,
        value: &owned.value,
      },
      ValueCow::Borrowed(borrowed) => *borrowed,
    }
  }

  pub fn key<'s: 'a>(&'s self) -> &'a K {
    match self {
      ValueCow::Owned(owned) => &owned.key,
      ValueCow::Borrowed(borrowed) => borrowed.key,
    }
  }

  pub fn value<'s: 'a>(&'s self) -> &'a V {
    match self {
      ValueCow::Owned(owned) => &owned.value,
      ValueCow::Borrowed(borrowed) => borrowed.value,
    }
  }
}

impl<'a, K, V> ValueCow<'a, K, V>
where
  K: Clone,
  V: Clone,
{
  pub fn to_owned(&self) -> Value<K, V> {
    match self {
      ValueCow::Owned(owned) => owned.clone(),
      ValueCow::Borrowed(borrowed) => borrowed.to_value(),
    }
  }

  pub fn into_owned(self) -> Value<K, V> {
    match self {
      ValueCow::Owned(owned) => owned,
      ValueCow::Borrowed(borrowed) => borrowed.to_value(),
    }
  }
}

impl<'a, K, V> From<&'a Value<K, V>> for ValueRef<'a, K, V> {
  fn from(val_ref: &'a Value<K, V>) -> Self {
    ValueRef {
      key: &val_ref.key,
      value: &val_ref.value,
    }
  }
}

impl<'a, K, V> From<&'a Value<K, V>> for ValueCow<'a, K, V> {
  fn from(val_ref: &'a Value<K, V>) -> Self {
    ValueCow::Borrowed(ValueRef {
      key: &val_ref.key,
      value: &val_ref.value,
    })
  }
}

impl<'a, K, V> From<ValueRef<'a, K, V>> for ValueCow<'a, K, V> {
  fn from(val_ref: ValueRef<'a, K, V>) -> Self {
    ValueCow::Borrowed(val_ref)
  }
}

impl<'a, K, V> From<Value<K, V>> for ValueCow<'a, K, V> {
  fn from(val_ref: Value<K, V>) -> Self {
    ValueCow::Owned(val_ref)
  }
}

// -----------

pub struct MergableMap<K, V> {
  map: BTreeMap<K, V>,
}

impl<K, V> MergableMap<K, V>
where
  K: Ord,
  V: Mergable,
{
  pub fn new() -> Self {
    MergableMap {
      map: BTreeMap::new(),
    }
  }

  pub fn insert(&mut self, val: Value<K, V>) -> WasChanged {
    match self.map.entry(val.key) {
      btree_map::Entry::Occupied(mut occ) => occ.get_mut().merge(&val.value),
      btree_map::Entry::Vacant(vac) => {
        vac.insert(val.value);
        WasChanged::Changed
      }
    }
  }

  pub fn get_entry(&self, key: &K) -> Option<ValueRef<K, V>> {
    self
      .map
      .get_key_value(key)
      .map(|(key, value)| ValueRef { key, value })
  }
}

impl<K, V> MergableMap<K, V>
where
  K: Ord + Clone,
  V: Mergable + Clone,
{
  pub fn insert_cow<'a, 's: 'a>(
    &'s mut self,
    val: impl Into<ValueCow<'a, K, V>>,
  ) -> WasChanged {
    let cow = val.into();
    if let Some(mut_v) = self.map.get_mut(cow.as_ref().key()) {
      mut_v.merge(cow.as_ref().value())
    } else {
      let val = cow.into_owned();
      self.map.insert(val.key, val.value);
      WasChanged::Changed
    }
  }
}
