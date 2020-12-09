// Copyright 2019 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     https://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use std::collections::{btree_map, BTreeMap, BTreeSet};

pub mod buffer;
pub mod fmt;
pub mod graph_closure;

pub fn fixed_point<T: Eq>(start: T, mut apply: impl FnMut(&T) -> T) -> T {
  let mut curr = start;
  loop {
    let next = apply(&curr);
    if next == curr {
      break curr;
    }
    curr = next;
  }
}

pub trait OrdKey:
  Clone + PartialEq + Eq + PartialOrd + Ord + std::fmt::Debug + 'static
{
}

impl<
    T: Clone + PartialEq + Eq + PartialOrd + Ord + std::fmt::Debug + 'static,
  > OrdKey for T
{
}

pub trait ToDoc {
  fn to_doc<'a, DA: pretty::DocAllocator<'a>>(
    &self,
    da: &'a DA,
  ) -> pretty::DocBuilder<'a, DA, ()>
  where
    DA::Doc: Clone;
}

impl ToDoc for () {
  fn to_doc<'a, DA: pretty::DocAllocator<'a>>(
    &self,
    da: &'a DA,
  ) -> pretty::DocBuilder<'a, DA> {
    da.text("()").into()
  }
}

/// Given an iterator, returns the only element in the iterator if it yields
/// only a single item, otherwise return None.
pub fn take_only<I: Iterator>(mut iter: I) -> Option<I::Item> {
  iter
    .next()
    .and_then(|v| if iter.next().is_some() { None } else { Some(v) })
}

/// A refcounted name type, used to avoid duplicating common string values
/// throughout an AST.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(std::rc::Rc<String>);

impl Name {
  /// Creates a new Name containing the given string.
  pub fn new(s: &(impl AsRef<str> + ?Sized)) -> Self {
    Name(std::rc::Rc::new(s.as_ref().to_string()))
  }

  /// Returns a reference to the internal ref.
  pub fn str(&self) -> &str {
    &**self.0
  }

  /// Returns a mutable reference to a string to modify this name. Will not
  /// alter any other names.
  pub fn make_mut(&mut self) -> &mut String {
    std::rc::Rc::make_mut(&mut self.0)
  }
}

impl AsRef<str> for Name {
  fn as_ref(&self) -> &str {
    return self.str();
  }
}

impl std::fmt::Debug for Name {
  fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
    fmt.write_str(&self.0)
  }
}

impl std::fmt::Display for Name {
  fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
    fmt.write_str(&self.0)
  }
}

impl ToDoc for Name {
  fn to_doc<'a, DA: pretty::DocAllocator<'a>>(
    &self,
    da: &'a DA,
  ) -> pretty::DocBuilder<'a, DA> {
    da.text(self.str().to_string())
  }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum TreeValue<L, V> {
  Node(Box<TreeNode<L, V>>),
  Leaf(V),
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct TreeNode<L, V> {
  action_name: L,
  params: BTreeMap<Name, TreeValue<L, V>>,
}

impl<L: Ord, V> TreeNode<L, V> {
  pub fn new(action: L, params: BTreeMap<Name, TreeValue<L, V>>) -> Self {
    TreeNode {
      action_name: action,
      params: params,
    }
  }
  pub fn from_action(action: L) -> Self {
    TreeNode {
      action_name: action,
      params: BTreeMap::new(),
    }
  }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum Void {}

pub fn breadth_first_search<T, InitI, StepI, F>(
  initial: InitI,
  mut f: F,
) -> BTreeSet<T>
where
  T: Ord + Clone,
  InitI: IntoIterator<Item = T>,
  StepI: IntoIterator<Item = T>,
  F: FnMut(&T) -> StepI,
{
  let mut next_set = BTreeSet::new();
  let mut curr_set: BTreeSet<_> = initial.into_iter().collect();
  let mut seen_set = next_set.clone();

  while !curr_set.is_empty() {
    for next_item in &curr_set {
      for step_item in f(next_item) {
        if !seen_set.contains(&step_item) {
          next_set.insert(step_item.clone());
          seen_set.insert(step_item);
        }
      }
    }

    std::mem::swap(&mut curr_set, &mut next_set);
    next_set.clear();
  }

  seen_set
}

pub fn merge_value_pairs<K, V>(
  iter: impl IntoIterator<Item = (K, V)>,
) -> BTreeMap<K, BTreeSet<V>>
where
  K: Ord,
  V: Ord,
{
  let mut result = BTreeMap::new();
  for (k, v) in iter {
    match result.entry(k) {
      btree_map::Entry::Vacant(vac) => {
        let mut new_set = BTreeSet::new();
        new_set.insert(v);
        vac.insert(new_set);
      }
      btree_map::Entry::Occupied(mut occ) => {
        occ.get_mut().insert(v);
      }
    }
  }
  result
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum WasChanged {
  Changed,
  Unchanged,
}

impl WasChanged {
  pub fn from_changed(changed: bool) -> Self {
    if changed {
      WasChanged::Changed
    } else {
      WasChanged::Unchanged
    }
  }

  pub fn join(self, other: Self) -> Self {
    match (self, other) {
      (WasChanged::Changed, _) | (_, WasChanged::Changed) => {
        WasChanged::Changed
      }
      _ => WasChanged::Unchanged,
    }
  }

  pub fn merge(&mut self, other: Self) {
    *self = self.join(other);
  }
}

pub fn change_loop<F>(mut func: F)
where
  F: FnMut() -> WasChanged,
{
  while let WasChanged::Changed = func() {}
}

pub fn change_iter<I, F>(iter: I, mut func: F) -> WasChanged
where
  I: Iterator,
  F: FnMut(I::Item) -> WasChanged,
{
  let mut changed = WasChanged::Unchanged;
  for item in iter {
    changed = changed.join(func(item));
  }

  changed
}

pub struct CollectMap<K, V>(BTreeMap<K, BTreeSet<V>>);

impl<K, V> CollectMap<K, V>
where
  K: Ord,
  V: Ord,
{
  pub fn new() -> Self {
    CollectMap(BTreeMap::new())
  }

  pub fn get(&self, key: &K) -> Option<&BTreeSet<V>> {
    self.0.get(key)
  }

  pub fn insert(&mut self, key: K, value: V) -> WasChanged {
    match self.0.entry(key) {
      btree_map::Entry::Occupied(mut occ) => {
        WasChanged::from_changed(occ.get_mut().insert(value))
      }
      btree_map::Entry::Vacant(vac) => {
        let mut new_set = BTreeSet::new();
        new_set.insert(value);
        vac.insert(new_set);
        WasChanged::Changed
      }
    }
  }

  pub fn insert_iter(
    &mut self,
    key: K,
    values: impl IntoIterator<Item = V>,
  ) -> WasChanged
  where
    K: Clone,
  {
    match self.0.entry(key) {
      btree_map::Entry::Occupied(mut occ) => {
        let set = occ.get_mut();
        change_iter(values.into_iter(), |val| {
          WasChanged::from_changed(set.insert(val))
        })
      }
      btree_map::Entry::Vacant(vac) => {
        let mut val_iter = values.into_iter();
        match val_iter.next() {
          Some(init) => {
            let mut new_set = BTreeSet::new();
            new_set.insert(init);
            new_set.extend(val_iter);
            vac.insert(new_set);
            WasChanged::Changed
          }

          None => WasChanged::Unchanged,
        }
      }
    }
  }

  pub fn insert_from_key_set(
    &mut self,
    key: K,
    src_key: K,
  ) -> WasChanged where V: Clone {
    if key == src_key {
      return WasChanged::Unchanged;
    }

    let (key, mut value) = match self.0.entry(key) {
      btree_map::Entry::Occupied(occ) => occ.remove_entry(),
      btree_map::Entry::Vacant(vac) => (vac.into_key(), BTreeSet::new()),
    };

    let changed_result = if let Some(src_set) = self.0.get(&src_key) {
      change_iter(src_set.iter(), |src_value| {
        WasChanged::from_changed(value.insert(src_value.clone()))
      })
    } else {
      WasChanged::Unchanged
    };

    self.0.insert(key, value);

    changed_result
  }

  pub fn into_inner(self) -> BTreeMap<K, BTreeSet<V>> {
    self.0
  }
}

pub trait FixedPointProcessor<K, V> {
  fn next(&self, key: &K, value: &V) -> Vec<(K, V)>;
  fn merge(&self, target: &mut V, other: V) -> WasChanged;
}

pub fn apply_fixed_point<K, V, P: FixedPointProcessor<K, V>>(
  proc: &P,
  target: &mut BTreeMap<K, V>,
) where
  K: Clone + Ord,
{
  let currs = target.keys().cloned().collect::<BTreeSet<_>>();
  let mut nexts = BTreeSet::new();

  while !currs.is_empty() {
    for curr in &currs {
      let curr_val = target
        .get(curr)
        .expect("curr must already exist in target.");
      for (next_k, next_v) in proc.next(curr, curr_val) {
        match target.entry(next_k) {
          btree_map::Entry::Vacant(vac) => {
            nexts.insert(vac.key().clone());
            vac.insert(next_v);
          }
          btree_map::Entry::Occupied(mut occ) => {
            if matches!(proc.merge(occ.get_mut(), next_v), WasChanged::Changed)
            {
              nexts.insert(occ.key().clone());
            }
          }
        }
      }
    }
  }
}
