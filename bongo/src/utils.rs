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

use std::collections::{BTreeMap, BTreeSet};

use crate::pdisplay::LayoutDisplay;
use codefmt::Layout;

pub mod buffer;

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

/// A trait for ordered keys. Keys must be clonable, fully ordered,
pub trait OrdKey:
  Clone
  + PartialEq
  + Eq
  + PartialOrd
  + Ord
  + LayoutDisplay
  + std::fmt::Debug
  + 'static
{
}

/// A refcounted name type, used to avoid duplicating common string values
/// throughout an AST.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
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

  pub fn layout(&self) -> codefmt::Layout {
    Layout::text(self.str())
  }
}

impl AsRef<str> for Name {
  fn as_ref(&self) -> &str {
    return self.str();
  }
}

impl LayoutDisplay for Name {
  fn disp(&self) -> codefmt::Layout {
    Layout::text(self.0.as_ref())
  }
}

impl OrdKey for Name {}

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
