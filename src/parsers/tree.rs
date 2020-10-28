// Copyright 2020 Google LLC
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

use std::sync::{Arc, RwLock};

use crate::ElementTypes;
use im::Vector;
use std::collections::{BTreeMap, BTreeSet};

// Function to help compare raw reference pointers, instead of contents.
fn cmp_raw_refs<T>(a: &T, b: &T) -> std::cmp::Ordering {
  let a_raw = a as *const T;
  let b_raw = b as *const T;

  a_raw.cmp(&b_raw)
}

#[derive(Derivative)]
#[derivative(
  PartialEq(bound = "T: PartialEq"),
  Eq(bound = "T: Eq"),
  PartialOrd(bound = "T: PartialOrd"),
  Ord(bound = "T: Ord")
)]
struct LeafData<E: ElementTypes, T> {
  kind: E::Term,
  value: Arc<T>,
}

#[derive(Derivative)]
#[derivative(
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = "")
)]
struct BranchData<E: ElementTypes> {
  action: E::ActionKey,
  nodes: Vector<usize>,
}

#[derive(Derivative)]
#[derivative(
  PartialEq(bound = "T: PartialEq"),
  Eq(bound = "T: Eq"),
  PartialOrd(bound = "T: PartialOrd"),
  Ord(bound = "T: Ord"),
  PartialOrd = "feature_allow_slow_enum",
  Ord = "feature_allow_slow_enum"
)]
enum NodeContentInner<E: ElementTypes, T> {
  Leaf(LeafData<E, T>),
  Branch(BranchData<E>),
}

impl<E: ElementTypes, T> NodeContentInner<E, T> {
  pub fn as_leaf(&self) -> Option<&LeafData<E, T>> {
    match self {
      NodeContentInner::Leaf(leaf) => Some(leaf),
      NodeContentInner::Branch(_) => None,
    }
  }

  pub fn as_branch(&self) -> Option<&BranchData<E>> {
    match self {
      NodeContentInner::Leaf(_) => None,
      NodeContentInner::Branch(branch) => Some(branch),
    }
  }
}

struct NodeData {
  alternatives: BTreeSet<usize>,
}

struct ListData {
  nodes: Vector<usize>,
}

struct Inner<E: ElementTypes, T> {
  nodes: Vec<NodeData>,
  alternatives: BTreeMap<Arc<NodeContentInner<E, T>>, usize>,
  alts_by_id: Vec<Arc<NodeContentInner<E, T>>>,
}

impl<E: ElementTypes, T> Inner<E, T>
where
  T: Ord,
{
  pub fn new() -> Self {
    Inner {
      nodes: Vec::new(),
      alternatives: BTreeMap::new(),
      alts_by_id: Vec::new(),
    }
  }

  pub fn make_node(&mut self) -> usize {
    let index = self.nodes.len();
    self.nodes.push(NodeData {
      alternatives: BTreeSet::new(),
    });
    index
  }

  pub fn make_alt(&mut self, content: NodeContentInner<E, T>) -> usize {
    match self.alternatives.get(&content) {
      Some(index) => *index,
      None => {
        let index = self.alts_by_id.len();
        let arc = Arc::new(content);
        self.alts_by_id.push(arc.clone());
        self.alternatives.insert(arc, index);
        index
      }
    }
  }

  pub fn add_node_alt(&mut self, node_index: usize, alt_index: usize) -> bool {
    let node_data = self.get_node_mut(node_index);
    node_data.alternatives.insert(alt_index)
  }

  pub fn is_same(&self, other: &Self) -> bool {
    (self as *const Inner<E, T>) == (other as *const Inner<E, T>)
  }

  pub fn get_node(&self, index: usize) -> &NodeData {
    self.nodes.get(index).expect(&format!(
      "Invalid index: {} where nodes list size is {}",
      index,
      self.nodes.len(),
    ))
  }

  pub fn get_node_mut(&mut self, index: usize) -> &mut NodeData {
    let nodes_len = self.nodes.len();
    self.nodes.get_mut(index).expect(&format!(
      "Invalid index: {} where nodes list size is {}",
      index, nodes_len,
    ))
  }

  pub fn get_alt(&self, index: usize) -> &NodeContentInner<E, T> {
    self.alts_by_id.get(index).expect(&format!(
      "Invalid index: {} where alts list size is {}",
      index,
      self.alternatives.len()
    ))
  }
}

/// The owning object of an AST tree.
///
/// All elements of this tree are created under the lifetime of this tree
/// owner.
pub struct TreeOwner<E: ElementTypes, T> {
  inner: RwLock<Inner<E, T>>,
}

impl<E: ElementTypes, T> TreeOwner<E, T>
where
  T: Ord,
{
  /// Creates a new, empty tree with no nodes.
  pub fn new() -> Self {
    TreeOwner {
      inner: RwLock::new(Inner::new()),
    }
  }

  /// Returns a TreeHandle for this owner.
  ///
  /// This can be used to create new nodes and alternatives.
  pub fn handle<'a>(&'a self) -> TreeHandle<'a, E, T> {
    TreeHandle(&self.inner)
  }
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct TreeHandle<'a, E: ElementTypes, T>(&'a RwLock<Inner<E, T>>);

impl<E: ElementTypes, T> std::cmp::Ord for TreeHandle<'_, E, T> {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    cmp_raw_refs(self.0, other.0)
  }
}

impl<E: ElementTypes, T> std::cmp::PartialOrd for TreeHandle<'_, E, T> {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl<E: ElementTypes, T> std::cmp::PartialEq for TreeHandle<'_, E, T> {
  fn eq(&self, other: &Self) -> bool {
    matches!(self.cmp(other), std::cmp::Ordering::Equal)
  }
}

impl<E: ElementTypes, T> std::cmp::Eq for TreeHandle<'_, E, T> {}

impl<'a, E: ElementTypes, T> TreeHandle<'a, E, T>
where
  T: Ord,
{
  pub fn make_node(&self) -> Node<'a, E, T> {
    self.with_mut(|inner| {
      let index = inner.make_node();
      Node {
        index,
        inner: self.clone(),
      }
    })
  }

  fn make_alt(&self, content: NodeContentInner<E, T>) -> Alternative<'a, E, T> {
    self.with_mut(|inner| {
      let index = inner.make_alt(content);

      Alternative {
        index,
        inner: self.clone(),
      }
    })
  }

  pub fn make_leaf_alt(&self, kind: E::Term, value: T) -> Alternative<'a, E, T> {
    self.make_alt(NodeContentInner::Leaf(LeafData {
      kind,
      value: Arc::new(value),
    }))
  }

  pub fn make_branch_alt(
    &self,
    action: E::ActionKey,
    nodes: impl IntoIterator<Item = Node<'a, E, T>>,
  ) -> Alternative<E, T> {
    let node_indexes = self.with(|inner| {
      nodes
        .into_iter()
        .inspect(|n| {
          // Check that all nodes are coming from the same inner instance.
          assert!(n.inner.with(|n_inner| { inner.is_same(n_inner) }));
        })
        .map(|n| n.index)
        .collect()
    });

    self.make_alt(NodeContentInner::Branch(BranchData {
      action,
      nodes: node_indexes,
    }))
  }

  pub fn is_same(&self, other: &Self) -> bool {
    self.with(|self_inner| {
      other.with(|other_inner| self_inner.is_same(other_inner))
    })
  }

  fn with<R, F>(&self, func: F) -> R
  where
    F: FnOnce(&Inner<E, T>) -> R,
  {
    let guard = self.0.read().unwrap();
    func(&guard)
  }

  fn with_mut<R, F>(&self, func: F) -> R
  where
    F: FnOnce(&mut Inner<E, T>) -> R,
  {
    let mut guard = self.0.write().unwrap();
    func(&mut guard)
  }
}

#[derive(Derivative)]
#[derivative(
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = "")
)]
pub struct Node<'a, E: ElementTypes, T> {
  index: usize,
  inner: TreeHandle<'a, E, T>,
}

impl<'a, E: ElementTypes, T> Node<'a, E, T>
where
  T: Ord,
{
  pub fn handle(&self) -> TreeHandle<'a, E, T> {
    self.inner.clone()
  }

  pub fn add_alt(&self, alt: &Alternative<'a, E, T>) -> bool {
    assert!(self.inner.is_same(&alt.inner));

    self.inner.with_mut(|inner| {
      inner.add_node_alt(self.index, alt.index)
    })
  }

  pub fn alts(&self) -> impl Iterator<Item = Alternative<'a, E, T>> {
    let alt_indexes = self
      .inner
      .with(|inner| inner.get_node(self.index).alternatives.clone());

    alt_indexes.into_iter().map({
      let inner = self.inner.clone();
      move |index| Alternative {
        index,
        inner: inner.clone(),
      }
    })
  }
}

#[derive(Derivative)]
#[derivative(
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = "")
)]
pub struct Alternative<'a, E: ElementTypes, T> {
  index: usize,
  inner: TreeHandle<'a, E, T>,
}

impl<'a, E: ElementTypes, T> Alternative<'a, E, T>
where
  T: Ord,
{
  pub fn handle(&self) -> TreeHandle<'a, E, T> {
    self.inner.clone()
  }

  pub fn content(&self) -> NodeContent<'a, E, T> {
    self.inner.with(|inner| match inner.get_alt(self.index) {
      NodeContentInner::Leaf(_) => NodeContent::Leaf(Leaf {
        index: self.index,
        inner: self.inner.clone(),
      }),
      NodeContentInner::Branch(_) => NodeContent::Branch(Branch {
        index: self.index,
        inner: self.inner.clone(),
      }),
    })
  }
}

#[derive(Derivative)]
#[derivative(
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = "")
)]
pub struct Leaf<'a, E: ElementTypes, T> {
  index: usize,
  inner: TreeHandle<'a, E, T>,
}

impl<'a, E: ElementTypes, T> Leaf<'a, E, T>
where
  T: Ord,
{
  pub fn handle(&self) -> TreeHandle<'a, E, T> {
    self.inner.clone()
  }

  fn with_leaf<F, R>(&self, func: F) -> R
  where
    F: FnOnce(&LeafData<E, T>) -> R,
  {
    self.inner.with(|inner| {
      func(
        inner
          .get_alt(self.index)
          .as_leaf()
          .expect("Found branch where leaf expected."),
      )
    })
  }

  pub fn kind(&self) -> E::Term {
    self.with_leaf(|leaf| leaf.kind.clone())
  }

  pub fn value(&self) -> Arc<T> {
    self.with_leaf(|leaf| leaf.value.clone())
  }
}

#[derive(Derivative)]
#[derivative(
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = "")
)]
pub struct Branch<'a, E: ElementTypes, T> {
  index: usize,
  inner: TreeHandle<'a, E, T>,
}

impl<'a, E: ElementTypes, T> Branch<'a, E, T>
where
  T: Ord,
{
  pub fn handle(&self) -> TreeHandle<'a, E, T> {
    self.inner.clone()
  }

  fn with_branch<F, R>(&self, func: F) -> R
  where
    F: FnOnce(&BranchData<E>) -> R,
  {
    self.inner.with(|inner| {
      func(
        inner
          .get_alt(self.index)
          .as_branch()
          .expect("Found leaf where branch expected."),
      )
    })
  }

  pub fn action(&self) -> E::ActionKey {
    self.with_branch(|branch| branch.action.clone())
  }

  pub fn nodes(&self) -> impl Iterator<Item = Node<'a, E, T>> {
    let node_indexes = self.with_branch(|branch| branch.nodes.clone());

    node_indexes.into_iter().map({
      let inner = self.inner.clone();
      move |index| Node {
        index,
        inner: inner.clone(),
      }
    })
  }
}

#[derive(Derivative)]
#[derivative(
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = ""),
  PartialOrd = "feature_allow_slow_enum",
  Ord = "feature_allow_slow_enum"
)]
pub enum NodeContent<'a, E: ElementTypes, T> {
  Leaf(Leaf<'a, E, T>),
  Branch(Branch<'a, E, T>),
}
