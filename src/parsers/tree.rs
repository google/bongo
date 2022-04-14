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

use crate::utils::{change_iter, WasChanged};
use im::Vector;
use std::collections::{BTreeMap, BTreeSet};

// Function to help compare raw reference pointers, instead of contents.
fn cmp_raw_refs<T>(a: &T, b: &T) -> std::cmp::Ordering {
  let a_raw = a as *const T;
  let b_raw = b as *const T;

  a_raw.cmp(&b_raw)
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct LeafData<T, V> {
  kind: T,
  value: Arc<V>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct BranchData<AK> {
  action: AK,
  nodes: Vector<usize>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum NodeContentInner<T, AK, V> {
  Leaf(LeafData<T, V>),
  Branch(BranchData<AK>),
}

impl<T, AK, V> NodeContentInner<T, AK, V> {
  pub fn as_leaf(&self) -> Option<&LeafData<T, V>> {
    match self {
      NodeContentInner::Leaf(leaf) => Some(leaf),
      NodeContentInner::Branch(_) => None,
    }
  }

  pub fn as_branch(&self) -> Option<&BranchData<AK>> {
    match self {
      NodeContentInner::Leaf(_) => None,
      NodeContentInner::Branch(branch) => Some(branch),
    }
  }
}

struct AltSet<T, AK, V> {
  value_to_id: BTreeMap<Arc<NodeContentInner<T, AK, V>>, usize>,
  id_to_value: Vec<Arc<NodeContentInner<T, AK, V>>>,
}

impl<T, AK, V> AltSet<T, AK, V> {
  pub fn new() -> Self {
    AltSet {
      value_to_id: BTreeMap::new(),
      id_to_value: Vec::new(),
    }
  }

  pub fn get(&self, index: usize) -> &NodeContentInner<T, AK, V> {
    self.id_to_value.get(index).unwrap_or_else(|| {
      panic!(
        "Invalid index: {} where alts list size is {}",
        index,
        self.id_to_value.len()
      )
    })
  }
}

impl<T, AK, V> AltSet<T, AK, V>
where
  T: Ord,
  AK: Ord,
  V: Ord,
{
  pub fn add(&mut self, content: NodeContentInner<T, AK, V>) -> usize {
    match self.value_to_id.get(&content) {
      Some(index) => *index,
      None => {
        let index = self.id_to_value.len();
        let arc = Arc::new(content);
        self.id_to_value.push(arc.clone());
        self.value_to_id.insert(arc, index);
        index
      }
    }
  }
}

#[derive(Clone)]
struct NodeData {
  alternatives: BTreeSet<usize>,
}

struct ListData {
  nodes: Vector<usize>,
}

struct Inner<T, AK, V> {
  nodes: Vec<NodeData>,
  alternatives: AltSet<T, AK, V>,
}

impl<T, AK, V> Inner<T, AK, V> {
  pub fn new() -> Self {
    Inner {
      nodes: Vec::new(),
      alternatives: AltSet::new(),
    }
  }

  pub fn get_node(&self, index: usize) -> &NodeData {
    self.nodes.get(index).unwrap_or_else(|| {
      panic!(
        "Invalid index: {} where nodes list size is {}",
        index,
        self.nodes.len(),
      )
    })
  }

  pub fn get_node_mut(&mut self, index: usize) -> &mut NodeData {
    let nodes_len = self.nodes.len();
    self.nodes.get_mut(index).unwrap_or_else(|| {
      panic!(
        "Invalid index: {} where nodes list size is {}",
        index, nodes_len,
      )
    })
  }

  pub fn get_alt(&self, index: usize) -> &NodeContentInner<T, AK, V> {
    self.alternatives.get(index)
  }
}

impl<T, AK, V> Inner<T, AK, V>
where
  T: Ord,
  AK: Ord,
  V: Ord,
{
  pub fn make_node(&mut self) -> usize {
    let index = self.nodes.len();
    self.nodes.push(NodeData {
      alternatives: BTreeSet::new(),
    });
    index
  }

  pub fn make_alt(&mut self, content: NodeContentInner<T, AK, V>) -> usize {
    self.alternatives.add(content)
  }

  pub fn add_node_alt(
    &mut self,
    node_index: usize,
    alt_index: usize,
  ) -> WasChanged {
    let node_data = self.get_node_mut(node_index);
    WasChanged::from_changed(node_data.alternatives.insert(alt_index))
  }

  pub fn is_same(&self, other: &Self) -> bool {
    std::ptr::eq(self, other)
  }
}

/// The owning object of an AST tree.
///
/// All elements of this tree are created under the lifetime of this tree
/// owner.
pub struct TreeOwner<T, AK, V> {
  inner: RwLock<Inner<T, AK, V>>,
}

impl<T, AK, V> Default for TreeOwner<T, AK, V>
where
  T: Ord,
  AK: Ord,
  V: Ord,
{
  fn default() -> Self {
    TreeOwner {
      inner: RwLock::new(Inner::new()),
    }
  }
}

impl<T, AK, V> TreeOwner<T, AK, V>
where
  T: Ord,
  AK: Ord,
  V: Ord,
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
  pub fn handle(&self) -> TreeHandle<T, AK, V> {
    TreeHandle(&self.inner)
  }
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct TreeHandle<'a, T, AK, V>(&'a RwLock<Inner<T, AK, V>>);

impl<T, AK, V> std::cmp::Ord for TreeHandle<'_, T, AK, V> {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    cmp_raw_refs(self.0, other.0)
  }
}

impl<T, AK, V> std::cmp::PartialOrd for TreeHandle<'_, T, AK, V> {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl<T, AK, V> std::cmp::PartialEq for TreeHandle<'_, T, AK, V> {
  fn eq(&self, other: &Self) -> bool {
    matches!(self.cmp(other), std::cmp::Ordering::Equal)
  }
}

impl<T, AK, V> std::cmp::Eq for TreeHandle<'_, T, AK, V> {}

impl<'a, T, AK, V> TreeHandle<'a, T, AK, V> {
  fn with<R, F>(&self, func: F) -> R
  where
    F: FnOnce(&Inner<T, AK, V>) -> R,
  {
    let guard = self.0.read().unwrap();
    func(&guard)
  }

  fn with_mut<R, F>(&self, func: F) -> R
  where
    F: FnOnce(&mut Inner<T, AK, V>) -> R,
  {
    let mut guard = self.0.write().unwrap();
    func(&mut guard)
  }
}

impl<'a, T, AK, V> TreeHandle<'a, T, AK, V>
where
  T: Ord,
  AK: Ord,
  V: Ord,
{
  pub fn make_node(&self) -> Node<'a, T, AK, V> {
    self.with_mut(|inner| {
      let index = inner.make_node();
      Node {
        index,
        inner: self.clone(),
      }
    })
  }

  pub fn make_leaf_node(&self, kind: T, value: V) -> Node<'a, T, AK, V> {
    let node = self.make_node();
    let leaf_alt = self.make_leaf_alt(kind, value);
    node.add_alt(&leaf_alt);
    node
  }

  pub fn make_branch_node(
    &self,
    action: AK,
    nodes: impl IntoIterator<Item = Node<'a, T, AK, V>>,
  ) -> Node<'a, T, AK, V> {
    let node = self.make_node();
    let branch_alt = self.make_branch_alt(action, nodes);
    node.add_alt(&branch_alt);
    node
  }

  fn make_alt(
    &self,
    content: NodeContentInner<T, AK, V>,
  ) -> Alternative<'a, T, AK, V> {
    self.with_mut(|inner| {
      let index = inner.make_alt(content);

      Alternative {
        index,
        inner: self.clone(),
      }
    })
  }

  pub fn make_leaf_alt(&self, kind: T, value: V) -> Alternative<'a, T, AK, V> {
    self.make_alt(NodeContentInner::Leaf(LeafData {
      kind,
      value: Arc::new(value),
    }))
  }

  pub fn make_branch_alt(
    &self,
    action: AK,
    nodes: impl IntoIterator<Item = Node<'a, T, AK, V>>,
  ) -> Alternative<T, AK, V> {
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
}

#[derive(Derivative)]
#[derivative(
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = "")
)]
pub struct Node<'a, T, AK, V> {
  index: usize,
  inner: TreeHandle<'a, T, AK, V>,
}

impl<'a, T, AK, V> Node<'a, T, AK, V> {
  pub fn handle(&self) -> TreeHandle<'a, T, AK, V> {
    self.inner.clone()
  }
}

impl<'a, T, AK, V> Node<'a, T, AK, V>
where
  T: Ord,
  AK: Ord,
  V: Ord,
{
  pub fn add_alt(&self, alt: &Alternative<'a, T, AK, V>) -> WasChanged {
    assert!(self.inner.is_same(&alt.inner));

    self
      .inner
      .with_mut(|inner| inner.add_node_alt(self.index, alt.index))
  }

  pub fn alts(&self) -> impl Iterator<Item = Alternative<'a, T, AK, V>> {
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

  /// Adds all alternatives from node other into this node. Does not modify the other node.
  pub fn add_all(&self, other: &Node<'a, T, AK, V>) -> WasChanged {
    assert!(self.inner.is_same(&other.inner));

    self.inner.with_mut(|inner| {
      let other_data = inner.get_node(other.index).clone();
      change_iter(other_data.alternatives.iter(), |other_alt| {
        inner.add_node_alt(self.index, *other_alt)
      })
    })
  }
}

impl<'a, T, AK, V> Node<'a, T, AK, V>
where
  T: Ord + std::fmt::Debug,
  AK: Ord + std::fmt::Debug,
  V: Ord + std::fmt::Debug,
{
  pub fn to_dot(&self) -> String {
    let mut vec_cursor = std::io::Cursor::new(Vec::new());
    dot::render(&DotPrinter::new(self), &mut vec_cursor).unwrap();
    String::from_utf8(vec_cursor.into_inner()).unwrap()
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
pub struct Alternative<'a, T, AK, V> {
  index: usize,
  inner: TreeHandle<'a, T, AK, V>,
}

impl<'a, T, AK, V> Alternative<'a, T, AK, V>
where
  T: Ord,
  AK: Ord,
  V: Ord,
{
  pub fn handle(&self) -> TreeHandle<'a, T, AK, V> {
    self.inner.clone()
  }

  pub fn content(&self) -> NodeContent<'a, T, AK, V> {
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
pub struct Leaf<'a, T, AK, V> {
  index: usize,
  inner: TreeHandle<'a, T, AK, V>,
}

impl<'a, T, AK, V> Leaf<'a, T, AK, V>
where
  T: Ord + Clone,
  AK: Ord,
  V: Ord,
{
  pub fn handle(&self) -> TreeHandle<'a, T, AK, V> {
    self.inner.clone()
  }

  fn with_leaf<F, R>(&self, func: F) -> R
  where
    F: FnOnce(&LeafData<T, V>) -> R,
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

  pub fn kind(&self) -> T {
    self.with_leaf(|leaf| leaf.kind.clone())
  }

  pub fn value(&self) -> Arc<V> {
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
pub struct Branch<'a, T, AK, V> {
  index: usize,
  inner: TreeHandle<'a, T, AK, V>,
}

impl<'a, T, AK, V> Branch<'a, T, AK, V>
where
  T: Ord,
  AK: Ord + Clone,
  V: Ord,
{
  pub fn handle(&self) -> TreeHandle<'a, T, AK, V> {
    self.inner.clone()
  }

  fn with_branch<F, R>(&self, func: F) -> R
  where
    F: FnOnce(&BranchData<AK>) -> R,
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

  pub fn action(&self) -> AK {
    self.with_branch(|branch| branch.action.clone())
  }

  pub fn nodes(&self) -> impl Iterator<Item = Node<'a, T, AK, V>> {
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
pub enum NodeContent<'a, T, AK, V> {
  Leaf(Leaf<'a, T, AK, V>),
  Branch(Branch<'a, T, AK, V>),
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
enum DotNode {
  Node(usize),
  Alt(usize),
}

fn collect_nodes_and_alts<T, AK, V: Ord>(
  node: &Node<T, AK, V>,
) -> (Vec<usize>, Vec<usize>)
where
  T: Ord,
  AK: Ord,
{
  let handle = node.handle();

  let mut seen = BTreeSet::new();
  let mut curr = BTreeSet::new();
  let mut next = BTreeSet::new();

  curr.insert(DotNode::Node(node.index));

  while !curr.is_empty() {
    for curr_item in &curr {
      match curr_item {
        DotNode::Node(node_id) => handle.with(|inner| {
          let node_data = inner.get_node(*node_id);
          for alt_id in &node_data.alternatives {
            if seen.insert(DotNode::Alt(*alt_id)) {
              next.insert(DotNode::Alt(*alt_id));
            }
          }
        }),
        DotNode::Alt(alt_id) => handle.with(|inner| {
          if let NodeContentInner::Branch(branch_data) = inner.get_alt(*alt_id)
          {
            for node_id in &branch_data.nodes {
              if seen.insert(DotNode::Node(*node_id)) {
                next.insert(DotNode::Node(*node_id));
              }
            }
          }
        }),
      }
    }

    curr.clear();
    std::mem::swap(&mut curr, &mut next);
  }

  let mut nodes = Vec::new();
  let mut alts = Vec::new();

  for item in seen {
    match item {
      DotNode::Node(node_id) => nodes.push(node_id),
      DotNode::Alt(alt_id) => alts.push(alt_id),
    }
  }
  (nodes, alts)
}

struct DotPrinter<'a, T, AK, V> {
  handle: TreeHandle<'a, T, AK, V>,
  nodes: Vec<usize>,
  alts: Vec<usize>,
  root_node: usize,
}

impl<'a, T, AK, V> DotPrinter<'a, T, AK, V>
where
  T: Ord,
  AK: Ord,
  V: Ord,
{
  pub fn new(node: &Node<'a, T, AK, V>) -> Self {
    let handle = node.handle().clone();
    let (nodes, alts) = collect_nodes_and_alts(node);
    DotPrinter {
      handle,
      nodes,
      alts,
      root_node: node.index,
    }
  }
}

type DotEdge = (DotNode, DotNode);

impl<'a, 'b: 'a, T, AK, V> dot::GraphWalk<'a, DotNode, DotEdge>
  for DotPrinter<'b, T, AK, V>
where
  T: Ord,
  AK: Ord,
  V: Ord,
{
  fn nodes(&'a self) -> dot::Nodes<'a, DotNode> {
    self
      .nodes
      .iter()
      .copied()
      .map(DotNode::Node)
      .chain(self.alts.iter().copied().map(DotNode::Alt))
      .collect()
  }

  fn edges(&'a self) -> dot::Edges<'a, DotEdge> {
    let node_edges = self.nodes.iter().copied().flat_map(|node_id| {
      self.handle.with(|i| {
        i.get_node(node_id)
          .alternatives
          .iter()
          .copied()
          .map(|alt_id| (DotNode::Node(node_id), DotNode::Alt(alt_id)))
          .collect::<Vec<_>>()
      })
    });

    let alt_edges = self.alts.iter().copied().flat_map(|alt_id| {
      self.handle.with(|i| {
        if let NodeContentInner::Branch(branch_data) = i.get_alt(alt_id) {
          branch_data
            .nodes
            .iter()
            .copied()
            .map(|node_id| (DotNode::Alt(alt_id), DotNode::Node(node_id)))
            .collect::<Vec<_>>()
        } else {
          Vec::new()
        }
      })
    });

    node_edges.chain(alt_edges).collect()
  }

  fn source(&'a self, edge: &DotEdge) -> DotNode {
    edge.0
  }

  fn target(&'a self, edge: &DotEdge) -> DotNode {
    edge.1
  }
}

impl<'a, 'b: 'a, T, AK, V> dot::Labeller<'a, DotNode, DotEdge>
  for DotPrinter<'b, T, AK, V>
where
  T: std::fmt::Debug,
  AK: std::fmt::Debug,
  V: std::fmt::Debug,
{
  fn graph_id(&'a self) -> dot::Id<'a> {
    dot::Id::new("G").unwrap()
  }

  fn node_id(&'a self, n: &DotNode) -> dot::Id<'a> {
    match n {
      DotNode::Node(node_id) => dot::Id::new(format!("n{}", node_id)).unwrap(),
      DotNode::Alt(alt_id) => dot::Id::new(format!("a{}", alt_id)).unwrap(),
    }
  }

  fn node_shape(&'a self, node: &DotNode) -> Option<dot::LabelText<'a>> {
    match node {
      DotNode::Node(_) => Some(dot::LabelText::label("point")),
      DotNode::Alt(_) => Some(dot::LabelText::label("box")),
    }
  }

  fn node_label(&'a self, node: &DotNode) -> dot::LabelText<'a> {
    match node {
      DotNode::Node(_) => dot::LabelText::label(""),
      DotNode::Alt(alt_id) => self.handle.with(|i| match i.get_alt(*alt_id) {
        NodeContentInner::Branch(branch_data) => {
          dot::LabelText::label(format!("{:?}", branch_data.action))
        }
        NodeContentInner::Leaf(leaf_data) => dot::LabelText::label(format!(
          "{:?}({:?})",
          leaf_data.kind, leaf_data.value
        )),
      }),
    }
  }
}
