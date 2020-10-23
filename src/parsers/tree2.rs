use std::sync::{Arc, RwLock, Weak};

use crate::ElementTypes;
use im::Vector;
use std::collections::BTreeSet;

struct LeafData<E: ElementTypes, T> {
  kind: E::Term,
  value: Arc<T>,
}

struct BranchData<E: ElementTypes> {
  action: E::ActionKey,
  nodes: Vector<usize>,
}

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
  alternatives: Vec<NodeContentInner<E, T>>,
}

impl<E: ElementTypes, T> Inner<E, T> {
  pub fn new() -> Self {
    Inner {
      nodes: Vec::new(),
      alternatives: Vec::new(),
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
    let index = self.alternatives.len();
    self.alternatives.push(content);
    index
  }

  pub fn add_node_alt(&mut self, node_index: usize, alt_index: usize) {
    let node_data = self.get_node_mut(node_index);
    node_data.alternatives.insert(alt_index);
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
    self.alternatives.get(index).expect(&format!(
      "Invalid index: {} where alts list size is {}",
      index,
      self.alternatives.len()
    ))
  }
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
struct InnerHandle<E: ElementTypes, T>(Weak<RwLock<Inner<E, T>>>);

impl<E: ElementTypes, T> InnerHandle<E, T> {
  pub fn is_same(&self, other: &Self) -> bool {
    self.with(|self_inner| {
      other.with(|other_inner| self_inner.is_same(other_inner))
    })
  }

  fn with_upgraded<R, F>(&self, func: F) -> R
  where
    F: FnOnce(&Arc<RwLock<Inner<E, T>>>) -> R,
  {
    func(
      &self
        .0
        .upgrade()
        .expect("Lost the TreeHandle for this tree."),
    )
  }

  pub fn with<R, F>(&self, func: F) -> R
  where
    F: FnOnce(&Inner<E, T>) -> R,
  {
    self.with_upgraded(|inner| {
      let guard = inner.read().unwrap();
      func(&guard)
    })
  }

  pub fn with_mut<R, F>(&self, func: F) -> R
  where
    F: FnOnce(&mut Inner<E, T>) -> R,
  {
    self.with_upgraded(|inner| {
      let mut guard = inner.write().unwrap();
      func(&mut guard)
    })
  }
}

/// The owning object of an AST tree.
///
/// Due to the possibility of alternatives, an AST Tree can have cycles in it.
/// To avoid memory leaks, this object owns all of the node data. When this
/// object is dropped, all Node, Alternative, Leaf, and Branch objects that
/// came from this will panic if any methods are called on them, but may be
/// safely dropped.
pub struct TreeHandle<E: ElementTypes, T> {
  inner: Arc<RwLock<Inner<E, T>>>,
}

impl<E: ElementTypes, T> TreeHandle<E, T> {
  pub fn new() -> Self {
    TreeHandle {
      inner: Arc::new(RwLock::new(Inner::new())),
    }
  }

  fn inner_handle(&self) -> InnerHandle<E, T> {
    InnerHandle(Arc::downgrade(&self.inner))
  }

  pub fn make_node(&self) -> Node<E, T> {
    let mut guard = self.inner.write().unwrap();
    let index = guard.make_node();
    Node {
      index,
      inner: self.inner_handle(),
    }
  }

  fn make_alt(&self, content: NodeContentInner<E, T>) -> Alternative<E, T> {
    let mut guard = self.inner.write().unwrap();
    let index = guard.make_alt(content);

    Alternative {
      index,
      inner: self.inner_handle(),
    }
  }

  pub fn make_leaf_alt(&self, kind: E::Term, value: T) -> Alternative<E, T> {
    self.make_alt(NodeContentInner::Leaf(LeafData {
      kind,
      value: Arc::new(value),
    }))
  }

  fn has_same_inner(&self, other: &InnerHandle<E, T>) -> bool {
    let guard = self.inner.read().unwrap();
    other.with(|other_inner| guard.is_same(other_inner))
  }

  pub fn make_branch_alt(
    &self,
    action: E::ActionKey,
    nodes: impl IntoIterator<Item = Node<E, T>>,
  ) -> Alternative<E, T> {
    let s_guard = self.inner.read().unwrap();
    let node_indexes = nodes
      .into_iter()
      .inspect(|n| {
        // Check that all nodes are coming from the same inner instance.
        assert!(n.inner.with(|n_inner| { s_guard.is_same(n_inner) }));
      })
      .map(|n| n.index)
      .collect();

    self.make_alt(NodeContentInner::Branch(BranchData {
      action,
      nodes: node_indexes,
    }))
  }
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Node<E: ElementTypes, T> {
  index: usize,
  inner: InnerHandle<E, T>,
}

impl<E: ElementTypes, T> Node<E, T> {
  pub fn add_alt(&self, alt: &Alternative<E, T>) {
    assert!(self.inner.is_same(&alt.inner));

    self.inner.with_mut(|inner| {
      inner.add_node_alt(self.index, alt.index);
    })
  }

  pub fn alts(&self) -> impl Iterator<Item = Alternative<E, T>> {
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
#[derivative(Clone(bound = ""))]
pub struct Alternative<E: ElementTypes, T> {
  index: usize,
  inner: InnerHandle<E, T>,
}

impl<E: ElementTypes, T> Alternative<E, T> {
  pub fn content(&self) -> NodeContent<E, T> {
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

pub struct Leaf<E: ElementTypes, T> {
  index: usize,
  inner: InnerHandle<E, T>,
}

impl<E: ElementTypes, T> Leaf<E, T> {
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

pub struct Branch<E: ElementTypes, T> {
  index: usize,
  inner: InnerHandle<E, T>,
}

impl<E: ElementTypes, T> Branch<E, T> {
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

  pub fn nodes(&self) -> impl Iterator<Item = Node<E, T>> {
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

pub enum NodeContent<E: ElementTypes, T> {
  Leaf(Leaf<E, T>),
  Branch(Branch<E, T>),
}
