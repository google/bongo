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

use std::sync::Arc;

use im::Vector;

use crate::ElementTypes;

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = "T: std::fmt::Debug"))]
pub struct Leaf<E: ElementTypes, T> {
  kind: E::Term,
  value: Arc<T>,
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = "T: std::fmt::Debug"))]
pub struct Branch<E: ElementTypes, T> {
  kind: E::NonTerm,
  action: E::ActionKey,
  edges: NodeList<E, T>,
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = "T: std::fmt::Debug"))]
pub struct Node<E: ElementTypes, T>(Vector<NodeContents<E, T>>);

impl<E: ElementTypes, T> Node<E, T> {
  fn from_contents(contents: NodeContents<E, T>) -> Self {
    Node(Vector::unit(contents))
  }
  pub fn new_leaf(kind: E::Term, value: T) -> Self {
    Node::from_contents(NodeContents::Leaf(Leaf {
      kind,
      value: Arc::new(value),
    }))
  }

  pub fn new_branch(
    kind: E::NonTerm,
    action: E::ActionKey,
    edges: impl IntoIterator<Item = Node<E, T>>,
  ) -> Self {
    Node::from_contents(NodeContents::Branch(Branch {
      kind,
      action,
      edges: NodeList::from_iter(edges.into_iter()),
    }))
  }

  pub fn alternatives(&self) -> impl Iterator<Item = &NodeContents<E, T>> {
    self.0.iter()
  }

  pub fn merge(&self, other: &Self) -> Self {
    let mut new_alts = self.0.clone();
    new_alts.append(other.0.clone());
    Node(new_alts)
  }
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = "T: std::fmt::Debug"))]
pub enum NodeContents<E: ElementTypes, T> {
  Leaf(Leaf<E, T>),
  Branch(Branch<E, T>),
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = "T: std::fmt::Debug"))]
pub struct NodeList<E: ElementTypes, T>(Vector<Node<E, T>>);

impl<E: ElementTypes, T> std::ops::Deref for NodeList<E, T> {
  type Target = Vector<Node<E, T>>;

  fn deref(&self) -> &Vector<Node<E, T>> {
    &self.0
  }
}

impl<E: ElementTypes, T> NodeList<E, T> {
  pub fn new() -> Self {
    NodeList(Vector::new())
  }

  pub fn from_iter(iter: impl Iterator<Item = Node<E, T>>) -> Self {
    NodeList(iter.collect())
  }

  pub fn push(&mut self, elem: Node<E, T>) {
    self.0.push_back(elem);
  }
}