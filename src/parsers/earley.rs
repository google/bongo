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

//! A dumb earley parser for any implementation of Grammar.
//!
//! This is not intended to be a high-performance implementation of a bongo
//! grammar, but a baseline that can be validated against.

use {
  crate::{
    grammar::{Element, Prod, ProductionElement},
    parsers::tree::{Node, NodeList},
    state::ProdState,
    ElementTypes,
  },
  im::Vector,
  std::collections::BTreeMap,
};

struct ParserImpl<'a, E: ElementTypes, T> {
  states: Vec<EarleyState<'a, E, T>>,
}

struct EarleyState<'a, E: ElementTypes, T> {
  state: BTreeMap<ProdState<'a, E>, Vector<NodeList<E, T>>>,
}

impl<'a, E: ElementTypes, T> EarleyState<'a, E, T> {
  pub fn new() -> Self {
    EarleyState {
      state: BTreeMap::new(),
    }
  }

  fn take_closure(&mut self) {
    for (prod_state, values) in self.state.iter() {
      if prod_state.is_complete() {
        prod_state
      }
    }
  }

  pub fn is_empty(&self) -> bool {
    self.state.is_empty()
  }

  pub fn add_prod(&mut self, prod: Prod<'a, E>) -> bool {
    self
      .state
      .insert(ProdState::from_start(prod), im::vector![NodeList::new()])
      .is_none()
  }

  pub fn nexts<'b: 'a>(&'b self) -> impl Iterator<Item = Element<E>> + 'b {
    self
      .state
      .keys()
      .filter_map(ProdState::next_elem)
      .map(ProductionElement::elem)
      .cloned()
  }

  pub fn advance_on(&self, elem: &Element<E>, node: &Node<E, T>) -> Self {
    let mut new_state = BTreeMap::new();
    for (k, v) in self.state.iter() {
      if let Some(next_prod_state) = k.advance_if(elem) {
        let mut new_values = v.clone();
        for value in new_values.iter_mut() {
          value.push(node.clone());
        }
        new_state.insert(next_prod_state, new_values);
      }
    }
    todo!()
  }
}
