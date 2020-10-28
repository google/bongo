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
    grammar::{Element, Grammar, Prod, ProductionElement},
    parsers::tree2::{Node, TreeHandle},
    state::ProdState,
    ElementTypes,
  },
  std::collections::{btree_map, BTreeMap, BTreeSet},
};

struct ParserImpl<'a, E: ElementTypes, T> {
  states: Vec<EarleyState<'a, E, T>>,
}

struct EarleyState<'a, E: ElementTypes, T> {
  state: BTreeMap<ProdState<'a, E>, Vec<Node<'a, E, T>>>,
}

impl<'a, E: ElementTypes, T> EarleyState<'a, E, T>
where
  T: Ord,
{
  pub fn new() -> Self {
    EarleyState {
      state: BTreeMap::new(),
    }
  }

  fn take_one_prod_closure(&mut self, g: &'a Grammar<E>) -> bool {
    let mut changed = false;
    let mut new_prods = BTreeSet::new();
    for prod_state in self.state.keys() {
      if let Some(next_elem) = prod_state.next_elem() {
        if let Element::NonTerm(nt) = next_elem.elem() {
          let rule = g.get_rule(nt).unwrap();
          for prod in rule.prods() {
            new_prods.insert(prod);
          }
        }
      }
    }

    for new_prod in new_prods {
      let new_state = ProdState::from_start(new_prod);
      if let btree_map::Entry::Vacant(vac) = self.state.entry(new_state) {
        vac.insert(Vec::new());
        changed = true;
      }
    }

    changed
  }

  fn take_one_reduce_closure(
    &mut self,
    tree_handle: &TreeHandle<'a, E, T>,
  ) -> bool {
    let mut changed = false;
    let mut complete_alts = BTreeMap::new();

    for (prod_state, values) in self.state.iter() {
      if prod_state.is_complete() {
        let prod = prod_state.prod();
        let alt = tree_handle
          .make_branch_alt(prod.action_key().clone(), values.clone());

        complete_alts
          .entry(prod.head().clone())
          .or_insert_with(Vec::new)
          .push(alt);
      }
    }

    let mut alts_by_next_state = BTreeMap::new();

    for prod_state in self.state.keys() {
      if let Some((next_elem, next_state)) = prod_state.next_elem_state() {
        if let Element::NonTerm(nt) = next_elem.elem() {
          if let Some(alt_vec) = complete_alts.get(&nt) {
            alts_by_next_state
              .entry(next_state)
              .or_insert_with(|| BTreeSet::new())
              .extend(alt_vec);
          }
        }
      }
    }

    for (next_state, alt_set) in alts_by_next_state {
      if let Some(values) = self.state.get_mut(&next_state) {
        for alt in alt_set {
          changed = values.last().unwrap().add_alt(alt) || changed;
        }
      }
    }

    changed
  }

  pub fn take_closure(
    &mut self,
    g: &'a Grammar<E>,
    tree_handle: &TreeHandle<'a, E, T>,
  ) {
    loop {
      let changed = self.take_one_prod_closure(g)
        || self.take_one_reduce_closure(tree_handle);

      if !changed {
        break;
      }
    }
  }

  pub fn shift(&self, tree_handle: TreeHandle<'a, E, T>, kind: E::Term, token: T) -> EarleyState<'a, E, T> {
    let node = tree_handle.make_node();
    node.add_alt(&tree_handle.make_leaf_alt(kind.clone(), token));
    let result = self.state.iter().filter_map(|(ps, v)| {
      ps.advance_if(&Element::Term(kind.clone()))
        .map(|nps| {
          let mut nv : Vec<_> = v.clone();
          nv.push(node.clone());
          (nps, nv)
        })
    }).collect();

    EarleyState {
      state: result,
    }
  }

  pub fn is_empty(&self) -> bool {
    self.state.is_empty()
  }

  pub fn add_prod(&mut self, prod: Prod<'a, E>) -> bool {
    self
      .state
      .insert(ProdState::from_start(prod), Vec::new())
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

  pub fn advance_on(&self, elem: &Element<E>, node: &Node<'a, E, T>) -> Self {
    let mut new_state = BTreeMap::new();
    for (k, v) in self.state.iter() {
      if let Some(next_prod_state) = k.advance_if(elem) {
        let mut new_values = v.clone();
        new_values.push(node.clone());
        new_state.insert(next_prod_state, new_values);
      }
    }
    todo!()
  }
}
