// Copyright 2018 Google LLC
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

use crate::grammar::{Elem, ElemTypes, Prod, ProdElement};
use std::{
  collections::{BTreeMap, BTreeSet},
  iter::FromIterator,
};

/// A state of a production within a parse state.
///
/// A production state keeps track of a particular production, the nonterminal
/// to which the production belongs, and an index into the production, which is
/// the current location of the parse state. For example:
///
/// ```text
/// A => a <b> . c
/// ```
///
/// This indicates that the head is A, the production is a <b> c, and the
/// current location is just before the final c.
#[derive(Derivative)]
#[derivative(
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = ""),
  Debug(bound = "")
)]

pub struct ProdState<'a, E: ElemTypes> {
  /// The production this state is part of.
  prod: Prod<'a, E>,

  /// The index of this production state. Must be in the range [0,
  /// self.prod.prod_elements().len()].
  index: usize,
}

impl<'a, E: ElemTypes> ProdState<'a, E> {
  /// Create a ProdState from a given NonTerminal and Prod.
  ///
  /// This state's index will be at the start of the production.
  pub fn from_start(prod: Prod<'a, E>) -> Self {
    ProdState { prod, index: 0 }
  }

  pub fn prod(&self) -> Prod<'a, E> {
    self.prod
  }

  pub fn offset_prod_elem(&self, i: usize) -> Option<&'a ProdElement<E>> {
    self.prod.prod_elements().get(self.index + i)
  }

  pub fn offset_elem(&self, i: usize) -> Option<&'a Elem<E>> {
    self.offset_prod_elem(i).map(ProdElement::elem)
  }

  pub fn next_prod_elem(&self) -> Option<&'a ProdElement<E>> {
    self.offset_prod_elem(0)
  }

  pub fn next_elem(&self) -> Option<&'a Elem<E>> {
    self.offset_elem(0)
  }

  /// Returns the next element after the current index. If it is at the
  /// end, then it reuturns `None`.
  pub fn next_elem_state(
    &self,
  ) -> Option<(&'a ProdElement<E>, ProdState<'a, E>)> {
    self.next_prod_elem().map(|prod_elem| {
      (
        prod_elem,
        ProdState {
          prod: self.prod,
          index: self.index + 1,
        },
      )
    })
  }

  /// Return Some(state) which is this state advanced if
  /// the next element type is elem.
  pub fn advance_if(&self, elem: &Elem<E>) -> Option<ProdState<'a, E>> {
    self
      .next_elem_state()
      .filter(|(e, _)| e.elem() == elem)
      .map(|(_, next)| next)
  }

  pub fn is_complete(&self) -> bool {
    self.prod.num_elements() == self.index
  }

  pub fn action_key(&self) -> &'a E::ActionKey {
    self.prod.action_key()
  }
}

/// A set of production states.
#[derive(Derivative)]
#[derivative(
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = ""),
  Debug(bound = "")
)]
pub struct ProdStateSet<'a, E: ElemTypes> {
  states: BTreeSet<ProdState<'a, E>>,
}

impl<'a, E: ElemTypes> FromIterator<ProdState<'a, E>> for ProdStateSet<'a, E> {
  fn from_iter<T: IntoIterator<Item = ProdState<'a, E>>>(iter: T) -> Self {
    ProdStateSet {
      states: iter.into_iter().collect(),
    }
  }
}

impl<'a, E: ElemTypes> ProdStateSet<'a, E> {
  pub fn new_empty() -> Self {
    ProdStateSet {
      states: BTreeSet::new(),
    }
  }

  pub fn add(&mut self, state: ProdState<'a, E>) {
    self.states.insert(state);
  }

  /// Returns an iterator of pairs of production elements, and the states that are entered after
  /// that.
  pub fn nexts(
    &self,
  ) -> impl Iterator<Item = (&'a ProdElement<E>, ProdStateSet<'a, E>)> {
    let mut result = BTreeMap::new();

    for (k, v) in self.states.iter().filter_map(ProdState::next_elem_state) {
      result
        .entry(k)
        .or_insert_with(|| ProdStateSet::new_empty())
        .add(v);
    }

    result.into_iter()
  }

  /// Given a function that takes the closure of a given prod state, take the closure of all states in this set.
  pub fn take_closure<F, I>(&mut self, mut close_func: F)
  where
    F: FnMut(&ProdState<'a, E>) -> I,
    I: Iterator<Item = ProdState<'a, E>>,
  {
    let mut seen_states = self.states.clone();
    let mut curr_states = self.states.clone();
    let mut next_states = BTreeSet::new();
    loop {
      for state in curr_states.iter() {
        for new_state in close_func(state) {
          if seen_states.insert(new_state.clone()) {
            next_states.insert(new_state);
          }
        }
      }

      if next_states.is_empty() {
        self.states = seen_states;
        return;
      }

      curr_states = next_states;
      next_states = BTreeSet::new();
    }
  }

  /// Returns an iterator over those states that are complete.
  pub fn complete(&self) -> impl Iterator<Item = ProdState<'a, E>> {
    self
      .states
      .iter()
      .filter(|st| st.is_complete())
      .cloned()
      .collect::<Vec<_>>()
      .into_iter()
  }
}
