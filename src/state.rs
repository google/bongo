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

use crate::grammar::{Elem, Prod, ProdElement};
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
  Debug(bound = "T: std::fmt::Debug, NT: std::fmt::Debug")
)]

pub struct ProdState<'a, T, NT, AK, AV> {
  /// The production this state is part of.
  prod: Prod<'a, T, NT, AK, AV>,

  /// The index of this production state. Must be in the range [0,
  /// self.prod.prod_elements().len()].
  index: usize,
}

impl<'a, T, NT, AK, AV> Ord for ProdState<'a, T, NT, AK, AV>
where
  NT: Ord,
{
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.prod.cmp(&other.prod)
  }
}

impl<'a, T, NT, AK, AV> PartialOrd for ProdState<'a, T, NT, AK, AV>
where
  NT: Ord,
{
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl<'a, T, NT, AK, AV> PartialEq for ProdState<'a, T, NT, AK, AV>
where
  NT: Ord,
{
  fn eq(&self, other: &Self) -> bool {
    self.prod == other.prod
  }
}

impl<'a, T, NT, AK, AV> Eq for ProdState<'a, T, NT, AK, AV> where NT: Ord {}

impl<'a, T, NT, AK, AV> ProdState<'a, T, NT, AK, AV> {
  /// Create a ProdState from a given NonTerminal and Prod.
  ///
  /// This state's index will be at the start of the production.
  pub fn from_start(prod: Prod<'a, T, NT, AK, AV>) -> Self {
    ProdState { prod, index: 0 }
  }

  pub fn prod(&self) -> Prod<'a, T, NT, AK, AV> {
    self.prod
  }

  pub fn offset_prod_elem(&self, i: usize) -> Option<&'a ProdElement<T, NT>> {
    self.prod.prod_elements().get(self.index + i)
  }

  pub fn offset_elem(&self, i: usize) -> Option<&'a Elem<T, NT>> {
    self.offset_prod_elem(i).map(ProdElement::elem)
  }

  pub fn next_prod_elem(&self) -> Option<&'a ProdElement<T, NT>> {
    self.offset_prod_elem(0)
  }

  pub fn next_elem(&self) -> Option<&'a Elem<T, NT>> {
    self.offset_elem(0)
  }

  /// Returns the next element after the current index. If it is at the
  /// end, then it reuturns `None`.
  pub fn next_elem_state(
    &self,
  ) -> Option<(&'a ProdElement<T, NT>, ProdState<'a, T, NT, AK, AV>)> {
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

  pub fn is_complete(&self) -> bool {
    self.prod.num_elements() == self.index
  }

  pub fn action_key(&self) -> &'a AK {
    self.prod.action_key()
  }
}

impl<'a, T, NT, AK, AV> ProdState<'a, T, NT, AK, AV>
where
  T: Eq,
  NT: Eq,
{
  /// Return Some(state) which is this state advanced if
  /// the next element type is elem.
  pub fn advance_if(
    &self,
    elem: &Elem<T, NT>,
  ) -> Option<ProdState<'a, T, NT, AK, AV>> {
    self
      .next_elem_state()
      .filter(|(e, _)| e.elem() == elem)
      .map(|(_, next)| next)
  }
}

/// A set of production states.
#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct ProdStateSet<'a, T, NT, AK, AV> {
  states: BTreeSet<ProdState<'a, T, NT, AK, AV>>,
}

impl<'a, T, NT, AK, AV> Ord for ProdStateSet<'a, T, NT, AK, AV>
where
  NT: Ord,
{
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.states.cmp(&other.states)
  }
}

impl<'a, T, NT, AK, AV> PartialOrd for ProdStateSet<'a, T, NT, AK, AV>
where
  NT: Ord,
{
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    self.states.partial_cmp(&other.states)
  }
}

impl<'a, T, NT, AK, AV> PartialEq for ProdStateSet<'a, T, NT, AK, AV>
where
  NT: Ord,
{
  fn eq(&self, other: &Self) -> bool {
    self.states.eq(&other.states)
  }
}

impl<'a, T, NT, AK, AV> Eq for ProdStateSet<'a, T, NT, AK, AV> where NT: Ord {}

impl<'a, T, NT, AK, AV> FromIterator<ProdState<'a, T, NT, AK, AV>>
  for ProdStateSet<'a, T, NT, AK, AV>
where
  NT: Ord,
{
  fn from_iter<I: IntoIterator<Item = ProdState<'a, T, NT, AK, AV>>>(
    iter: I,
  ) -> Self {
    ProdStateSet {
      states: iter.into_iter().collect(),
    }
  }
}

impl<'a, T, NT, AK, AV> ProdStateSet<'a, T, NT, AK, AV> {
  pub fn new_empty() -> Self {
    ProdStateSet {
      states: BTreeSet::new(),
    }
  }

  /// Returns an iterator over those states that are complete.
  pub fn complete(&self) -> impl Iterator<Item = ProdState<'a, T, NT, AK, AV>> {
    self
      .states
      .iter()
      .filter(|st| st.is_complete())
      .cloned()
      .collect::<Vec<_>>()
      .into_iter()
  }
}

impl<'a, T, NT, AK, AV> ProdStateSet<'a, T, NT, AK, AV>
where
  T: Ord,
  NT: Ord,
{
  pub fn add(&mut self, state: ProdState<'a, T, NT, AK, AV>) {
    self.states.insert(state);
  }

  /// Returns an iterator of pairs of production elements, and the states that are entered after
  /// that.
  pub fn nexts(
    &self,
  ) -> impl Iterator<Item = (&'a ProdElement<T, NT>, ProdStateSet<'a, T, NT, AK, AV>)>
  {
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
    F: FnMut(&ProdState<'a, T, NT, AK, AV>) -> I,
    I: Iterator<Item = ProdState<'a, T, NT, AK, AV>>,
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
}
