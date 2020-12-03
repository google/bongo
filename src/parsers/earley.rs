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

mod state;

use {
  crate::{
    parsers::{
      tree::{Node, TreeHandle},
      Token,
    },
    start_grammar::StartGrammar,
    utils::{change_iter, change_loop},
    ElemTypes,
  },
  state::EarleyStateSet,
};

pub fn close_state<'a, E, T>(
  grammar: &'a StartGrammar<E>,
  tree_handle: &TreeHandle<'a, E, T>,
  prev_states: &[EarleyStateSet<'a, E, T>],
  new_state: &mut EarleyStateSet<'a, E, T>,
) where
  E: ElemTypes,
  T: Ord + Clone,
{
  assert!(!new_state.is_empty());
  change_loop(|| {
    let closure = new_state
      .states()
      .flat_map(|state| {
        std::iter::empty()
          .chain(state.predict(grammar, prev_states).into_iter())
          .chain(state.complete(tree_handle, prev_states, &new_state))
      })
      .collect::<Vec<_>>();

    change_iter(closure.into_iter(), |state| new_state.insert(&state))
  });
}

pub fn parse<'a, E, T>(
  grammar: &'a StartGrammar<E>,
  tree_handle: &TreeHandle<'a, E, T>,
  tokens: Vec<Token<E::Term, T>>,
) -> Option<Node<'a, E, T>>
where
  E: ElemTypes,
  T: Ord + Clone,
{
  let mut states = vec![];

  let mut init_state: EarleyStateSet<'a, E, T> =
    EarleyStateSet::new_start(grammar);
  close_state(grammar, tree_handle, &states, &mut init_state);
  states.push(init_state);

  for token in tokens {
    let mut new_state = states.last().unwrap().shift(tree_handle, &token);

    close_state(grammar, tree_handle, &states, &mut new_state);

    states.push(new_state);
  }

  let last_state = states.last().unwrap();
  last_state.get_final().cloned()
}
