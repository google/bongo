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
  crate::{parsers::ParseTree, state::ProductionState, ElementTypes},
  std::collections::BTreeMap,
};

struct ParserImpl<'a, E: ElementTypes, Leaf> {
  states: Vec<EarleyState<'a, E, Leaf>>,
}

struct EarleyState<'a, E: ElementTypes, Leaf> {
  state: BTreeMap<ProductionState<'a, E>, Vec<ParseTree<E, Leaf>>>,
}

impl<E: ElementTypes, Leaf> EarleyState<'_, E, Leaf> {
  fn new() -> Self {
    EarleyState {
      state: BTreeMap::new(),
    }
  }
}
