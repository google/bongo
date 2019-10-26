//! A dumb earley parser for any implementation of Grammar.
//!
//! This is not intended to be a high-performance implementation of a bongo
//! grammar, but a baseline that can be validated against.

use {
  crate::{
    parsers::ParseTree,
    state::ProductionState,
    ElementTypes,
  },
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
