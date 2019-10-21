//! A dumb earley parser for any implementation of Grammar.
//!
//! This is not intended to be a high-performance implementation of a bongo
//! grammar, but a baseline that can be validated against.

use {
  crate::{
    ElementTypes,
    parsers::{self, TokenStream, ParseTree},
    state::ProductionState,
  },
  std::collections::BTreeMap,
};

struct ParserImpl<E: ElementTypes, Leaf> {
  states: Vec<EarleyState<E, Leaf>>,
}

struct EarleyState<E: ElementTypes, Leaf> {
  state: BTreeMap<ProductionState<E>, Vec<ParseTree<E, Leaf>>>,
}

impl<E: ElementTypes, Leaf> EarleyState<E, Leaf> {
  fn new() -> Self {
    EarleyState { state : BTreeMap::new() }
  }
}