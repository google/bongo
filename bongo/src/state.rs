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

use codefmt::Layout;
use crate::grammar::{Element, NonTerminal, Production};
use crate::pdisplay::{self, LayoutDisplay};

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
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct ProductionState {
  /// The head nonterminal this production belongs to.
  head: NonTerminal,

  /// The production this state is part of.
  prod: Production,

  /// The index of this production state. Must be in the range [0,
  /// self.prod.prod_elements().len()].
  index: usize,
}

impl ProductionState {
  /// Create a ProductionState from a given NonTerminal and Production.
  ///
  /// This state's index will be at the start of the production.
  pub fn from_start(head: NonTerminal, prod: Production) -> Self {
    ProductionState {
      head,
      prod,
      index: 0,
    }
  }

  /// Returns the next element after the current index. If it is at the
  /// end, then it reuturns `None`.
  pub fn next(&self) -> Option<&Element> {
    self.prod.element_at(self.index)
  }

  // Unconditionally creates a new production state that is the
  // increment of this production state. If `index` >= length, then this will
  // create a production state.
  fn advance_forced(&self) -> ProductionState {
    let mut result = self.clone();
    result.index += 1;
    result
  }

  /// Return another `ProductionState` with the same head and production as
  /// `self`, but with the index advanced. If the index is already at the
  /// end of the production, returns `None`.
  pub fn advance(&self) -> Option<ProductionState> {
    self.next().map(|_| self.advance_forced())
  }

  /// Return Some(state) which is this state advanced if
  /// the next element type is elem.
  pub fn advance_if(&self, elem: &Element) -> Option<ProductionState> {
    self
      .next()
      .filter(|e| e == &elem)
      .map(|_| self.advance_forced())
  }
}

impl LayoutDisplay for ProductionState {
  fn disp(&self) -> Layout {
    let mut layouts = Vec::new();
    let (first_slice, second_slice) =
      self.prod.prod_elements().split_at(self.index);
    for elem in first_slice {
      layouts.push(elem.disp());
    }
    layouts.push(codefmt::Layout::text("."));
    for elem in second_slice {
      layouts.push(elem.disp())
    }
    let body = pdisplay::join_layout(layouts, codefmt::Layout::text(" "));
    codefmt::Layout::juxtapose(vec![
      self.head.disp(),
      codefmt::Layout::text(" => "),
      body,
    ])
  }
}
