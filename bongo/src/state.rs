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

use {
  crate::{
    grammar::{Element, ElementTypes, Prod, ProductionElement},
    pdisplay::{self, LayoutDisplay},
  },
  codefmt::Layout,
  derivative::Derivative,
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
  Debug(bound = ""),
)]
pub struct ProdState<'a, E: ElementTypes> {
  /// The production this state is part of.
  prod: Prod<'a, E>,

  /// The index of this production state. Must be in the range [0,
  /// self.prod.prod_elements().len()].
  index: usize,
}

impl<'a, E: ElementTypes> ProdState<'a, E> {
  /// Create a ProdState from a given NonTerminal and Prod.
  ///
  /// This state's index will be at the start of the production.
  pub fn from_start(prod: Prod<'a, E>) -> Self {
    ProdState { prod, index: 0 }
  }
  pub fn prod(&self) -> Prod<'a, E> {
    self.prod
  }

  /// Returns the next element after the current index. If it is at the
  /// end, then it reuturns `None`.
  pub fn next(&self) -> Option<(&'a ProductionElement<E>, ProdState<'a, E>)> {
    self.prod.prod_elements().get(self.index).map(|prod_elem| {
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
  pub fn advance_if(&self, elem: &Element<E>) -> Option<ProdState<'a, E>> {
    self
      .next()
      .filter(|(e, _)| e.elem() == elem)
      .map(|(_, next)| next)
  }
}

impl<'a, E: ElementTypes> LayoutDisplay for ProdState<'a, E> {
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
      self.prod.head().disp(),
      codefmt::Layout::text(" => "),
      body,
    ])
  }
}
