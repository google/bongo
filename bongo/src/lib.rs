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

//! Bongo is a library for generating Marpa parsers, and executing them on inputs.

#![allow(dead_code)]

#[macro_use]
extern crate derivative;

pub mod grammar;
pub mod parsers;
mod pdisplay;
mod state;
pub mod utils;

use crate::grammar::{nullables::GrammarNullableInfo, ElementTypes, Grammar};

#[derive(Clone)]
pub struct NullableGrammar<E: ElementTypes> {
  grammar: Grammar<E>,
  nullables: GrammarNullableInfo<E>,
}

impl<E: ElementTypes> NullableGrammar<E> {
  pub fn new(grammar: Grammar<E>) -> Self {
    let nullables =
      crate::grammar::nullables::calculate_nullables(&grammar).unwrap();
    NullableGrammar { grammar, nullables }
  }

  pub fn is_nullable(&self, nt: &E::NonTerm) -> bool {
    self.nullables.nonterm_info().contains_key(nt)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::grammar::build;
  use crate::grammar::{BaseElementTypes, NonTerminal, Terminal};
  use crate::pdisplay::LayoutDisplay;
  use crate::utils::Name;

  #[test]
  fn test_grammar_print() {
    let t_a = Terminal::new("A");
    let nt_x = NonTerminal::new("x");

    let g: Grammar<BaseElementTypes> = build(&nt_x, |gb| {
      gb.add_rule(&nt_x, |rb| {
        rb.add_prod(Name::new("Recursive"), (), |pb| {
          pb.add_term(&t_a).add_nonterm(&nt_x).add_term(&t_a);
        })
        .add_prod(Name::new("Empty"), (), |_pb| {});
      });
    })
    .unwrap();

    println!("{}", g.disp().layout(80));

    let ng = NullableGrammar::new(g);

    assert!(ng.is_nullable(&nt_x));
  }
}
