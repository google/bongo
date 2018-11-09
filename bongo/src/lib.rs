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

mod grammar;
mod pdisplay;

use codefmt::Layout;
use crate::grammar::{
  Element, Grammar, NonTerminal, Production, ProductionSet,
};
use crate::pdisplay::LayoutDisplay;
use std::collections::BTreeSet;

#[derive(Clone)]
pub struct NullableGrammar {
  grammar: Grammar,
  nullables: BTreeSet<NonTerminal>,
}

impl NullableGrammar {
  pub fn new(grammar: Grammar) -> Self {
    let nullables = fixed_point(BTreeSet::new(), |nullables| {
      is_nullable_fp(&grammar, nullables)
    });
    NullableGrammar { grammar, nullables }
  }

  pub fn is_nullable(&self, nt: &NonTerminal) -> bool {
    self.nullables.contains(nt)
  }
}

fn fixed_point<T: Eq>(start: T, mut apply: impl FnMut(&T) -> T) -> T {
  let mut curr = start;
  loop {
    let next = apply(&curr);
    if next == curr {
      break curr;
    }
    curr = next;
  }
}

fn is_prod_nullable(
  nullables: &BTreeSet<NonTerminal>,
  prod: &Production,
) -> bool {
  for elem in prod.elements_iter() {
    match elem {
      Element::Term(_) => return false,
      Element::NonTerm(nt) => {
        if !nullables.contains(nt) {
          return false;
        }
      }
    }
  }
  true
}

fn are_any_prods_nullable(
  nullables: &BTreeSet<NonTerminal>,
  prod_set: &ProductionSet,
) -> bool {
  for prod in prod_set.prods() {
    if is_prod_nullable(nullables, prod) {
      return true;
    }
  }
  false
}

fn is_nullable_fp(
  grammar: &Grammar,
  prev_nullables: &BTreeSet<NonTerminal>,
) -> BTreeSet<NonTerminal> {
  let mut curr_nullables = prev_nullables.clone();
  for (nt, prod_set) in grammar.rule_set() {
    if are_any_prods_nullable(prev_nullables, prod_set) {
      curr_nullables.insert(nt.clone());
    }
  }
  curr_nullables
}

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
  head: NonTerminal,
  prod: Production,
  index: usize,
}

impl ProductionState {
  pub fn from_start(head: NonTerminal, prod: Production) -> Self {
    ProductionState {
      head,
      prod,
      index: 0,
    }
  }

  pub fn next(&self) -> Option<&Element> {
    self.prod.element_at(self.index)
  }

  fn advance_forced(&self) -> ProductionState {
    let mut result = self.clone();
    result.index += 1;
    result
  }

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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::grammar::{ProductionElement, Terminal};

  #[test]
  fn test_grammar_print() {
    let t_a = Terminal::new("A");
    let nt_x = NonTerminal::new("x");

    let v1: Vec<ProductionElement> =
      vec![t_a.clone().into(), nt_x.clone().into(), t_a.into()];
    let v2: Vec<ProductionElement> = vec![];

    let prod_set = ProductionSet::new(vec![
      Production::new("Recursive", v1),
      Production::new("Empty", v2),
    ]);

    let mut rules = std::collections::BTreeMap::new();
    rules.insert(nt_x.clone(), prod_set);

    let g = Grammar::new(nt_x.clone(), rules);

    println!("{}", g.disp().layout(80));

    let ng = NullableGrammar::new(g);

    assert!(ng.is_nullable(&nt_x));
  }
}
