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
mod state;

use crate::grammar::{Element, Grammar, NonTerminal, Production, Rule};
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
  rule: &Rule,
) -> bool {
  for prod in rule.prods() {
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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::grammar::{ProductionElement, Rule, Terminal};
  use crate::pdisplay::LayoutDisplay;

  #[test]
  fn test_grammar_print() {
    let t_a = Terminal::new("A");
    let nt_x = NonTerminal::new("x");

    let v1: Vec<ProductionElement> =
      vec![t_a.clone().into(), nt_x.clone().into(), t_a.into()];
    let v2: Vec<ProductionElement> = vec![];

    let x_rule = Rule::new(
      nt_x.clone(),
      vec![
        Production::new("Recursive", v1),
        Production::new("Empty", v2),
      ],
    );

    let g = Grammar::new(nt_x.clone(), vec![x_rule]);

    println!("{}", g.disp().layout(80));

    let ng = NullableGrammar::new(g);

    assert!(ng.is_nullable(&nt_x));
  }
}
