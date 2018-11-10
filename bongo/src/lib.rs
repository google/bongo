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

/// For a given type and type parameters, generates PartialEq, Eq, and
/// PartialOrd such that all three depend on the implementation of Ord.
///
/// This is necessary because the typical approach to implementing any of these
/// for a type `Foo<T>` is to require `T` to implement that trait as well. Our
/// grammars are using a type which does not need to implement all of those.
macro_rules! impl_from_ord {
  ($t:ident[$(($targ:ident : $($bound:tt)*))*]) => {
    impl<$($targ : $($bound)*),*> std::cmp::Eq for $t<$($targ),*> {
    }

    impl<$($targ : $($bound)*),*> std::cmp::PartialEq for $t<$($targ),*> {
      fn eq(&self, other: &Self) -> bool {
        std::cmp::Ord::cmp(self, other) == std::cmp::Ordering::Equal
      }
    }

    impl<$($targ : $($bound)*),*> std::cmp::PartialOrd for $t<$($targ),*> {
      fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
      }
    }
  };
}

mod grammar;
mod pdisplay;
mod state;

use crate::grammar::{Element, ElementTypes, Grammar, Production, Rule};
use std::collections::BTreeSet;

#[derive(Clone)]
pub struct NullableGrammar<E: ElementTypes> {
  grammar: Grammar<E>,
  nullables: BTreeSet<E::NonTerm>,
}

impl<E: ElementTypes> NullableGrammar<E> {
  pub fn new(grammar: Grammar<E>) -> Self {
    let nullables = fixed_point(BTreeSet::new(), |nullables| {
      is_nullable_fp(&grammar, nullables)
    });
    NullableGrammar { grammar, nullables }
  }

  pub fn is_nullable(&self, nt: &E::NonTerm) -> bool {
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

fn is_prod_nullable<E: ElementTypes>(
  nullables: &BTreeSet<E::NonTerm>,
  prod: &Production<E>,
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

fn are_any_prods_nullable<E: ElementTypes>(
  nullables: &BTreeSet<E::NonTerm>,
  rule: &Rule<E>,
) -> bool {
  for prod in rule.prods() {
    if is_prod_nullable(nullables, prod) {
      return true;
    }
  }
  false
}

fn is_nullable_fp<E: ElementTypes>(
  grammar: &Grammar<E>,
  prev_nullables: &BTreeSet<E::NonTerm>,
) -> BTreeSet<E::NonTerm> {
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
  use crate::grammar::{
    BaseElementTypes, Name, NonTerminal, ProductionElement, Rule, Terminal,
  };
  use crate::pdisplay::LayoutDisplay;

  #[test]
  fn test_grammar_print() {
    let t_a = Terminal::new("A");
    let nt_x = NonTerminal::new("x");

    let v1: Vec<ProductionElement<BaseElementTypes>> = vec![
      ProductionElement::new_empty(Element::Term(t_a.clone())),
      ProductionElement::new_empty(Element::NonTerm(nt_x.clone())),
      ProductionElement::new_empty(Element::Term(t_a)),
    ];
    let v2: Vec<ProductionElement<BaseElementTypes>> = vec![];

    let x_rule = Rule::new(
      nt_x.clone(),
      vec![
        Production::new(Name::new("Recursive"), v1),
        Production::new(Name::new("Empty"), v2),
      ],
    );

    let g = Grammar::new(nt_x.clone(), vec![x_rule]);

    println!("{}", g.disp().layout(80));

    let ng = NullableGrammar::new(g);

    assert!(ng.is_nullable(&nt_x));
  }
}
