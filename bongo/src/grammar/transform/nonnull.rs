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

//! A Grammar transform that removes all nullable non-terminals.
//!
//! This transformation is documented in [Practical Earley Parsing][pep-aycock].
//! In short, each production is replaced with a set of productions where, for
//! each possibile assignment from {Null, NonNull} to each nullable non-terminal
//! in that production, there exists a production with all Null-assigned
//! non-terminals are removed. The exception is that if a particular production
//! is empty after this transformation, it is excluded. This guarantees that
//! each non-terminal will consume at least one terminal.
//!
//! Note that this can introduce local ambiguities. These are not directly
//! detected by this transformation.
//!
//! Example: For the grammar:
//!
//! ```text
//! q := p p <seq> ;
//! p := A <has-a>
//!    | <not-has-a>
//!    ;
//! ```
//!
//! This is transformed into:
//!
//! ```text
//! q := p p <seq(*, *)>
//!    | p <seq(*, not-has-a)>
//!    | p <seq(not-has-a, *)>
//!    /* The nonnull-case is not included. */
//!    ;
//! p := a <has-a>
//! ```

use std::collections::BTreeMap;
use std::marker::PhantomData;

use failure::Error;

use crate::grammar::{
  nullables::{calculate_nullables, GrammarNullableInfo},
  Element, ElementTypes, Grammar, Production, ProductionElement, Rule,
};

use crate::utils::{Name, TreeNode, Void};

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct ElemTypes<E: ElementTypes>(PhantomData<E>);

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct Action<E: ElementTypes> {
  action: E::Action,
  nt_nullable_states: Vec<bool>,
  nullables: BTreeMap<Name, TreeNode<E::Action, Void>>,
}

impl<E: ElementTypes> ElementTypes for ElemTypes<E> {
  type Term = E::Term;
  type NonTerm = E::NonTerm;
  type Action = Action<E>;
}

pub fn transform_to_nonnull<E: ElementTypes>(
  g: &Grammar<E>,
) -> Result<Grammar<ElemTypes<E>>, Error> {
  let nullables = calculate_nullables(g)?;

  let mut new_rules = Vec::new();

  for (nt, rule) in g.rule_set() {
    let nonnullable_prods = rule
      .prods()
      .iter()
      .filter(|prod| !nullables.is_prod_nullable(&prod))
      .flat_map(|prod| to_nonnull_prods(&nullables, prod))
      .collect();

    new_rules.push(Rule::new(nt.clone(), nonnullable_prods));
  }

  Ok(Grammar::new(g.start_nt().clone(), new_rules))
}

#[derive(Clone, Debug)]
struct ProdBuildState<E: ElementTypes> {
  elems: Vec<ProductionElement<ElemTypes<E>>>,
  nt_nullable_states: Vec<bool>,
  action_args: BTreeMap<Name, TreeNode<E::Action, Void>>,
}

fn to_nonnull_prods<E: ElementTypes>(
  nullable_info: &GrammarNullableInfo<E>,
  prod: &Production<E>,
) -> Vec<Production<ElemTypes<E>>> {
  let mut curr_build_states = vec![ProdBuildState {
    elems: Vec::new(),
    nt_nullable_states: Vec::new(),
    action_args: BTreeMap::new(),
  }];

  for prod_elem in prod.prod_elements() {
    match &prod_elem.elem() {
      Element::NonTerm(nt) => {
        match nullable_info.get_nullable_info(nt) {
          Some(info) => {
            // We have to clone all of the current build states.
            let mut new_build_states = Vec::new();
            for curr_build_state in &mut curr_build_states {
              let mut new_build_state = curr_build_state.clone();
              // Write non-null version into existing state
              curr_build_state.elems.push(prod_elem.clone_as_other());
              curr_build_state.nt_nullable_states.push(false);

              // Write null version into cloned state
              if let Some(id) = &prod_elem.identifier {
                new_build_state
                  .action_args
                  .insert(id.clone(), info.nullable_action().clone());
              }

              new_build_state.nt_nullable_states.push(true);
              new_build_states.push(new_build_state);
            }

            curr_build_states.append(&mut new_build_states);
          }
          None => {
            for curr_build_state in &mut curr_build_states {
              // Write non-null version into existing state
              curr_build_state.elems.push(prod_elem.clone_as_other());
              curr_build_state.nt_nullable_states.push(false);
            }
          }
        }
      }
      Element::Term(_) => {
        for prod_build_state in &mut curr_build_states {
          prod_build_state.elems.push(prod_elem.clone_as_other())
        }
      }
    }
  }

  let mut prods = Vec::new();

  for curr_build_state in curr_build_states {
    let new_action = Action {
      action: prod.action().clone(),
      nt_nullable_states: curr_build_state.nt_nullable_states,
      nullables: curr_build_state.action_args,
    };

    prods.push(Production::new(new_action, curr_build_state.elems));
  }

  prods
}
