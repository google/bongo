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

use {
  crate::{
    grammar::{
      build,
      passes::{
        nullable::{GrammarNullableInfo, Nullable},
        PassContext,
      },
      Elem, ElemTypes, Grammar, Prod, ProdElement, ProdKey, RuleBuilder,
    },
    utils::{Name, ToDoc, TreeNode, Void},
  },
  std::{collections::BTreeMap, marker::PhantomData},
};

pub struct NonNullElemTypes<E: ElemTypes>(PhantomData<E>);

#[derive(Derivative)]
#[derivative(
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = ""),
  Debug(bound = "")
)]
pub struct ActionKey<E: ElemTypes> {
  action: E::ActionKey,
  nt_nullable_states: Vec<bool>,
}

impl<E: ElemTypes> ToDoc for ActionKey<E> {
  fn to_doc<'a, DA: pretty::DocAllocator<'a>>(
    &self,
    da: &'a DA,
  ) -> pretty::DocBuilder<'a, DA>
  where
    DA::Doc: Clone,
  {
    da.nil().append(self.action.to_doc(da))
  }
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub struct ActionValue<E: ElemTypes> {
  parent_value: E::ActionValue,
  nullable_arguments: BTreeMap<Name, TreeNode<ProdKey<E>, Void>>,
}

impl<E: ElemTypes> ToDoc for ActionValue<E> {
  fn to_doc<'a, DA: pretty::DocAllocator<'a>>(
    &self,
    da: &'a DA,
  ) -> pretty::DocBuilder<'a, DA, ()>
  where
    DA::Doc: Clone,
  {
    da.nil().append(self.parent_value.to_doc(da))
  }
}

impl<E: ElemTypes> ElemTypes for NonNullElemTypes<E> {
  type Term = E::Term;
  type NonTerm = E::NonTerm;
  type ActionKey = ActionKey<E>;
  type ActionValue = ActionValue<E>;
}

pub fn transform_to_nonnull<E: ElemTypes>(
  g: &Grammar<E>,
) -> anyhow::Result<Grammar<NonNullElemTypes<E>>> {
  let pass_map = PassContext::new(g);
  let nullable = pass_map.get_pass::<Nullable<E>>()?;

  build(g.start_nt().clone(), |g_builder| {
    for (nt, rule) in g.rule_set() {
      g_builder.add_rule(nt.clone(), |r_builder| {
        for prod in rule.prods() {
          if nullable.is_prod_nullable(&prod) {
            continue;
          }

          build_nonnull_prods(nullable.get_nullable_info(), &prod, r_builder);
        }
      });
    }
  })
  .map_err(|_| anyhow::anyhow!("Grammar failed to build"))
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
struct ProdBuildState<E: ElemTypes> {
  elems: Vec<ProdElement<NonNullElemTypes<E>>>,
  nt_nullable_states: Vec<bool>,
  action_args: BTreeMap<Name, TreeNode<ProdKey<E>, Void>>,
}

fn build_nonnull_prods<E: ElemTypes>(
  nullable_info: &GrammarNullableInfo<E>,
  prod: &Prod<E>,
  r_builder: &mut RuleBuilder<NonNullElemTypes<E>>,
) {
  let mut curr_build_states = vec![ProdBuildState {
    elems: Vec::new(),
    nt_nullable_states: Vec::new(),
    action_args: BTreeMap::new(),
  }];

  for prod_elem in prod.prod_elements() {
    match &prod_elem.elem() {
      Elem::NonTerm(nt) => {
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
              if let Some(id) = prod_elem.id() {
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
      Elem::Term(_) => {
        for prod_build_state in &mut curr_build_states {
          prod_build_state.elems.push(prod_elem.clone_as_other())
        }
      }
    }
  }

  for curr_build_state in curr_build_states {
    let prev_action_value = prod.action_value();

    let new_action_key = ActionKey {
      action: prod.action_key().clone(),
      nt_nullable_states: curr_build_state.nt_nullable_states,
    };

    let new_action_value = ActionValue {
      parent_value: prev_action_value.clone(),
      nullable_arguments: curr_build_state.action_args,
    };

    r_builder.add_prod_with_elems(
      new_action_key,
      new_action_value,
      curr_build_state.elems,
    );
  }
}
