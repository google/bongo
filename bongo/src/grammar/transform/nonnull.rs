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

/*
use std::collections::BTreeMap;
use std::marker::PhantomData;

use crate::grammar::{
  nullables::{calculate_nullables, is_prod_nullable},
  ElementTypes, Grammar,
};

use crate::utils::{Name, TreeNode, Void};

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct ElemTypes<E: ElementTypes>(PhantomData<E>);

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct Action<A> {
  action: A,
  nullables: BTreeMap<Name, Box<TreeNode<A, Void>>>,
}

impl<E: ElementTypes> ElementTypes for ElemTypes<E> {
  type Term = E::Term;
  type NonTerm = E::NonTerm;
  type Action = Action<E::Action>;
}

pub fn transform_to_nonnull<E: ElementTypes>(
  g: &Grammar<E>,
) -> Grammar<ElemTypes<E>> {
  let nullables = calculate_nullables(g);

  for nullable_nt in &nullables {
    for rule in g.get_rule(nullable_nt) {
      let nullable_prods = rule
        .prods()
        .iter()
        .filter(|prod| is_prod_nullable(&nullables, &prod))
        .map(|prod| prod.action())
        .collect::<Vec<_>>();

      for prod in &rule.prods {}
    }
  }

  unimplemented!()
}
*/
