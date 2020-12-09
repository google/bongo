use std::collections::{BTreeMap, BTreeSet};

use crate::{grammar::{Elem, ElemTypes, Grammar}, state::ProdState, utils::{WasChanged, change_iter, change_loop, CollectMap}};

fn first_set<'a, E: ElemTypes>(gram: &'a Grammar<E>) -> BTreeMap<&'a E::NonTerm, BTreeSet<&'a E::Term>> {
  let mut first_map = CollectMap::new();
  change_loop(|| {
    change_iter(gram.prods(), |prod| {
      if let Some(first) = prod.first_elem() {
        match first {
          Elem::NonTerm(nt) => {
            first_map.insert_iter(prod.head(), first_map.get(&nt).unwrap().clone())
          }
          Elem::Term(t) => {
            first_map.insert(prod.head(), t)
          }
        }
      } else {
        WasChanged::Unchanged
      }
    })
  });
  
  first_map.into_inner()
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
struct ParseState<'a, E>
where
  E: ElemTypes,
{
  prods: BTreeMap<ProdState<'a, E>, BTreeSet<E::Term>>,
}

impl<'a, E> ParseState<'a, E>
where
  E: ElemTypes,
{
  pub fn reducable_actions<'b>(
    &'b self,
  ) -> impl Iterator<Item = &'b E::ActionKey> {
    self.prods.keys().filter_map(|prod| {
      if prod.is_complete() {
        Some(prod.action_key())
      } else {
        None
      }
    })
  }
}
