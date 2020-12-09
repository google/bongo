use std::collections::{BTreeMap, BTreeSet};

use crate::utils::{change_iter, change_loop, WasChanged};
use crate::{
  grammar::{Elem, ElemTypes},
  utils::CollectMap,
};

use super::nullable::Nullable;
use super::Pass;

pub struct Firsts;

impl<E> Pass<E> for Firsts
where
  E: ElemTypes,
{
  type Value = BTreeMap<E::NonTerm, BTreeSet<E::Term>>;

  fn run_pass<'a>(pass_map: &super::PassMap<'a, E>) -> Self::Value {
    let gram = pass_map.grammar();

    let nullables = pass_map.get_pass::<Nullable>();

    let mut firsts = CollectMap::new();

    change_loop(|| {
      change_iter(gram.prods(), |prod| {
        let mut changed = WasChanged::Unchanged;
        for elem in prod.elements() {
          match elem {
            Elem::Term(t) => {
              changed.merge(firsts.insert(prod.head(), t));
              break;
            }
            Elem::NonTerm(nt) => {
              changed.merge(firsts.insert_from_key_set(prod.head(), nt));
              if !nullables.contains(nt) {
                break;
              }
            }
          }
        }

        changed
      })
    });

    firsts
      .into_inner()
      .into_iter()
      .map(|(k, v)| (k.clone(), v.into_iter().cloned().collect()))
      .collect()
  }
}
