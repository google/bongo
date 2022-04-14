use std::collections::{BTreeMap, BTreeSet};

use crate::utils::{change_iter, change_loop, WasChanged};
use crate::{grammar::Elem, utils::CollectMap};

use super::nullable::{self, Nullable};
use super::Pass;

#[derive(thiserror::Error, Debug)]
pub enum FirstsError {
  #[error(transparent)]
  NullableError(#[from] nullable::NullableError),
}

pub struct Firsts<T, NT>(BTreeMap<NT, BTreeSet<T>>);

impl<T, NT> Firsts<T, NT>
where
  NT: Ord,
{
  pub fn get(&self, nt: &NT) -> Option<&BTreeSet<T>> {
    self.0.get(nt)
  }
}

impl<T, NT, AK, AV> Pass<T, NT, AK, AV> for Firsts<T, NT>
where
  T: Ord + Clone + 'static,
  NT: Ord + Clone + 'static,
  AK: Ord + Clone + 'static,
{
  type Error = FirstsError;

  fn run_pass(
    pass_context: &super::PassContext<T, NT, AK, AV>,
  ) -> Result<Self, FirstsError> {
    let gram = pass_context.grammar();

    let nullables = pass_context.get_pass::<Nullable<NT, AK>>()?;

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
              if !nullables.is_nullable(nt) {
                break;
              }
            }
          }
        }

        changed
      })
    });

    Ok(Firsts(
      firsts
        .into_inner()
        .into_iter()
        .map(|(k, v)| (k.clone(), v.into_iter().cloned().collect()))
        .collect(),
    ))
  }
}
