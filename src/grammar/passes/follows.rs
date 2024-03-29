use std::collections::{BTreeMap, BTreeSet};

use crate::utils::{change_iter, change_loop, WasChanged};
use crate::{grammar::Elem, utils::CollectMap};

use super::firsts::{Firsts, FirstsError};
use super::Pass;

#[derive(thiserror::Error, Debug)]
pub enum FollowsError {
  #[error(transparent)]
  First(#[from] FirstsError),
}

pub struct Follows<T, NT>(BTreeMap<NT, BTreeSet<T>>);

impl<T, NT, AK, AV> Pass<T, NT, AK, AV> for Follows<T, NT>
where
  T: Ord + Clone + 'static,
  NT: Ord + Clone + 'static,
  AK: Ord + Clone + 'static,
{
  type Error = FollowsError;

  fn run_pass(
    pass_map: &super::PassContext<T, NT, AK, AV>,
  ) -> Result<Self, FollowsError> {
    let gram = pass_map.grammar();

    let firsts = pass_map.get_pass::<Firsts<T, NT>>()?;

    let mut follows = CollectMap::new();

    change_loop(|| {
      change_iter(gram.prods(), |prod| {
        let mut changed = WasChanged::Unchanged;
        let elems = prod.elements().collect::<Vec<_>>();
        for elem_pair in elems.windows(2) {
          if let Elem::NonTerm(first_nt) = elem_pair[0] {
            match elem_pair[1] {
              Elem::Term(t) => changed.merge(follows.insert(first_nt, t)),
              Elem::NonTerm(second_nt) => changed.merge(
                follows
                  .insert_iter(first_nt, firsts.get(second_nt).unwrap().iter()),
              ),
            }
          }
        }

        if let Some(Elem::NonTerm(last_nt)) = elems.last() {
          changed.merge(follows.insert_from_key_set(last_nt, prod.head()));
        }

        changed
      })
    });

    Ok(Follows(
      follows
        .into_inner()
        .into_iter()
        .map(|(k, v)| (k.clone(), v.into_iter().cloned().collect()))
        .collect(),
    ))
  }
}
