use std::collections::{btree_map, BTreeMap, BTreeSet};

use crate::{
  grammar::{
    passes::{firsts::Firsts, PassContext},
    Elem,
  },
  state::ProdState,
  utils::{change_iter, change_loop, CollectMap},
};

fn shuffle_iter<K, V>(
  iter: impl Iterator<Item = (K, V)>,
) -> impl Iterator<Item = (K, BTreeSet<V>)>
where
  K: Ord,
  V: Ord,
{
  let mut result = BTreeMap::new();
  for (k, v) in iter {
    match result.entry(k) {
      btree_map::Entry::Vacant(vac) => {
        vac.insert(std::iter::once(v).collect::<BTreeSet<_>>());
      }
      btree_map::Entry::Occupied(mut occ) => {
        occ.get_mut().insert(v);
      }
    }
  }

  result.into_iter()
}

#[derive(Clone)]
struct ParseState<'a, T, NT, AK, AV> {
  prods: BTreeMap<ProdState<'a, T, NT, AK, AV>, BTreeSet<T>>,
}

impl<'a, T, NT, AK, AV> Ord for ParseState<'a, T, NT, AK, AV>
where
  T: Ord,
  NT: Ord,
{
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.prods.cmp(&other.prods)
  }
}

impl<'a, T, NT, AK, AV> PartialEq for ParseState<'a, T, NT, AK, AV>
where
  T: Ord,
  NT: Ord,
{
  fn eq(&self, other: &Self) -> bool {
    self.prods == other.prods
  }
}

impl<'a, T, NT, AK, AV> Eq for ParseState<'a, T, NT, AK, AV>
where
  T: Ord,
  NT: Ord,
{
}

impl<'a, T, NT, AK, AV> PartialOrd for ParseState<'a, T, NT, AK, AV>
where
  T: Ord,
  NT: Ord,
{
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl<'a, T, NT, AK, AV> ParseState<'a, T, NT, AK, AV>
where
  T: Ord + Clone + 'static,
  NT: Ord + Clone + 'static,
  AK: Ord + Clone + 'static,
{
  pub fn from_prod_lookahead(
    passes: &PassContext<'a, T, NT, AK, AV>,
    items: impl IntoIterator<Item = (ProdState<'a, T, NT, AK, AV>, BTreeSet<T>)>,
  ) -> anyhow::Result<Self> {
    let firsts = passes.get_pass::<Firsts<T, NT>>()?;
    let mut prods = CollectMap::from_seed(items.into_iter().collect());

    change_loop(|| {
      let mut new_expansions = Vec::new();
      for (prod, la) in prods.iter() {
        if let Some(Elem::NonTerm(nt)) = prod.next_elem() {
          let new_la = match prod.offset_elem(1) {
            Some(Elem::NonTerm(nt)) => firsts.get(nt).unwrap().clone(),
            Some(Elem::Term(t)) => std::iter::once(t.clone()).collect(),
            None => la.clone(),
          };

          new_expansions.push((nt, new_la));
        }
      }

      change_iter(new_expansions, |(nt, new_la)| {
        change_iter(
          passes
            .grammar()
            .get_rule(nt)
            .prods()
            .map(|p| ProdState::from_start(p)),
          |ps| prods.insert_iter(ps, new_la.iter().cloned()),
        )
      })
    });

    Ok(ParseState {
      prods: prods.into_inner(),
    })
  }

  pub fn shift_actions(
    &self,
    passes: &PassContext<'a, T, NT, AK, AV>,
  ) -> anyhow::Result<BTreeMap<T, ParseState<'a, T, NT, AK, AV>>> {
    let iter = self
      .prods
      .iter()
      .filter_map(|(p, la)| p.next_elem_state().map(|(e, ps)| (e, ps, la)))
      .filter_map(|(e, ps, la)| {
        e.elem().as_term().map(|t| (t, (ps, la.clone())))
      });
    shuffle_iter(iter)
      .map(|(t, next_elems)| {
        ParseState::from_prod_lookahead(passes, next_elems)
          .map(|ps| (t.clone(), ps))
      })
      .collect::<Result<_, _>>()
  }

  pub fn reducable_actions(&self) -> impl Iterator<Item = &AK> {
    self.prods.keys().filter_map(|prod| {
      if prod.is_complete() {
        Some(prod.action_key())
      } else {
        None
      }
    })
  }
}
