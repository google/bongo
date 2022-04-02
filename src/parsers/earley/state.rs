//! Provides base data structures to represent and work with Earley states.
//! 
//! Earley states consist of a set of production states.

use {
  crate::{
    grammar::{ElemTypes, Prod},
    parsers::{
      tree::{Node, TreeHandle},
      Token,
    },
    start_grammar::{StartElementTypes, StartGrammar, StreamTerminal},
    state::ProdState,
    utils::{change_iter, WasChanged},
  },
  im::Vector,
  std::collections::{btree_map, BTreeMap},
};

#[derive(Derivative)]
#[derivative(
  Clone(bound = ""),
  Eq(bound = ""),
  PartialEq(bound = ""),
  Ord(bound = ""),
  PartialOrd(bound = ""),
  Debug(bound = "")
)]
struct EarleyStateKey<'a, E: ElemTypes> {
  prod_state: ProdState<'a, StartElementTypes<E>>,
  origin_index: usize,
}

impl<'a, E: ElemTypes> EarleyStateKey<'a, E> {
  pub fn is_final_key(&self) -> bool {
    self
      .prod_state
      .next_elem()
      .and_then(|elem| elem.as_term())
      .map_or(false, StreamTerminal::is_eos)
  }
}

#[derive(Derivative)]
#[derivative(
  Clone(bound = ""),
  Eq(bound = ""),
  PartialEq(bound = ""),
  Ord(bound = ""),
  PartialOrd(bound = "")
)]
pub struct EarleyState<'a, E, T>
where
  E: ElemTypes,
{
  key: EarleyStateKey<'a, E>,
  #[derivative(Debug = "ignore")]
  value: Vector<Node<'a, E, T>>,
}

impl<'a, E, T> EarleyState<'a, E, T>
where
  E: ElemTypes,
  T: Ord + Clone,
{
  pub fn from_prod_start(
    prod: Prod<'a, StartElementTypes<E>>,
    index: usize,
  ) -> Self {
    EarleyState {
      key: EarleyStateKey {
        prod_state: ProdState::from_start(prod),
        origin_index: index,
      },
      value: Vector::new(),
    }
  }

  pub fn prod_state(&self) -> &ProdState<'a, StartElementTypes<E>> {
    &self.key.prod_state
  }

  pub fn create_next_value(
    &self,
    node: Node<'a, E, T>,
  ) -> Vector<Node<'a, E, T>> {
    let mut value = self.value.clone();
    value.push_back(node);
    value
  }

  pub fn predict(
    &self,
    grammar: &'a StartGrammar<E>,
    prev_states: &[EarleyStateSet<'a, E, T>],
  ) -> Vec<EarleyState<'a, E, T>> {
    self
      .prod_state()
      .next_elem()
      .iter()
      .filter_map(|nt| nt.as_nonterm())
      .filter_map(|nt| grammar.try_get_rule(nt))
      .flat_map(|r| r.prods())
      .map(|p| EarleyState {
        key: EarleyStateKey {
          prod_state: ProdState::from_start(p),
          origin_index: prev_states.len(),
        },
        value: Vector::new(),
      })
      .collect()
  }

  pub fn scan(
    &self,
    tree_handle: &TreeHandle<'a, E, T>,
    next_token: &Token<E::Term, T>,
  ) -> Option<EarleyState<'a, E, T>> {
    self
      .prod_state()
      .next_elem_state()
      .and_then(|(e, ps)| e.elem().as_term().map(move |t| (t, ps)))
      .filter(|(t, _)| t.has_kind(&next_token.kind))
      .map(|(_, ps)| EarleyState {
        key: EarleyStateKey {
          prod_state: ps,
          origin_index: self.key.origin_index,
        },
        value: self.create_next_value(
          tree_handle
            .make_leaf_node(next_token.kind.clone(), next_token.value.clone()),
        ),
      })
  }

  pub fn complete(
    &self,
    tree_handle: &TreeHandle<'a, E, T>,
    prev_states: &[EarleyStateSet<'a, E, T>],
    curr_state: &EarleyStateSet<'a, E, T>,
  ) -> Vec<EarleyState<'a, E, T>> {
    if self.prod_state().is_complete() {
      log::trace!("Completing prod state {:?}.", self.prod_state());
      let origin_state = if self.key.origin_index == prev_states.len() {
        curr_state
      } else {
        &prev_states[self.key.origin_index]
      };

      origin_state
        .states()
        .into_iter()
        .filter_map(|state| {
          state
            .prod_state()
            .next_elem_state()
            .map(|(e, ps)| (state, e, ps))
        })
        .filter_map(|(st, e, ps)| e.elem().as_nonterm().map(|nt| (st, nt, ps)))
        .filter(|(_, nt, _)| nt == &self.prod_state().prod().head())
        .map(|(st, _, ps)| EarleyState {
          key: EarleyStateKey {
            prod_state: ps,
            origin_index: st.key.origin_index,
          },
          value: st.create_next_value(tree_handle.make_branch_node(
            self.prod_state().action_key().as_base().unwrap().clone(),
            self.value.iter().cloned(),
          )),
        })
        .collect()
    } else {
      Vec::new()
    }
  }
}

impl<'a, E, T> std::fmt::Debug for EarleyState<'a, E, T>
where
  E: ElemTypes,
{
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    let mut st = f.debug_struct("EarleyState");
    st.field("prod_state", &self.key.prod_state);
    st.field("origin_index", &self.key.origin_index);
    st.field("value_len", &self.value.len());
    st.finish()
  }
}

pub struct EarleyStateSet<'a, E, T>
where
  E: ElemTypes,
{
  states: BTreeMap<EarleyStateKey<'a, E>, Vector<Node<'a, E, T>>>,
}

impl<'a, E, T> EarleyStateSet<'a, E, T>
where
  E: ElemTypes,
  T: Ord + Clone,
{
  pub fn new() -> Self {
    EarleyStateSet {
      states: BTreeMap::new(),
    }
  }

  pub fn new_start(grammar: &'a StartGrammar<E>) -> Self {
    let start_nt = grammar.start_nt();
    let start_rule = grammar.get_rule(start_nt);
    EarleyStateSet::from_states(
      start_rule
        .prods()
        .map(|prod| EarleyState::from_prod_start(prod, 0)),
    )
  }

  pub fn from_states(
    iter: impl Iterator<Item = EarleyState<'a, E, T>>,
  ) -> Self {
    let mut state_set = EarleyStateSet::new();
    for state in iter {
      state_set.insert(&state);
    }

    state_set
  }

  pub fn states<'b>(
    &'b self,
  ) -> impl Iterator<Item = EarleyState<'a, E, T>> + 'b {
    self.states.iter().map(|(key, value)| EarleyState {
      key: key.clone(),
      value: value.clone(),
    })
  }

  pub fn insert(&mut self, state: &EarleyState<'a, E, T>) -> WasChanged {
    match self.states.entry(state.key.clone()) {
      btree_map::Entry::Vacant(vac) => {
        vac.insert(state.value.clone());
        WasChanged::Changed
      }
      btree_map::Entry::Occupied(mut occ) => {
        let existing_nodes = occ.get_mut();
        let new_nodes = &state.value;
        assert_eq!(existing_nodes.len(), new_nodes.len());
        change_iter(
          existing_nodes.iter_mut().zip(new_nodes),
          |(existing_node, new_node)| existing_node.add_all(new_node),
        )
      }
    }
  }

  pub fn shift(
    &self,
    tree_handle: &TreeHandle<'a, E, T>,
    token: &Token<E::Term, T>,
  ) -> Self {
    EarleyStateSet::from_states(
      self
        .states()
        .into_iter()
        .filter_map(|state| state.scan(tree_handle, token)),
    )
  }

  pub fn is_empty(&self) -> bool {
    self.states.is_empty()
  }

  pub fn get_final(&self) -> Option<&Node<'a, E, T>> {
    for (k, v) in &self.states {
      if k.is_final_key() {
        assert_eq!(v.len(), 1);
        return Some(v.get(0).unwrap());
      }
    }

    None
  }
}

impl<'a, E, T> std::fmt::Debug for EarleyStateSet<'a, E, T>
where
  E: ElemTypes,
  T: Ord + Clone,
{
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    f.debug_set().entries(self.states()).finish()
  }
}
