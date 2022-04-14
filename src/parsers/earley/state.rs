//! Provides base data structures to represent and work with Earley states.
//!
//! Earley states consist of a set of production states.

use crate::start_grammar::{
  StartActionKey, StartActionValue, StartNonTerminal,
};

use {
  crate::{
    grammar::Prod,
    parsers::{
      tree::{Node, TreeHandle},
      Token,
    },
    start_grammar::{StartGrammar, StreamTerminal},
    state::ProdState,
    utils::{change_iter, WasChanged},
  },
  im::Vector,
  std::collections::{btree_map, BTreeMap},
};

#[derive(Derivative)]
#[derivative(
  Clone(bound = ""),
  Eq(bound = "NT: Ord"),
  PartialEq(bound = "NT: Ord"),
  Ord(bound = "NT: Ord"),
  PartialOrd(bound = "NT: Ord"),
  Debug(bound = "T: std::fmt::Debug, NT: std::fmt::Debug")
)]
struct EarleyStateKey<'a, T, NT, AK, AV> {
  prod_state: ProdState<
    'a,
    StreamTerminal<T>,
    StartNonTerminal<NT>,
    StartActionKey<AK>,
    StartActionValue<AV>,
  >,
  origin_index: usize,
}

impl<'a, T, NT, AK, AV> EarleyStateKey<'a, T, NT, AK, AV>
where
  T: Eq,
{
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
  Eq(bound = "NT: Ord"),
  PartialEq(bound = "NT: Ord"),
  Ord(bound = "NT: Ord"),
  PartialOrd(bound = "NT: Ord")
)]
pub struct EarleyState<'a, T, NT, AK, AV, V> {
  key: EarleyStateKey<'a, T, NT, AK, AV>,
  #[derivative(Debug = "ignore")]
  value: Vector<Node<'a, T, AK, V>>,
}
impl<'a, T, NT, AK, AV, V> EarleyState<'a, T, NT, AK, AV, V> {
  pub fn from_prod_start(
    prod: Prod<
      'a,
      StreamTerminal<T>,
      StartNonTerminal<NT>,
      StartActionKey<AK>,
      StartActionValue<AV>,
    >,
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

  pub fn prod_state(
    &self,
  ) -> &ProdState<
    'a,
    StreamTerminal<T>,
    StartNonTerminal<NT>,
    StartActionKey<AK>,
    StartActionValue<AV>,
  > {
    &self.key.prod_state
  }

  pub fn create_next_value(
    &self,
    node: Node<'a, T, AK, V>,
  ) -> Vector<Node<'a, T, AK, V>> {
    let mut value = self.value.clone();
    value.push_back(node);
    value
  }
}

impl<'a, T, NT, AK, AV, V> EarleyState<'a, T, NT, AK, AV, V>
where
  T: Ord + Clone,
  NT: Ord + Clone,
  AK: Ord + Clone,
  V: Ord + Clone,
{
  pub fn scan(
    &self,
    tree_handle: &TreeHandle<'a, T, AK, V>,
    next_token: &Token<T, V>,
  ) -> Option<EarleyState<'a, T, NT, AK, AV, V>> {
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
}

impl<'a, T, NT, AK, AV, V> EarleyState<'a, T, NT, AK, AV, V>
where
  T: Ord + Clone + std::fmt::Debug,
  NT: Ord + Clone + std::fmt::Debug,
  AK: Ord + Clone,
  V: Ord + Clone,
{
  pub fn predict(
    &self,
    grammar: &'a StartGrammar<T, NT, AK, AV>,
    prev_states: &[EarleyStateSet<'a, T, NT, AK, AV, V>],
  ) -> Vec<EarleyState<'a, T, NT, AK, AV, V>> {
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

  pub fn complete(
    &self,
    tree_handle: &TreeHandle<'a, T, AK, V>,
    prev_states: &[EarleyStateSet<'a, T, NT, AK, AV, V>],
    curr_state: &EarleyStateSet<'a, T, NT, AK, AV, V>,
  ) -> Vec<EarleyState<'a, T, NT, AK, AV, V>> {
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

impl<'a, T, NT, AK, AV, V> std::fmt::Debug for EarleyState<'a, T, NT, AK, AV, V>
where
  T: std::fmt::Debug,
  NT: std::fmt::Debug,
{
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    let mut st = f.debug_struct("EarleyState");
    st.field("prod_state", &self.key.prod_state);
    st.field("origin_index", &self.key.origin_index);
    st.field("value_len", &self.value.len());
    st.finish()
  }
}

pub struct EarleyStateSet<'a, T, NT, AK, AV, V> {
  states:
    BTreeMap<EarleyStateKey<'a, T, NT, AK, AV>, Vector<Node<'a, T, AK, V>>>,
}

impl<T, NT, AK, AV, V> Default for EarleyStateSet<'_, T, NT, AK, AV, V> {
  fn default() -> Self {
    EarleyStateSet {
      states: BTreeMap::new(),
    }
  }
}
impl<'a, T, NT, AK, AV, V> EarleyStateSet<'a, T, NT, AK, AV, V> {
  pub fn new() -> Self {
    EarleyStateSet {
      states: BTreeMap::new(),
    }
  }

  pub fn is_empty(&self) -> bool {
    self.states.is_empty()
  }

  pub fn states<'b>(
    &'b self,
  ) -> impl Iterator<Item = EarleyState<'a, T, NT, AK, AV, V>> + 'b {
    self.states.iter().map(|(key, value)| EarleyState {
      key: key.clone(),
      value: value.clone(),
    })
  }
}

impl<'a, T, NT, AK, AV, V> EarleyStateSet<'a, T, NT, AK, AV, V>
where
  T: Ord + Clone,
  NT: Ord + Clone,
  AK: Ord + Clone,
  V: Ord + Clone,
{
  pub fn new_start(grammar: &'a StartGrammar<T, NT, AK, AV>) -> Self {
    let start_nt = grammar.start_nt();
    let start_rule = grammar.get_rule(start_nt);
    EarleyStateSet::from_states(
      start_rule
        .prods()
        .map(|prod| EarleyState::from_prod_start(prod, 0)),
    )
  }

  pub fn from_states(
    iter: impl Iterator<Item = EarleyState<'a, T, NT, AK, AV, V>>,
  ) -> Self {
    let mut state_set = EarleyStateSet::new();
    for state in iter {
      state_set.insert(&state);
    }

    state_set
  }

  pub fn insert(
    &mut self,
    state: &EarleyState<'a, T, NT, AK, AV, V>,
  ) -> WasChanged {
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
    tree_handle: &TreeHandle<'a, T, AK, V>,
    token: &Token<T, V>,
  ) -> Self {
    EarleyStateSet::from_states(
      self
        .states()
        .into_iter()
        .filter_map(|state| state.scan(tree_handle, token)),
    )
  }

  pub fn get_final(&self) -> Option<&Node<'a, T, AK, V>> {
    for (k, v) in &self.states {
      if k.is_final_key() {
        assert_eq!(v.len(), 1);
        return Some(v.get(0).unwrap());
      }
    }

    None
  }
}

impl<'a, T, NT, AK, AV, V> std::fmt::Debug
  for EarleyStateSet<'a, T, NT, AK, AV, V>
where
  T: std::fmt::Debug,
  NT: std::fmt::Debug,
{
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    f.debug_set().entries(self.states()).finish()
  }
}
