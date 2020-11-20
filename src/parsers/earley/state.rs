use {
  crate::{
    grammar::{Element, Prod},
    parsers::{
      tree::{Node, TreeHandle},
      Token,
    },
    start_grammar::{StartElementTypes, StartGrammar, StreamTerminal},
    state::ProdState,
    utils::{change_iter, WasChanged},
    ElementTypes,
  },
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
struct EarleyStateKey<'a, E: ElementTypes> {
  prod_state: ProdState<'a, StartElementTypes<E>>,
  origin_index: usize,
}

impl<'a, E: ElementTypes> EarleyStateKey<'a, E> {
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
  PartialOrd(bound = ""),
  Debug(bound = "")
)]
pub struct EarleyState<'a, E, T>
where
  E: ElementTypes,
{
  key: EarleyStateKey<'a, E>,
  #[derivative(Debug = "ignore")]
  value: Vec<Node<'a, E, T>>,
}

impl<'a, E, T> EarleyState<'a, E, T>
where
  E: ElementTypes,
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
      value: Vec::new(),
    }
  }

  pub fn prod_state(&self) -> &ProdState<'a, StartElementTypes<E>> {
    &self.key.prod_state
  }

  pub fn predict(
    &self,
    grammar: &'a StartGrammar<E>,
    prev_states: &[EarleyStateSet<'a, E, T>],
  ) -> Vec<EarleyState<'a, E, T>> {
    if let Some(Element::NonTerm(nt)) = self.prod_state().next_elem() {
      eprintln!("Predicting NT {:?} from prod state.", nt);
      let mut predicted = Vec::new();
      if let Some(rule) = grammar.get_rule(nt) {
        for prod in rule.prods() {
          predicted.push(EarleyState {
            key: EarleyStateKey {
              prod_state: ProdState::from_start(prod),
              origin_index: prev_states.len(),
            },
            value: Vec::new(),
          })
        }
      }

      predicted
    } else {
      Vec::new()
    }
  }

  pub fn scan(
    &self,
    tree_handle: &TreeHandle<'a, E, T>,
    next_token: &Token<E::Term, T>,
  ) -> Option<EarleyState<'a, E, T>> {
    if let Some((next_elem, next_prod_state)) =
      self.prod_state().next_elem_state()
    {
      if let Element::Term(kind) = next_elem.elem() {
        if kind.has_kind(&next_token.kind) {
          return Some(EarleyState {
            key: EarleyStateKey {
              prod_state: next_prod_state,
              origin_index: self.key.origin_index,
            },
            value: self
              .value
              .iter()
              .cloned()
              .chain(std::iter::once(tree_handle.make_leaf_node(
                next_token.kind.clone(),
                next_token.value.clone(),
              )))
              .collect(),
          });
        }
      }
    }

    None
  }

  pub fn complete(
    &self,
    tree_handle: &TreeHandle<'a, E, T>,
    prev_states: &[EarleyStateSet<'a, E, T>],
    curr_state: &EarleyStateSet<'a, E, T>,
  ) -> Vec<EarleyState<'a, E, T>> {
    eprintln!("Trying to complete prod state {:?}", self.prod_state());
    if self.prod_state().is_complete() {
      eprintln!("Completing prod state {:?}.", self.prod_state());
      let origin_state = if self.key.origin_index == prev_states.len() {
        curr_state
      } else {
        &prev_states[self.key.origin_index]
      };

      let mut complete_states = Vec::new();

      for state in origin_state.states() {
        if let Some((next_elem, next_prod_state)) =
          state.prod_state().next_elem_state()
        {
          if let Element::NonTerm(nt) = next_elem.elem() {
            if nt == self.prod_state().prod().head() {
              complete_states.push(EarleyState {
                key: EarleyStateKey {
                  prod_state: next_prod_state,
                  origin_index: state.key.origin_index,
                },
                value: state
                  .value
                  .iter()
                  .cloned()
                  .chain(std::iter::once(
                    tree_handle.make_branch_node(
                      self
                        .prod_state()
                        .prod()
                        .action_key()
                        .as_base()
                        .unwrap()
                        .clone(),
                      self.value.iter().cloned(),
                    ),
                  ))
                  .collect(),
              })
            }
          }
        }
      }
      complete_states
    } else {
      Vec::new()
    }
  }
}

pub struct EarleyStateSet<'a, E, T>
where
  E: ElementTypes,
{
  states: BTreeMap<EarleyStateKey<'a, E>, Vec<Node<'a, E, T>>>,
}

impl<'a, E, T> EarleyStateSet<'a, E, T>
where
  E: ElementTypes,
  T: Ord + Clone,
{
  pub fn new() -> Self {
    EarleyStateSet {
      states: BTreeMap::new(),
    }
  }

  pub fn new_start(grammar: &'a StartGrammar<E>) -> Self {
    let start_nt = grammar.start_nt();
    let start_rule =
      grammar.get_rule(start_nt).expect("Start NT must be valid.");
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
  E: ElementTypes,
  T: Ord + Clone,
{
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    f.debug_set().entries(self.states()).finish()
  }
}
