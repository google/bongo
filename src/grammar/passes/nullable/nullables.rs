// Copyright 2019 Google LLC
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
use {
  crate::{
    grammar::{Elem, Grammar, Prod, ProdKey},
    utils::{TreeNode, TreeValue, Void},
  },
  std::collections::{BTreeMap, BTreeSet},
};
struct InternalNullableInfo<'a, T, NT, AK, AV> {
  /// The set of productions that are nullable
  nullable_actions: BTreeSet<Prod<'a, T, NT, AK, AV>>,
}

impl<'a, T, NT, AK, AV> Ord for InternalNullableInfo<'a, T, NT, AK, AV>
where
  NT: Ord,
{
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.nullable_actions.cmp(&other.nullable_actions)
  }
}

impl<'a, T, NT, AK, AV> PartialOrd for InternalNullableInfo<'a, T, NT, AK, AV>
where
  NT: Ord,
{
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl<'a, T, NT, AK, AV> PartialEq for InternalNullableInfo<'a, T, NT, AK, AV>
where
  NT: Ord,
{
  fn eq(&self, other: &Self) -> bool {
    self.nullable_actions == other.nullable_actions
  }
}

impl<'a, T, NT, AK, AV> Eq for InternalNullableInfo<'a, T, NT, AK, AV> where
  NT: Ord
{
}

impl<T, NT, AK, AV> InternalNullableInfo<'_, T, NT, AK, AV> {
  pub fn new() -> Self {
    InternalNullableInfo {
      nullable_actions: BTreeSet::new(),
    }
  }
}

/// Calculate the nullable set of a grammar
///
/// The nullable set of a grammar is the set of non-terminals in that grammar
/// that can parse the empty terminal sequence.
fn inner_calculate_nullables<T, NT, AK, AV>(
  g: &Grammar<T, NT, AK, AV>,
) -> BTreeMap<NT, InternalNullableInfo<T, NT, AK, AV>>
where
  T: Ord,
  NT: Ord + Clone,
{
  let prods = g.prods().collect::<Vec<_>>();

  let mut nullable_nts: BTreeMap<NT, InternalNullableInfo<T, NT, AK, AV>> =
    BTreeMap::new();

  loop {
    let mut changed = false;

    for prod in &prods {
      if is_prod_nullable(&nullable_nts, prod) {
        let nullable_info = nullable_nts
          .entry(prod.head().clone())
          .or_insert_with(InternalNullableInfo::new);
        if nullable_info.nullable_actions.insert(*prod) {
          changed = true;
        }
      }
    }

    if !changed {
      return nullable_nts;
    }
  }
}

#[derive(Clone, Debug)]
pub struct GrammarNullableInfo<NT, AK> {
  nonterm_info: BTreeMap<NT, NonTermNullableInfo<NT, AK>>,
}

impl<NT, AK> GrammarNullableInfo<NT, AK>
where
  NT: Ord + Clone,
{
  fn nonterm_info(&self) -> &BTreeMap<NT, NonTermNullableInfo<NT, AK>> {
    &self.nonterm_info
  }

  pub fn is_nullable(&self, nt: &NT) -> bool {
    self.nonterm_info.contains_key(nt)
  }

  pub fn is_prod_nullable<T, AV>(&self, prod: &Prod<T, NT, AK, AV>) -> bool {
    is_prod_nullable(&self.nonterm_info, prod)
  }

  pub fn get_nullable_action(
    &self,
    nt: &NT,
  ) -> Option<&TreeNode<ProdKey<NT, AK>, Void>> {
    self.nonterm_info.get(nt).map(|info| &info.nullable_action)
  }

  pub fn get_nullable_set(&self) -> BTreeSet<NT> {
    self.nonterm_info().keys().cloned().collect()
  }
}

#[derive(Clone, Debug)]
struct NonTermNullableInfo<NT, AK> {
  nullable_action: TreeNode<ProdKey<NT, AK>, Void>,
}

impl<NT, AK> NonTermNullableInfo<NT, AK> {
  pub fn nullable_action(&self) -> &TreeNode<ProdKey<NT, AK>, Void> {
    &self.nullable_action
  }
}

#[derive(Debug, thiserror::Error)]
pub enum NullableError {
  #[error("found nullable ambiguities in grammar")]
  Ambiguity,
}

pub fn calculate_nullables<T, NT, AK, AV>(
  g: &Grammar<T, NT, AK, AV>,
) -> Result<GrammarNullableInfo<NT, AK>, NullableError>
where
  T: Ord,
  NT: Ord + Clone,
  AK: Ord + Clone,
{
  let inner_info = inner_calculate_nullables(g);

  // Sanity check outputs.
  for info in inner_info.values() {
    let actions_length = info.nullable_actions.len();
    if actions_length == 0 {
      panic!("Unexpectedly empty nullable action!")
    } else if actions_length > 1 {
      return Err(NullableError::Ambiguity);
    }
  }

  // Calculate simple nullable map, mapping one nullable nonterminal to one
  // production action
  let mut nullable_action_map = BTreeMap::new();
  for (nt, info) in &inner_info {
    nullable_action_map.insert(nt, get_only(&info.nullable_actions));
  }

  let mut remaining_nullables =
    inner_info.keys().cloned().collect::<BTreeSet<_>>();
  let mut nullable_infos: BTreeMap<NT, NonTermNullableInfo<NT, AK>> =
    BTreeMap::new();
  loop {
    'outer: for null_nt in &remaining_nullables {
      let prod = nullable_action_map.get(&null_nt).unwrap();

      let mut nullable_tree_fields: BTreeMap<_, TreeValue<_, Void>> =
        BTreeMap::new();
      for prod_elem in prod.prod_elements() {
        if let (Some(id), Elem::NonTerm(nt)) =
          (prod_elem.id(), prod_elem.elem())
        {
          match nullable_infos.get(nt) {
            None => continue 'outer,
            Some(s) => nullable_tree_fields.insert(
              id.clone(),
              TreeValue::Node(Box::new(s.nullable_action.clone())),
            ),
          };
        }
      }

      nullable_infos.insert(
        null_nt.clone(),
        NonTermNullableInfo {
          nullable_action: TreeNode::new(prod.prod_key(), nullable_tree_fields),
        },
      );
    }

    nullable_infos.keys().for_each(|nt| {
      remaining_nullables.remove(nt);
    });

    if remaining_nullables.is_empty() {
      break;
    }
  }

  Ok(GrammarNullableInfo {
    nonterm_info: nullable_infos,
  })
}

fn is_prod_nullable<T, NT, AK, AV, V>(
  nullables: &BTreeMap<NT, V>,
  prod: &Prod<T, NT, AK, AV>,
) -> bool
where
  NT: Ord,
{
  for elem in prod.elements() {
    match elem {
      Elem::Term(_) => return false,
      Elem::NonTerm(nt) => {
        if !nullables.contains_key(nt) {
          return false;
        }
      }
    }
  }
  true
}

// Helpers

fn get_only<I: IntoIterator>(op: I) -> I::Item {
  let mut iter = op.into_iter();
  let value = iter.next().expect("get_only argument must not be empty.");
  assert!(
    iter.next().is_none(),
    "get_only argument must not have more than one value."
  );
  value
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::grammar::examples;
  use crate::grammar::NonTerminal;
  use crate::start_grammar::wrap_grammar_with_start;

  #[test]
  fn test_simple_grammar() {
    let g = wrap_grammar_with_start(examples::make_simple()).unwrap();
    let nullables = calculate_nullables(&g).unwrap();
    assert!(nullables.get_nullable_set().is_empty());
  }

  #[test]
  fn test_simple_nullable_grammar() {
    let g = examples::make_simple_nullable();
    let nullables = calculate_nullables(&g).unwrap();
    let nullable_set = nullables.get_nullable_set();
    assert!(nullable_set.contains(&NonTerminal::new("start")));
    assert!(nullable_set.contains(&NonTerminal::new("a")));
    assert!(nullable_set.contains(&NonTerminal::new("b")));
    assert!(nullable_set.contains(&NonTerminal::new("c")));
  }

  #[test]
  fn test_paren_grammar() {
    let g = examples::make_paren();
    let nullables = calculate_nullables(&g).unwrap();
    assert!(nullables
      .get_nullable_action(&NonTerminal::new("expr_list"))
      .is_some());
    assert!(nullables
      .get_nullable_action(&NonTerminal::new("expr"))
      .is_none());
    assert!(nullables
      .get_nullable_action(&NonTerminal::new("start"))
      .is_none());
  }

  #[test]
  fn test_ambiguous_nullable_grammar() {
    let g = examples::make_ambiguous_nullable();
    let nullable_error = calculate_nullables(&g).unwrap_err();
    assert!(matches!(nullable_error, NullableError::Ambiguity));
  }
}
