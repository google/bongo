use std::collections::{BTreeMap, BTreeSet};

use crate::grammar::{Element, ElementTypes, Grammar, Production};
use crate::utils::{TreeNode, TreeValue, Void};

use failure::Error;

#[derive(Clone)]
struct InternalNullableInfo<A> {
  nullable_actions: BTreeSet<A>,
}

impl<A: Ord> InternalNullableInfo<A> {
  pub fn new() -> Self {
    InternalNullableInfo {
      nullable_actions: BTreeSet::new(),
    }
  }
}

#[derive(Copy, Clone, Debug)]
struct ProdWithHead<'a, E: ElementTypes> {
  head: &'a E::NonTerm,
  prod: &'a Production<E>,
}

fn grammar_to_prods_with_heads<E: ElementTypes>(
  g: &Grammar<E>,
) -> Vec<ProdWithHead<E>> {
  let mut result = Vec::new();
  for (head, rule) in g.rule_set() {
    for prod in rule.prods() {
      result.push(ProdWithHead { head, prod });
    }
  }
  result
}

/// Calculate the nullable set of a grammar
///
/// The nullable set of a grammar is the set of non-terminals in that grammar
/// that can parse the empty terminal sequence.
fn inner_calculate_nullables<E: ElementTypes>(
  g: &Grammar<E>,
) -> BTreeMap<E::NonTerm, InternalNullableInfo<E::ActionKey>> {
  let prods_with_heads = grammar_to_prods_with_heads(g);

  let mut nullable_nts = BTreeMap::new();

  loop {
    let mut changed = false;

    for prod_with_head in &prods_with_heads {
      if is_prod_nullable(&nullable_nts, prod_with_head.prod) {
        let nullable_info = nullable_nts
          .entry(prod_with_head.head.clone())
          .or_insert_with(InternalNullableInfo::new);
        if nullable_info
          .nullable_actions
          .insert(prod_with_head.prod.action_key().clone())
        {
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
pub struct GrammarNullableInfo<E: ElementTypes> {
  nonterm_info: BTreeMap<E::NonTerm, NonTermNullableInfo<E>>,
}

impl<E: ElementTypes> GrammarNullableInfo<E> {
  pub fn nonterm_info(&self) -> &BTreeMap<E::NonTerm, NonTermNullableInfo<E>> {
    &self.nonterm_info
  }

  pub fn is_nullable(&self, nt: &E::NonTerm) -> bool {
    self.nonterm_info.contains_key(nt)
  }

  pub fn is_prod_nullable(&self, prod: &Production<E>) -> bool {
    is_prod_nullable(&self.nonterm_info, prod)
  }

  pub fn get_nullable_info(
    &self,
    nt: &E::NonTerm,
  ) -> Option<&NonTermNullableInfo<E>> {
    self.nonterm_info.get(nt)
  }

  pub fn get_nullable_set(&self) -> BTreeSet<E::NonTerm> {
    self.nonterm_info().keys().cloned().collect()
  }
}

#[derive(Clone, Debug)]
pub struct NonTermNullableInfo<E: ElementTypes> {
  nullable_action: TreeNode<E::ActionKey, Void>,
}

impl<E: ElementTypes> NonTermNullableInfo<E> {
  pub fn nullable_action(&self) -> &TreeNode<E::ActionKey, Void> {
    &self.nullable_action
  }
}

#[derive(Debug, Fail)]
pub enum NullableError {
  #[fail(display = "found nullable ambiguities in grammar")]
  NullableAmbiguity,
}

pub fn calculate_nullables<E: ElementTypes>(
  g: &Grammar<E>,
) -> Result<GrammarNullableInfo<E>, Error> {
  let inner_info = inner_calculate_nullables(g);

  // Sanity check outputs.
  for (_, info) in &inner_info {
    let actions_length = info.nullable_actions.len();
    if actions_length == 0 {
      panic!("Unexpectedly empty nullable action!")
    } else if actions_length > 1 {
      return Err(NullableError::NullableAmbiguity.into());
    }
  }

  // Calculate simple nullable map, mapping one nullable nonterminal to one
  // production action
  let mut nullable_action_map = BTreeMap::new();
  for (nt, info) in &inner_info {
    nullable_action_map.insert(nt, get_only(&info.nullable_actions));
  }

  let action_map = g.get_action_map();

  let mut remaining_nullables: BTreeSet<E::NonTerm> =
    inner_info.keys().cloned().collect();
  let mut nullable_infos: BTreeMap<E::NonTerm, NonTermNullableInfo<E>> =
    BTreeMap::new();
  loop {
    'outer: for null_nt in &remaining_nullables {
      let action = nullable_action_map.get(&null_nt).unwrap();
      let prod_and_head = action_map.get(action).unwrap();

      let mut nullable_tree_fields: BTreeMap<_, TreeValue<_, Void>> =
        BTreeMap::new();
      for prod_elem in prod_and_head.prod.prod_elements() {
        if let (Some(id), Element::NonTerm(nt)) =
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
          nullable_action: TreeNode::new(
            (*action).clone(),
            nullable_tree_fields,
          ),
        },
      );
    }

    nullable_infos.keys().for_each(|nt| {
      remaining_nullables.remove(nt);
    });

    if remaining_nullables.len() == 0 {
      break;
    }
  }

  Ok(GrammarNullableInfo {
    nonterm_info: nullable_infos,
  })
}

fn is_prod_nullable<E: ElementTypes, V>(
  nullables: &BTreeMap<E::NonTerm, V>,
  prod: &Production<E>,
) -> bool {
  for elem in prod.elements_iter() {
    match elem {
      Element::Term(_) => return false,
      Element::NonTerm(nt) => {
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
  use crate::grammar::NonTerminal;
  use crate::grammar::examples;

  #[test]
  fn test_simple_grammar() {
    let g = examples::make_simple();
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
    assert!(nullables.get_nullable_info(&NonTerminal::new("expr_list")).is_some());
    assert!(nullables.get_nullable_info(&NonTerminal::new("expr")).is_none());
    assert!(nullables.get_nullable_info(&NonTerminal::new("start")).is_none());
  }

  #[test]
  fn test_ambiguous_nullable_grammar() {
    let g = examples::make_ambiguous_nullable();
    let nullable_error = calculate_nullables(&g).unwrap_err();
    assert!(nullable_error.downcast_ref::<NullableError>().is_some());
  }
}
