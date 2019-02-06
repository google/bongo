use std::collections::{BTreeMap, BTreeSet};

use crate::grammar::{Element, ElementTypes, Grammar, Production};
use crate::utils::{TreeNode, Void};

#[derive(Clone)]
pub struct NullableInfo<A> {
  nullable_actions: BTreeSet<TreeNode<A, Void>>,
}

impl<A: Ord> NullableInfo<A> {
  pub fn new() -> Self { NullableInfo { nullable_actions: BTreeSet::new() } }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
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
pub fn calculate_nullables<E: ElementTypes>(
  g: &Grammar<E>,
) -> BTreeMap<E::NonTerm, NullableInfo<E::Action>> {
  let prods_with_heads = grammar_to_prods_with_heads(g);

  let mut nullable_nts = BTreeMap::new();

  loop {
    let mut changed = false;

    for prod_with_head in &prods_with_heads {
      if is_prod_nullable(&nullable_nts, prod_with_head.prod) {
        let new_node =
          TreeNode::from_action(prod_with_head.prod.action().clone());
        let nullable_info = nullable_nts
          .entry(prod_with_head.head.clone())
          .or_insert_with(NullableInfo::new);

        if nullable_info.nullable_actions.insert(new_node) {
          changed = true;
        }
      }
    }

    if !changed {
      return nullable_nts;
    }
  }
}

pub fn is_prod_nullable<E: ElementTypes, V>(
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
