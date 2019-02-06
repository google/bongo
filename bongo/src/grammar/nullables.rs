use std::collections::{BTreeMap, BTreeSet};

use crate::grammar::{Element, ElementTypes, Grammar, Production};
use crate::utils::{TreeNode, TreeValue, Void};

#[derive(Clone)]
pub struct NullableInfo<A> {
  nullable_actions: BTreeSet<TreeNode<A, Void>>,
}

impl<A: Ord> NullableInfo<A> {
  pub fn new() -> Self {
    NullableInfo {
      nullable_actions: BTreeSet::new(),
    }
  }
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

fn calculate_nullable_tree_nodes<E: ElementTypes>(
  nullables: &BTreeMap<E::NonTerm, NullableInfo<E::Action>>,
  prod: &Production<E>,
) -> BTreeSet<TreeNode<E::Action, Void>> {
  let mut node_args_set = BTreeSet::new();
  node_args_set.insert(BTreeMap::new());
  for prod_elem in prod.prod_elements() {
    let nt = prod_elem.elem().as_nonterm();
    let nt_nullable_info = nullables.get(nt).unwrap();
    if let Some(id) = prod_elem.id() {
      let mut new_node_args_set = BTreeSet::new();
      for old_node_arg in &node_args_set {
        for nullable_action in &nt_nullable_info.nullable_actions {
          let mut new_node_arg = old_node_arg.clone();
          new_node_arg.insert(
            id.clone(),
            TreeValue::Node(Box::new(nullable_action.clone())),
          );
          new_node_args_set.insert(new_node_arg);
        }
      }

      node_args_set = new_node_args_set;
    }
  }

  node_args_set
    .into_iter()
    .map(|node_args| TreeNode::new(prod.action().clone(), node_args))
    .collect()
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
        let new_nodes =
          calculate_nullable_tree_nodes(&nullable_nts, prod_with_head.prod);

        let nullable_info = nullable_nts
          .entry(prod_with_head.head.clone())
          .or_insert_with(NullableInfo::new);
        for new_node in new_nodes {
          if nullable_info.nullable_actions.insert(new_node) {
            changed = true;
          }
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
