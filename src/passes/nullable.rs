use std::collections::BTreeSet;

use crate::grammar2::{Grammar, NamedElem, NonTerm, NonTermPass, Pass, Prod};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Nullable;

fn prod_is_nullable<P>(
  prod: &P,
  nullable_nterms: &BTreeSet<<P::NonTerm as NonTerm>::Key>,
) -> bool
where
  P: Prod,
{
  prod
    .prod_elements()
    .into_iter()
    .filter_map(|e| e.elem().into_nonterm_opt())
    .all(|nt| nullable_nterms.contains(&nt.key()))
}

impl<G> NonTermPass<G> for Nullable
where
  G: Grammar + 'static,
{
  type NonTermValue = bool;
}

impl<G> Pass<G> for Nullable
where
  G: Grammar + 'static,
{
  fn execute(&self, pass_context: &mut crate::grammar2::PassContext<Self, G>) {
    let mut nullable_nterms = BTreeSet::new();
    let mut changed = true;
    while changed {
      changed = false;
      for prod in pass_context.grammar().prods() {
        if nullable_nterms.contains(prod.head().key()) {
          continue;
        }

        if prod_is_nullable(&prod, &nullable_nterms) {
          nullable_nterms.insert(prod.head().key().clone());
          changed = true;
        }
      }
    }

    for nt_key in &nullable_nterms {
      pass_context.insert_non_term_value(nt_key, true);
    }
  }
}
