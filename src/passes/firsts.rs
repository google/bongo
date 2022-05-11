use std::collections::BTreeSet;

use crate::grammar2::{
  Grammar, NonTermKey, NonTermPass, Pass, PassContext, TermKey,
};

use super::nullable::Nullable;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Default)]
pub struct Firsts;

impl<G> NonTermPass<G> for Firsts
where
  G: Grammar + 'static,
{
  type NonTermValue = BTreeSet<TermKey<G>>;
}

impl<G> Pass<G> for Firsts
where
  G: Grammar + 'static,
{
  fn execute(&self, pass_context: &mut PassContext<Self, G>) {
    let nullables = pass_context.pass_set().get_non_term_values(&Nullable);
    let prods = pass_context.grammar().prods();
  }
}
