use crate::grammar::ElementTypes;
use crate::grammar::{Grammar, Rule, Prod};
use std::marker::PhantomData;

#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
pub enum StartTerminal<T> {
  EndOfFile,
  Term(T),
}

#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
pub enum StartNonTerminal<NT> {
  Start,
  NTerm(NT),
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub struct StartElementTypes<E>(PhantomData<E>);

impl<E: ElementTypes> ElementTypes for StartElementTypes<E> {
  type Term = StartTerminal<E::Term>;
  type NonTerm = StartNonTerminal<E::NonTerm>;

  type ActionKey = E::ActionKey;
  type ActionValue = E::ActionValue;
}

impl<E: ElementTypes> Grammar<StartElementTypes<E>> {
  pub fn start_rule(&self) -> Rule<StartElementTypes<E>> {
    self.get_rule(&StartNonTerminal::Start).unwrap()
  }

  pub fn start_prod(&self) -> Prod<StartElementTypes<E>> {
    let prods = self.start_rule().prods();
    assert_eq!(prods.len(), 1);
    prods.into_iter().next().unwrap()
  }
}
