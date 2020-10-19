use std::marker::PhantomData;
use crate::grammar::ElementTypes;
use derivative::Derivative;
use crate::pdisplay::LayoutDisplay;
use crate::grammar::{Grammar, Rule};
use codefmt::Layout;

#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
pub enum StartTerminal<T> {
  EndOfFile,
  Term(T),
}

impl<T: LayoutDisplay> LayoutDisplay for StartTerminal<T> {
  fn disp(&self) -> Layout {
    match self {
      StartTerminal::EndOfFile => Layout::text("<EOF>"),
      StartTerminal::Term(t) => t.disp(),
    }
  }
}

#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
pub enum StartNonTerminal<NT> {
  Start,
  NTerm(NT),
}

impl<NT: LayoutDisplay> LayoutDisplay for StartNonTerminal<NT> {
  fn disp(&self) -> Layout {
    match self {
      StartNonTerminal::Start => Layout::text("<START>"),
      StartNonTerminal::NTerm(nt) => nt.disp(),
    }
  }
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""),)]
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
}

