use crate::grammar::ElemTypes;
use crate::grammar::{
  build, Elem, Grammar, GrammarErrors, Prod, ProdElement, Rule,
};
use crate::utils::take_only;
use std::marker::PhantomData;

#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
pub enum StreamTerminal<T> {
  EndOfStream,
  Term(T),
}

impl<T> StreamTerminal<T>
where
  T: Eq,
{
  pub fn has_kind(&self, kind: &T) -> bool {
    match self {
      StreamTerminal::Term(t) => t == kind,
      StreamTerminal::EndOfStream => false,
    }
  }

  pub fn is_eos(&self) -> bool {
    matches!(self, StreamTerminal::EndOfStream)
  }
}

#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
pub enum StartNonTerminal<NT> {
  Start,
  NTerm(NT),
}

#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
pub enum StartActionKey<AK> {
  Start,
  ActionKey(AK),
}

impl<AK> StartActionKey<AK> {
  pub fn as_base(&self) -> Option<&AK> {
    match self {
      StartActionKey::Start => None,
      StartActionKey::ActionKey(ak) => Some(ak),
    }
  }
}

#[derive(Clone, Debug)]
pub enum StartActionValue<AV> {
  Start,
  ActionValue(AV),
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub struct StartElementTypes<E>(PhantomData<E>);

impl<E: ElemTypes> ElemTypes for StartElementTypes<E> {
  type Term = StreamTerminal<E::Term>;
  type NonTerm = StartNonTerminal<E::NonTerm>;

  type ActionKey = StartActionKey<E::ActionKey>;
  type ActionValue = StartActionValue<E::ActionValue>;
}

pub type StartGrammar<E> = Grammar<StartElementTypes<E>>;

impl<E: ElemTypes> StartGrammar<E> {
  pub fn start_rule(&self) -> Rule<StartElementTypes<E>> {
    self.get_rule(&StartNonTerminal::Start).unwrap()
  }

  pub fn start_prod(&self) -> Prod<StartElementTypes<E>> {
    take_only(self.start_rule().prods())
      .expect("The start rule should only have a single production.")
  }
}

fn base_elem_to_start_elem<E: ElemTypes>(
  elem: Elem<E>,
) -> Elem<StartElementTypes<E>> {
  match elem {
    Elem::Term(t) => Elem::Term(StreamTerminal::Term(t)),
    Elem::NonTerm(nt) => Elem::NonTerm(StartNonTerminal::NTerm(nt)),
  }
}

pub fn wrap_grammar_with_start<E: ElemTypes>(
  g: Grammar<E>,
) -> Result<Grammar<StartElementTypes<E>>, GrammarErrors<StartElementTypes<E>>>
{
  build(StartNonTerminal::Start, |gb| {
    gb.add_rule(StartNonTerminal::Start, |rb| {
      rb.add_prod(StartActionKey::Start, StartActionValue::Start, |pb| {
        pb.add_named_nonterm(
          "start",
          StartNonTerminal::NTerm(g.start_nt().clone()),
        )
        .add_term(StreamTerminal::EndOfStream);
      });
    });

    for rule in g.rules() {
      gb.add_rule(StartNonTerminal::NTerm(rule.head().clone()), |rb| {
        for prod in rule.prods() {
          rb.add_prod_with_elems(
            StartActionKey::ActionKey(prod.action_key().clone()),
            StartActionValue::ActionValue(prod.action_value().clone()),
            prod
              .prod_elements()
              .iter()
              .map(|e| {
                ProdElement::new(
                  e.id().cloned(),
                  base_elem_to_start_elem(e.elem().clone()),
                )
              })
              .collect::<Vec<_>>(),
          );
        }
      });
    }
  })
}
