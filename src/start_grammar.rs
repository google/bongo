use crate::grammar::ElemTypes;
use crate::grammar::{
  build, Elem, Grammar, GrammarErrors, Prod, ProdElement, Rule,
};
use crate::utils::{take_only, ToDoc};
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

impl<T> ToDoc for StreamTerminal<T>
where
  T: ToDoc,
{
  fn to_doc<'a, DA: pretty::DocAllocator<'a>>(
    &self,
    da: &'a DA,
  ) -> pretty::DocBuilder<'a, DA>
  where
    DA::Doc: Clone,
  {
    match self {
      StreamTerminal::EndOfStream => da.text("<EOS>"),
      StreamTerminal::Term(t) => t.to_doc(da),
    }
  }
}

#[derive(Clone, PartialOrd, Ord, PartialEq, Eq, Debug)]
pub enum StartNonTerminal<NT> {
  Start,
  NTerm(NT),
}

impl<NT> ToDoc for StartNonTerminal<NT>
where
  NT: ToDoc,
{
  fn to_doc<'a, DA: pretty::DocAllocator<'a>>(
    &self,
    da: &'a DA,
  ) -> pretty::DocBuilder<'a, DA>
  where
    DA::Doc: Clone,
  {
    match self {
      StartNonTerminal::Start => da.text("<START>"),
      StartNonTerminal::NTerm(nt) => nt.to_doc(da),
    }
  }
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

impl<AK> ToDoc for StartActionKey<AK>
where
  AK: ToDoc,
{
  fn to_doc<'a, DA: pretty::DocAllocator<'a>>(
    &self,
    da: &'a DA,
  ) -> pretty::DocBuilder<'a, DA>
  where
    DA::Doc: Clone,
  {
    match self {
      StartActionKey::Start => da.text("<START>"),
      StartActionKey::ActionKey(ak) => ak.to_doc(da),
    }
  }
}

#[derive(Clone, Debug)]
pub enum StartActionValue<AV> {
  Start,
  ActionValue(AV),
}

impl<AV> ToDoc for StartActionValue<AV>
where
  AV: ToDoc,
{
  fn to_doc<'a, DA: pretty::DocAllocator<'a>>(
    &self,
    da: &'a DA,
  ) -> pretty::DocBuilder<'a, DA>
  where
    DA::Doc: Clone,
  {
    match self {
      StartActionValue::Start => da.text("<START>"),
      StartActionValue::ActionValue(av) => av.to_doc(da),
    }
  }
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
    self.get_rule(&StartNonTerminal::Start)
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
