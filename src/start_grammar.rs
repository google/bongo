//! Traditionally grammars are defined in terms of a start symbol, with a special non-terminal
//! being defined such as:
//!
//! S' -> S <EOS>
//!
//! where <EOS> is a unique end-of-stream value. This allows the <EOS> terminal to be used in various passes over the grammar, such as in
//! lookahead generation. Since we want to deal with an arbitrary grammar, we create a grammar
//! wrapper type that introduces the special `Start` non-terminal, and the `EndOfStream` terminal.
//! Passes may then take these into account when generation their representations.

use crate::grammar::{
  build, Elem, Grammar, GrammarErrors, Prod, ProdElement, Rule,
};
use crate::utils::{take_only, ToDoc};

/// A terminal wrapper type that adds the special `EndOfStream` terminal.
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

/// A non-terminal wrapper type that adds the special `Start` non-terminal.
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

pub type StartGrammar<T, NT, AK, AV> = Grammar<
  StreamTerminal<T>,
  StartNonTerminal<NT>,
  StartActionKey<AK>,
  StartActionValue<AV>,
>;

impl<T, NT, AK, AV> StartGrammar<T, NT, AK, AV>
where
  NT: Ord + Clone,
  AK: Ord + Clone,
{
  pub fn start_rule(
    &self,
  ) -> Rule<
    StreamTerminal<T>,
    StartNonTerminal<NT>,
    StartActionKey<AK>,
    StartActionValue<AV>,
  > {
    self.get_rule(&StartNonTerminal::Start)
  }

  pub fn start_prod(
    &self,
  ) -> Prod<
    StreamTerminal<T>,
    StartNonTerminal<NT>,
    StartActionKey<AK>,
    StartActionValue<AV>,
  > {
    take_only(self.start_rule().prods())
      .expect("The start rule should only have a single production.")
  }
}

fn base_elem_to_start_elem<T, NT>(
  elem: Elem<T, NT>,
) -> Elem<StreamTerminal<T>, StartNonTerminal<NT>> {
  match elem {
    Elem::Term(t) => Elem::Term(StreamTerminal::Term(t)),
    Elem::NonTerm(nt) => Elem::NonTerm(StartNonTerminal::NTerm(nt)),
  }
}

pub fn wrap_grammar_with_start<T, NT, AK, AV>(
  g: Grammar<T, NT, AK, AV>,
) -> Result<StartGrammar<T, NT, AK, AV>, GrammarErrors<StartNonTerminal<NT>>>
where
  T: Clone,
  NT: Ord + Clone,
  AK: Ord + Clone,
  AV: Clone,
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
