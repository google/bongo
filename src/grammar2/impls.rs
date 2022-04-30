pub mod builder;

use std::borrow::Cow;
use std::fmt;
use std::rc::{Rc, Weak};

use crate::utils::svec::{IdentityKeyExtractor, SVec};

use super::key_extractors::{NonTermKeyExtractor, ProdKeyExtractor};
use super::traits::{Grammar, NamedElem, NonTerm, Prod};
use super::Elem;

pub struct NamedElemRef<T, NT, ProdID, AV> {
  grammar: Weak<GrammarImpl<T, NT, ProdID, AV>>,
  name: Option<Rc<String>>,
  elem: Elem<T, NT>,
}

impl<T, NT, ProdID, AV> NamedElem for NamedElemRef<T, NT, ProdID, AV>
where
  T: Clone,
  NT: Ord + Clone,
  ProdID: Ord,
{
  type Term = T;
  type NonTerm = NonTermHandle<T, NT, ProdID, AV>;

  fn name(&self) -> Option<&str> {
    self.name.as_ref().map(|s| s.as_str())
  }

  fn elem(&self) -> Elem<Self::Term, Self::NonTerm> {
    match &self.elem {
      Elem::Term(t) => Elem::Term(t.clone()),
      Elem::NonTerm(nt) => {
        let grammar = self.grammar.upgrade().expect("grammar is still alive");
        Elem::NonTerm(
          grammar
            .non_terminals
            .get(nt)
            .expect("non-terminal is still alive")
            .clone(),
        )
      }
    }
  }
}

struct GrammarImpl<T, NT, ProdID, AV> {
  start_nt: NT,
  terminals: SVec<T, IdentityKeyExtractor>,
  non_terminals: SVec<NonTermHandle<T, NT, ProdID, AV>, NonTermKeyExtractor>,
  prods: SVec<ProdHandle<T, NT, ProdID, AV>, ProdKeyExtractor>,
}

struct ProdImpl<T, NT, ProdID, AV> {
  grammar: Weak<GrammarImpl<T, NT, ProdID, AV>>,
  head: NT,
  action_id: ProdID,
  action_value: AV,
  prod_elems: Vec<NamedElemImpl<T, NT>>,
}

struct NonTermImpl<T, NT, ProdID, AV> {
  grammar: Weak<GrammarImpl<T, NT, ProdID, AV>>,
  key: NT,
  prods: SVec<ProdID, IdentityKeyExtractor>,
}

pub struct NamedElemImpl<T, NT> {
  name: Option<Rc<String>>,
  elem: Elem<T, NT>,
}

impl<T, NT> NamedElemImpl<T, NT> {
  pub fn new<'a, S: Into<Cow<'a, str>>>(name: S, elem: Elem<T, NT>) -> Self {
    Self {
      name: Some(Rc::new(name.into().into_owned())),
      elem,
    }
  }

  pub fn name(&self) -> Option<&str> {
    self.name.as_ref().map(|n| &***n)
  }

  pub fn elem(&self) -> &Elem<T, NT> {
    &self.elem
  }
}

impl<T, NT> fmt::Debug for NamedElemImpl<T, NT>
where
  T: fmt::Debug,
  NT: fmt::Debug,
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self.name() {
      Some(name) => write!(f, "{}:{:?}", name, self.elem),
      None => write!(f, "{:?}", self.elem),
    }
  }
}

pub struct GrammarHandle<T, NT, ProdID, AV>(Rc<GrammarImpl<T, NT, ProdID, AV>>);

impl<T, NT, ProdID, AV> GrammarHandle<T, NT, ProdID, AV> {
  fn new<F>(inner: F) -> Self
  where
    F: FnOnce(
      &Weak<GrammarImpl<T, NT, ProdID, AV>>,
    ) -> GrammarImpl<T, NT, ProdID, AV>,
  {
    GrammarHandle(Rc::new_cyclic(inner))
  }
}

impl<T, NT, ProdID, AV> Clone for GrammarHandle<T, NT, ProdID, AV> {
  fn clone(&self) -> Self {
    GrammarHandle(Rc::clone(&self.0))
  }
}

impl<T, NT, ProdID, AV> fmt::Debug for GrammarHandle<T, NT, ProdID, AV>
where
  T: fmt::Debug,
  NT: fmt::Debug,
  ProdID: fmt::Debug,
  AV: fmt::Debug,
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.debug_struct("GrammarHandle")
      .field("start_non_term", &self.0.start_nt)
      .field("terminals", &self.0.terminals)
      .field("non_terminals", &self.0.non_terminals)
      .field("prods", &self.0.prods)
      .finish()
  }
}

/// A handle for Prod objects.
pub struct ProdHandle<T, NT, ProdID, AV>(Rc<ProdImpl<T, NT, ProdID, AV>>);

impl<T, NT, ProdID, AV> ProdHandle<T, NT, ProdID, AV> {
  fn new(
    grammar: Weak<GrammarImpl<T, NT, ProdID, AV>>,
    head: NT,
    action_id: ProdID,
    action_value: AV,
    prod_elems: Vec<NamedElemImpl<T, NT>>,
  ) -> Self {
    ProdHandle(Rc::new(ProdImpl {
      grammar,
      head,
      action_id,
      action_value,
      prod_elems,
    }))
  }
}

impl<T, NT, ProdID, AV> Clone for ProdHandle<T, NT, ProdID, AV> {
  fn clone(&self) -> Self {
    ProdHandle(Rc::clone(&self.0))
  }
}

impl<T, NT, ProdID, AV> fmt::Debug for ProdHandle<T, NT, ProdID, AV>
where
  T: fmt::Debug,
  NT: fmt::Debug,
  ProdID: fmt::Debug,
  AV: fmt::Debug,
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.debug_struct("ProdHandle")
      .field("head", &self.0.head)
      .field("action_id", &self.0.action_id)
      .field("action_value", &self.0.action_value)
      .field("prod_elems", &self.0.prod_elems)
      .finish()
  }
}

/// A handle for NonTerm objects.
pub struct NonTermHandle<T, NT, ProdID, AV>(Rc<NonTermImpl<T, NT, ProdID, AV>>);

impl<T, NT, ProdID, AV> NonTermHandle<T, NT, ProdID, AV>
where
  ProdID: Ord,
{
  fn new(
    grammar: Weak<GrammarImpl<T, NT, ProdID, AV>>,
    key: NT,
    prods: Vec<ProdID>,
  ) -> Self {
    NonTermHandle(Rc::new(NonTermImpl {
      grammar,
      key,
      prods: prods.into(),
    }))
  }
}

impl<T, NT, ProdID, AV> Clone for NonTermHandle<T, NT, ProdID, AV> {
  fn clone(&self) -> Self {
    NonTermHandle(Rc::clone(&self.0))
  }
}

impl<T, NT, ProdID, AV> fmt::Debug for NonTermHandle<T, NT, ProdID, AV>
where
  NT: fmt::Debug,
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    fmt::Debug::fmt(&self.0.key, f)
  }
}

impl<T, NT, ProdID, AV> Grammar for GrammarHandle<T, NT, ProdID, AV>
where
  T: Clone,
  NT: Ord + Clone,
  ProdID: Ord,
{
  type Term = T;

  type NonTerm = NonTermHandle<T, NT, ProdID, AV>;

  type Prod = ProdHandle<T, NT, ProdID, AV>;

  fn start_non_term(&self) -> &Self::NonTerm {
    self.0.non_terminals.get(&self.0.start_nt).unwrap()
  }

  fn terminals(&self) -> Vec<&Self::Term> {
    self.0.terminals.iter().collect()
  }

  fn non_terminals(&self) -> Vec<&Self::NonTerm> {
    self.0.non_terminals.iter().collect()
  }

  fn prods(&self) -> Vec<&Self::Prod> {
    self.0.prods.iter().collect()
  }

  fn get_prod(
    &self,
    prod_id: &<Self::Prod as Prod>::ProdId,
  ) -> Option<&Self::Prod> {
    self.0.prods.get(prod_id)
  }

  fn get_non_term(
    &self,
    key: &<Self::NonTerm as NonTerm>::Key,
  ) -> Option<&Self::NonTerm> {
    self.0.non_terminals.get(key)
  }
}

impl<T, NT, ProdID, AV> Prod for ProdHandle<T, NT, ProdID, AV>
where
  T: Clone,
  NT: Ord + Clone,
  ProdID: Ord,
{
  type Term = T;

  type NonTerm = NonTermHandle<T, NT, ProdID, AV>;

  type ProdId = ProdID;

  type ActionValue = AV;

  type NamedElem = NamedElemRef<T, NT, ProdID, AV>;

  fn head(&self) -> Self::NonTerm {
    let grammar = self.0.grammar.upgrade().expect("grammar is alive");
    grammar
      .non_terminals
      .get(&self.0.head)
      .expect("non-term key exists in grammar")
      .clone()
  }

  fn action_id(&self) -> &Self::ProdId {
    &self.0.action_id
  }

  fn action_value(&self) -> &Self::ActionValue {
    &self.0.action_value
  }

  fn prod_elements(&self) -> Vec<Self::NamedElem> {
    self
      .0
      .prod_elems
      .iter()
      .map(|elem| NamedElemRef {
        grammar: self.0.grammar.clone(),
        name: elem.name.clone(),
        elem: elem.elem.clone(),
      })
      .collect()
  }
}

impl<T, NT, ProdID, AV> NonTerm for NonTermHandle<T, NT, ProdID, AV>
where
  T: Clone,
  NT: Ord + Clone,
  ProdID: Ord,
{
  type Key = NT;
  type Prod = ProdHandle<T, NT, ProdID, AV>;

  fn key(&self) -> &Self::Key {
    &self.0.key
  }

  fn prods(&self) -> Vec<Self::Prod> {
    let grammar = self.0.grammar.upgrade().expect("grammar is alive");
    self
      .0
      .prods
      .iter()
      .map(|prod_id| {
        grammar
          .prods
          .get(prod_id)
          .expect("prod key exists in grammar")
          .clone()
      })
      .collect()
  }
}
