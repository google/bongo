use std::{
  borrow::Cow,
  fmt,
  rc::{Rc, Weak},
};

use crate::utils::svec::{IdentityKeyExtractor, KeyExtractor, SVec};

mod builder;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Elem<T, NT> {
  Term(T),
  NonTerm(NT),
}

impl<T, NT> Elem<T, NT> {
  pub fn as_ref(&self) -> Elem<&T, &NT> {
    match self {
      Elem::Term(t) => Elem::Term(t),
      Elem::NonTerm(nt) => Elem::NonTerm(nt),
    }
  }

  pub fn into_term_opt(self) -> Option<T> {
    match self {
      Elem::Term(t) => Some(t),
      Elem::NonTerm(_) => None,
    }
  }

  pub fn into_nonterm_opt(self) -> Option<NT> {
    match self {
      Elem::Term(_) => None,
      Elem::NonTerm(nt) => Some(nt),
    }
  }
}

impl<T, NT> fmt::Debug for Elem<T, NT>
where
  T: fmt::Debug,
  NT: fmt::Debug,
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Elem::Term(t) => write!(f, "<{:?}>", t),
      Elem::NonTerm(nt) => write!(f, "{:?}", nt),
    }
  }
}

pub trait NamedElem {
  type Term;
  type NonTerm;

  fn name(&self) -> Option<&str>;
  fn elem(&self) -> Elem<Self::Term, Self::NonTerm>;
}

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

pub trait NonTerm {
  type Key;
  type Rule: Rule<NonTerm = Self, Key = Self::Key>;

  /// Gets the key of the non-terminal.
  fn key(&self) -> &Self::Key;

  /// Gets the rule in the grammar that has this non-terminal as a head.
  fn rule(&self) -> Self::Rule;
}

/// Key extractor for `Rule`, using the `NonTerm`'s `Key`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub struct NonTermKeyExtractor;

impl<T> KeyExtractor<T> for NonTermKeyExtractor
where
  T: NonTerm,
{
  type Key = T::Key;

  fn extract_key<'a>(&self, rule: &'a T) -> &'a Self::Key {
    rule.key()
  }
}

pub trait Prod {
  type Term;
  type NonTerm: NonTerm<Rule = Self::Rule>;
  type ProdId;
  type ActionValue;
  type Rule: Rule<Term = Self::Term, NonTerm = Self::NonTerm, Prod = Self>;
  type NamedElem: NamedElem<Term = Self::Term, NonTerm = Self::NonTerm>;

  fn head(&self) -> Self::NonTerm;
  fn action_id(&self) -> &Self::ProdId;
  fn action_value(&self) -> &Self::ActionValue;
  fn prod_elements(&self) -> Vec<Self::NamedElem>;
}

/// Key extractor for `Rule`, using the `NonTerm`'s `Key`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub struct ProdKeyExtractor;

impl<T> KeyExtractor<T> for ProdKeyExtractor
where
  T: Prod,
{
  type Key = T::ProdId;

  fn extract_key<'a>(&self, rule: &'a T) -> &'a Self::Key {
    rule.action_id()
  }
}

pub trait Rule {
  type Term;
  type Key;
  type NonTerm: NonTerm<Key = Self::Key, Rule = Self>;
  type Prod: Prod<Term = Self::Term, NonTerm = Self::NonTerm, Rule = Self>;

  fn key(&self) -> &Self::Key;
  fn head(&self) -> Self::NonTerm;
  fn prods(&self) -> Vec<Self::Prod>;
}

/// Key extractor for `Rule`, using the `NonTerm`'s `Key`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub struct RuleKeyExtractor;

impl<T> KeyExtractor<T> for RuleKeyExtractor
where
  T: Rule,
{
  type Key = T::Key;

  fn extract_key<'a>(&self, rule: &'a T) -> &'a Self::Key {
    rule.key()
  }
}

pub trait Grammar {
  type Term;
  type NonTerm: NonTerm<Rule = Self::Rule>;
  type ProdId;
  type Prod: Prod<
    Term = Self::Term,
    NonTerm = Self::NonTerm,
    Rule = Self::Rule,
    ProdId = Self::ProdId,
  >;
  type Rule: Rule<Term = Self::Term, NonTerm = Self::NonTerm, Prod = Self::Prod>;

  fn start_non_term(&self) -> &Self::NonTerm;
  fn terminals(&self) -> Vec<&Self::Term>;
  fn non_terminals(&self) -> Vec<&Self::NonTerm>;
  fn rules(&self) -> Vec<&Self::Rule>;
  fn get_prod(&self, prod_id: &Self::ProdId) -> Option<&Self::Prod>;
}

pub struct GrammarImpl<T, NT, ProdID, AV> {
  start_nt: NT,
  terminals: SVec<T, IdentityKeyExtractor>,
  non_terminals: SVec<NonTermHandle<T, NT, ProdID, AV>, NonTermKeyExtractor>,
  rules: SVec<RuleHandle<T, NT, ProdID, AV>, RuleKeyExtractor>,
  prods: SVec<ProdHandle<T, NT, ProdID, AV>, ProdKeyExtractor>,
}

struct RuleImpl<T, NT, ProdID, AV> {
  grammar: Weak<GrammarImpl<T, NT, ProdID, AV>>,
  head: NT,
  prods: SVec<ProdID, IdentityKeyExtractor>,
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

/// A handle for Rule objects.
pub struct RuleHandle<T, NT, ProdID, AV>(Rc<RuleImpl<T, NT, ProdID, AV>>);

impl<T, NT, ProdID, AV> RuleHandle<T, NT, ProdID, AV>
where
  ProdID: Ord,
{
  fn new(
    grammar: Weak<GrammarImpl<T, NT, ProdID, AV>>,
    head: NT,
    prods: Vec<ProdID>,
  ) -> Self {
    RuleHandle(Rc::new(RuleImpl {
      head,
      grammar,
      prods: prods.into(),
    }))
  }
}

impl<T, NT, ProdID, AV> Clone for RuleHandle<T, NT, ProdID, AV> {
  fn clone(&self) -> Self {
    RuleHandle(Rc::clone(&self.0))
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

impl<T, NT, ProdID, AV> NonTermHandle<T, NT, ProdID, AV> {
  fn new(grammar: Weak<GrammarImpl<T, NT, ProdID, AV>>, key: NT) -> Self {
    NonTermHandle(Rc::new(NonTermImpl { grammar, key }))
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

  type ProdId = ProdID;

  type Prod = ProdHandle<T, NT, ProdID, AV>;

  type Rule = RuleHandle<T, NT, ProdID, AV>;

  fn start_non_term(&self) -> &Self::NonTerm {
    self.0.non_terminals.get(&self.0.start_nt).unwrap()
  }

  fn terminals(&self) -> Vec<&Self::Term> {
    self.0.terminals.iter().collect()
  }

  fn non_terminals(&self) -> Vec<&Self::NonTerm> {
    self.0.non_terminals.iter().collect()
  }

  fn rules(&self) -> Vec<&Self::Rule> {
    self.0.rules.iter().collect()
  }

  fn get_prod(&self, prod_id: &Self::ProdId) -> Option<&Self::Prod> {
    self.0.prods.get(prod_id)
  }
}

impl<T, NT, ProdID, AV> Rule for RuleHandle<T, NT, ProdID, AV>
where
  T: Clone,
  NT: Ord + Clone,
  ProdID: Ord,
{
  type Term = T;

  type Key = NT;

  type NonTerm = NonTermHandle<T, NT, ProdID, AV>;

  type Prod = ProdHandle<T, NT, ProdID, AV>;

  fn key(&self) -> &Self::Key {
    &self.0.head
  }

  fn head(&self) -> Self::NonTerm {
    let grammar = self.0.grammar.upgrade().expect("grammar is alive");
    grammar
      .non_terminals
      .get(&self.0.head)
      .expect("non-term key exists in grammar")
      .clone()
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
          .get(&prod_id)
          .expect("prod key exists in grammar")
          .clone()
      })
      .collect()
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

  type Rule = RuleHandle<T, NT, ProdID, AV>;

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
  type Rule = RuleHandle<T, NT, ProdID, AV>;

  fn key(&self) -> &Self::Key {
    &self.0.key
  }

  fn rule(&self) -> Self::Rule {
    let grammar = self.0.grammar.upgrade().expect("grammar is alive");
    grammar
      .rules
      .get(&self.0.key)
      .expect("rule key exists in grammar")
      .clone()
  }
}

#[cfg(test)]
mod test {
  use super::builder::GrammarBuilder;
  use super::Grammar;
  use super::GrammarHandle;
  use super::NonTerm;
  use super::Rule;

  #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
  enum Term {
    Plus,
    Minus,
    Times,
    Div,
    Num,
    LParen,
    RParen,
  }

  #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
  enum NTerm {
    Expr,
  }

  #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
  enum Prod {
    AddExpr,
    SubExpr,
    MulExpr,
    DivExpr,
    ParenExpr,
    NumExpr,
  }

  /// Create simple arithmetic expression grammar.
  fn create_arithmetic_grammar() -> GrammarHandle<Term, NTerm, Prod, ()> {
    let mut builder = GrammarBuilder::new(NTerm::Expr);

    {
      let mut add_binop = |prod_id: Prod, op: Term| {
        builder.add_prod(prod_id, NTerm::Expr, (), move |mut e| {
          e.named_non_term(NTerm::Expr, "left")
            .term(op)
            .named_non_term(NTerm::Expr, "right");
        });
      };

      add_binop(Prod::AddExpr, Term::Plus);
      add_binop(Prod::SubExpr, Term::Minus);
      add_binop(Prod::MulExpr, Term::Times);
      add_binop(Prod::DivExpr, Term::Minus);
    }

    builder
      .add_prod(Prod::ParenExpr, NTerm::Expr, (), move |mut e| {
        e.term(Term::LParen)
          .named_non_term(NTerm::Expr, "e")
          .term(Term::RParen);
      })
      .add_prod(Prod::NumExpr, NTerm::Expr, (), move |mut e| {
        e.named_term(Term::Num, "n");
      });

    builder.build()
  }

  #[test]
  fn grammar_builds() {
    let grammar = create_arithmetic_grammar();
    println!("{:#?}", grammar);
    assert_eq!(grammar.non_terminals().len(), 1);
    assert_eq!(grammar.rules().len(), 1);
    assert_eq!(grammar.start_non_term().rule().prods().len(), 6);
  }
}
