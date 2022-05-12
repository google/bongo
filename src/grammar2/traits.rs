use super::Elem;

pub trait NamedElem {
  type Term;
  type NonTerm;

  fn name(&self) -> Option<&str>;
  fn elem(&self) -> Elem<Self::Term, Self::NonTerm>;
}

pub trait Term: Clone {
  type Key: Ord + Clone + 'static;

  fn key(&self) -> &Self::Key;
}

pub trait NonTerm: Clone {
  type Key: Ord + Clone + 'static;
  type Term: Term;
  type Prod: Prod<NonTerm = Self, Term = Self::Term>;

  /// Gets the key of the non-terminal.
  fn key(&self) -> &Self::Key;

  /// Gets the rule in the grammar that has this non-terminal as a head.
  fn prods(&self) -> Vec<Self::Prod>;

  fn is_nullable(&self) -> bool;

  fn firsts(&self) -> Vec<Self::Term>;

  fn follows(&self) -> Vec<Self::Term>;
}

pub trait Prod: Clone {
  type Key: Ord + Clone + 'static;
  type ActionValue;
  type Term: Term;
  type NonTerm: NonTerm<Prod = Self>;
  type NamedElem: NamedElem<Term = Self::Term, NonTerm = Self::NonTerm>;

  fn key(&self) -> &Self::Key;
  fn head(&self) -> Self::NonTerm;
  fn action_value(&self) -> &Self::ActionValue;
  fn prod_elements(&self) -> Vec<Self::NamedElem>;
}

/// A trait of a context-free grammar.
pub trait Grammar: Clone {
  /// The type of terminal symbols.
  type Term: Term;

  /// A handle to a non terminal.
  type NonTerm: NonTerm<Prod = Self::Prod, Term = Self::Term>;

  /// A handle to a production.
  type Prod: Prod<Term = Self::Term, NonTerm = Self::NonTerm>;

  /// Returns the start non-terminal of the grammar.
  fn start_non_term(&self) -> Self::NonTerm;
  /// Returns a list of all terminals in the grammar.
  fn terminals(&self) -> Vec<Self::Term>;
  /// Returns a list of all non-terminals in the grammar.
  fn non_terminals(&self) -> Vec<Self::NonTerm>;
  /// Returns a list of all productions in the grammar.
  fn prods(&self) -> Vec<Self::Prod>;

  /// Returns the prod with the given id.
  fn get_prod(&self, prod_id: &ProdKey<Self>) -> Option<Self::Prod>;

  /// Returns the non-term with the given key.
  fn get_non_term(
    &self,
    key: &<Self::NonTerm as NonTerm>::Key,
  ) -> Option<Self::NonTerm>;
}

pub type TermKey<G> = <<G as Grammar>::Term as Term>::Key;
pub type NonTermKey<G> = <<G as Grammar>::NonTerm as NonTerm>::Key;
pub type ProdKey<G> = <<G as Grammar>::Prod as Prod>::Key;
