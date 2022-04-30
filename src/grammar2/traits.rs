use super::Elem;

pub trait NamedElem {
  type Term;
  type NonTerm;

  fn name(&self) -> Option<&str>;
  fn elem(&self) -> Elem<Self::Term, Self::NonTerm>;
}

pub trait NonTerm {
  type Term;
  type Key;
  type Prod: Prod<Term = Self::Term, NonTerm = Self>;

  /// Gets the key of the non-terminal.
  fn key(&self) -> &Self::Key;

  /// Gets the rule in the grammar that has this non-terminal as a head.
  fn prods(&self) -> Vec<Self::Prod>;
}

pub trait Prod {
  type Term;
  type ProdId;
  type ActionValue;
  type NonTerm: NonTerm<Term = Self::Term, Prod = Self>;
  type NamedElem: NamedElem<Term = Self::Term, NonTerm = Self::NonTerm>;

  fn head(&self) -> Self::NonTerm;
  fn action_id(&self) -> &Self::ProdId;
  fn action_value(&self) -> &Self::ActionValue;
  fn prod_elements(&self) -> Vec<Self::NamedElem>;
}

/// A trait of a context-free grammar.
pub trait Grammar {
  /// The type of terminal symbols.
  type Term;

  /// A handle to a non terminal.
  type NonTerm: NonTerm<Term = Self::Term, Prod = Self::Prod>;

  /// An identifier of a production.
  type ProdId;

  /// A handle to a production.
  type Prod: Prod<
    Term = Self::Term,
    NonTerm = Self::NonTerm,
    ProdId = Self::ProdId,
  >;

  /// Returns the start non-terminal of the grammar.
  fn start_non_term(&self) -> &Self::NonTerm;
  /// Returns a list of all terminals in the grammar.
  fn terminals(&self) -> Vec<&Self::Term>;
  /// Returns a list of all non-terminals in the grammar.
  fn non_terminals(&self) -> Vec<&Self::NonTerm>;
  /// Returns a list of all productions in the grammar.
  fn prods(&self) -> Vec<&Self::Prod>;

  /// Returns the prod with the given id.
  fn get_prod(&self, prod_id: &Self::ProdId) -> Option<&Self::Prod>;
  /// Returns the non-term with the given key.
  fn get_non_term(&self, key: &<Self::NonTerm as NonTerm>::Key) -> Option<&Self::NonTerm>;
}
