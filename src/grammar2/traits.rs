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

pub trait Grammar {
  type Term;
  type NonTerm: NonTerm<Term = Self::Term, Prod = Self::Prod>;
  type ProdId;
  type Prod: Prod<
    Term = Self::Term,
    NonTerm = Self::NonTerm,
    ProdId = Self::ProdId,
  >;

  fn start_non_term(&self) -> &Self::NonTerm;
  fn terminals(&self) -> Vec<&Self::Term>;
  fn non_terminals(&self) -> Vec<&Self::NonTerm>;
  fn get_prod(&self, prod_id: &Self::ProdId) -> Option<&Self::Prod>;
}
