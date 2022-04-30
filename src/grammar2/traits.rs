use super::Elem;

pub trait NamedElem {
  type Term;
  type NonTerm;

  fn name(&self) -> Option<&str>;
  fn elem(&self) -> Elem<Self::Term, Self::NonTerm>;
}

pub trait NonTerm {
  type Key;
  type Rule: Rule<NonTerm = Self, Key = Self::Key>;

  /// Gets the key of the non-terminal.
  fn key(&self) -> &Self::Key;

  /// Gets the rule in the grammar that has this non-terminal as a head.
  fn rule(&self) -> Self::Rule;
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

pub trait Rule {
  type Term;
  type Key;
  type NonTerm: NonTerm<Key = Self::Key, Rule = Self>;
  type Prod: Prod<Term = Self::Term, NonTerm = Self::NonTerm, Rule = Self>;

  fn key(&self) -> &Self::Key;
  fn head(&self) -> Self::NonTerm;
  fn prods(&self) -> Vec<Self::Prod>;
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
