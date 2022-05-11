mod impls;
mod key_extractors;
mod pass;
mod traits;

use std::fmt;

pub use impls::builder::{ElementBuilder, GrammarBuilder};
pub use impls::{GrammarHandle, NonTermHandle, ProdHandle};
pub use pass::{NonTermPass, Pass, PassContext, PassSet, ProdPass, TermPass};
pub use traits::{
  Grammar, NamedElem, NonTerm, NonTermKey, Prod, ProdKey, TermKey,
};

/// An element of a production. It is either a terminal or non-terminal.
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

#[cfg(test)]
mod test {
  use super::Grammar;
  use super::GrammarBuilder;
  use super::GrammarHandle;
  use super::NonTerm;

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
    assert_eq!(grammar.start_non_term().prods().len(), 6);
  }
}
