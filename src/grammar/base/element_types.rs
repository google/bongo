use crate::utils::{Name, OrdKey, ToDoc};

/// A trait which carries the underlying types for a grammar.
///
/// This allows us to specify a family of types at once as a type parameter
/// instead of forcing us to provide a number of type variables with a long list
/// of bounds.
///
/// This type is not instantiated, and will typically be a zero-sized type.
pub trait ElemTypes: 'static {
  /// The type used to identify each possible terminal.
  ///
  /// Terminals must be cloneable, and must be Ord to be used as a key in a map.
  type Term: OrdKey + ToDoc;

  /// The type used to identify each possible non-terminal.
  type NonTerm: OrdKey + ToDoc;

  /// The type used to identify each production. A grammar requires that the
  /// pair of the head nonterminal of a production and its action key are
  /// unique per production.
  type ActionKey: OrdKey + ToDoc;

  /// A value that annotates each production.
  /// 
  /// This is useful for passing information between multiple derived grammars, such
  /// as passing nullability information about a previous layer's action key.
  type ActionValue: Clone + std::fmt::Debug + ToDoc + 'static;
}

/// A terminal element.
///
/// This is a simple terminal type compatible with `ElementTypes`.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Terminal(Name);

impl Terminal {
  pub fn new(s: &str) -> Self {
    Terminal(Name::new(s))
  }
}

impl ToDoc for Terminal {
  fn to_doc<'a, DA: pretty::DocAllocator<'a>>(
    &self,
    da: &'a DA,
  ) -> pretty::DocBuilder<'a, DA> {
    da.text(self.0.str().to_string())
  }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct NonTerminal(Name);

impl NonTerminal {
  pub fn new(s: &str) -> Self {
    NonTerminal(Name::new(s))
  }
}

impl ToDoc for NonTerminal {
  fn to_doc<'a, DA: pretty::DocAllocator<'a>>(
    &self,
    da: &'a DA,
  ) -> pretty::DocBuilder<'a, DA> {
    da.text(self.0.str().to_string())
  }
}

pub struct BaseElementTypes;

impl ElemTypes for BaseElementTypes {
  type Term = Terminal;
  type NonTerm = NonTerminal;
  type ActionKey = Name;
  type ActionValue = ();
}
