use crate::utils::{Name, ToDoc};

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
