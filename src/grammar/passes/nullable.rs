mod nullables;

use crate::grammar::Prod;

use super::Pass;

pub struct Nullable<NT, AK>(nullables::GrammarNullableInfo<NT, AK>);

impl<NT, AK> Nullable<NT, AK>
where
  NT: Ord + Clone,
{
  pub fn is_nullable(&self, nt: &NT) -> bool {
    self.0.is_nullable(nt)
  }

  pub fn get_nullable_set(&self) -> std::collections::BTreeSet<NT> {
    self.0.get_nullable_set()
  }

  pub fn is_prod_nullable<T, AV>(&self, prod: &Prod<T, NT, AK, AV>) -> bool {
    self.0.is_prod_nullable(prod)
  }

  pub fn get_nullable_info(&self) -> &nullables::GrammarNullableInfo<NT, AK> {
    &self.0
  }
}

pub use nullables::{GrammarNullableInfo, NullableError};

impl<T, NT, AK, AV> Pass<T, NT, AK, AV> for Nullable<NT, AK>
where
  T: Ord,
  NT: Ord + Clone + 'static,
  AK: Ord + Clone + 'static,
{
  type Error = nullables::NullableError;

  fn run_pass(
    pass_map: &super::PassContext<T, NT, AK, AV>,
  ) -> Result<Self, Self::Error> {
    nullables::calculate_nullables(pass_map.grammar()).map(Nullable)
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::grammar::NonTerminal;
  use crate::grammar::{examples, passes::PassContext};
  use crate::start_grammar::wrap_grammar_with_start;

  #[test]
  fn test_simple_grammar() {
    let g = wrap_grammar_with_start(examples::make_simple()).unwrap();
    let pass_map = PassContext::new(&g);
    let nullables = pass_map.get_pass::<Nullable<_, _>>().unwrap();
    assert!(nullables.get_nullable_set().is_empty());
  }

  #[test]
  fn test_simple_nullable_grammar() {
    let g = examples::make_simple_nullable();
    let pass_map = PassContext::new(&g);
    let nullables = pass_map.get_pass::<Nullable<_, _>>().unwrap();
    assert!(nullables.is_nullable(&NonTerminal::new("start")));
    assert!(nullables.is_nullable(&NonTerminal::new("a")));
    assert!(nullables.is_nullable(&NonTerminal::new("b")));
    assert!(nullables.is_nullable(&NonTerminal::new("c")));
  }

  #[test]
  fn test_paren_grammar() {
    let g = examples::make_paren();
    let pass_map = PassContext::new(&g);
    let nullables = pass_map.get_pass::<Nullable<_, _>>().unwrap();
    assert!(nullables.is_nullable(&NonTerminal::new("expr_list")));
  }
}
