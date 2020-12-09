mod nullables;

use crate::grammar::ElemTypes;

use super::Pass;

pub struct Nullable;

pub use nullables::{GrammarNullableInfo, NullableError};

impl<E> Pass<E> for Nullable
where
  E: ElemTypes,
{
  type Value = nullables::GrammarNullableInfo<E>;
  type Error = nullables::NullableError;

  fn run_pass<'a>(pass_map: &super::PassMap<'a, E>) -> Result<Self::Value, Self::Error> {
    nullables::calculate_nullables(pass_map.grammar())
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::grammar::NonTerminal;
  use crate::grammar::{examples, passes::PassMap};
  use crate::start_grammar::wrap_grammar_with_start;

  #[test]
  fn test_simple_grammar() {
    let g = wrap_grammar_with_start(examples::make_simple()).unwrap();
    let pass_map = PassMap::new(&g);
    let nullables = pass_map.get_pass::<Nullable>().unwrap();
    assert!(nullables.get_nullable_set().is_empty());
  }

  #[test]
  fn test_simple_nullable_grammar() {
    let g = examples::make_simple_nullable();
    let pass_map = PassMap::new(&g);
    let nullables = pass_map.get_pass::<Nullable>().unwrap();
    assert!(nullables.is_nullable(&NonTerminal::new("start")));
    assert!(nullables.is_nullable(&NonTerminal::new("a")));
    assert!(nullables.is_nullable(&NonTerminal::new("b")));
    assert!(nullables.is_nullable(&NonTerminal::new("c")));
  }

  #[test]
  fn test_paren_grammar() {
    let g = examples::make_paren();
    let pass_map = PassMap::new(&g);
    let nullables = pass_map.get_pass::<Nullable>().unwrap();
    assert!(nullables.is_nullable(&NonTerminal::new("expr_list")));
  }
}
