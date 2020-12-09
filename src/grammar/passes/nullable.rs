use std::collections::BTreeSet;

use crate::grammar::ElemTypes;
use crate::utils::{change_iter, change_loop, WasChanged};

use super::Pass;

pub struct Nullable;

impl<E> Pass<E> for Nullable
where
  E: ElemTypes,
{
  type Value = BTreeSet<E::NonTerm>;

  fn run_pass<'a>(pass_map: &super::PassMap<'a, E>) -> Self::Value {
    let gram = pass_map.grammar();

    let mut nullables = BTreeSet::new();

    change_loop(|| {
      change_iter(gram.prods(), |prod| {
        let all_elems_are_nullable = prod.elements().all(|e| {
          if let Some(nt) = e.as_nonterm() {
            nullables.contains(nt)
          } else {
            false
          }
        });

        if all_elems_are_nullable {
          WasChanged::from_changed(nullables.insert(prod.head().clone()))
        } else {
          WasChanged::Unchanged
        }
      })
    });

    nullables
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
    let nullables = pass_map.get_pass::<Nullable>();
    assert!(nullables.is_empty());
  }

  #[test]
  fn test_simple_nullable_grammar() {
    let g = examples::make_simple_nullable();
    let pass_map = PassMap::new(&g);
    let nullables = pass_map.get_pass::<Nullable>();
    assert!(nullables.contains(&NonTerminal::new("start")));
    assert!(nullables.contains(&NonTerminal::new("a")));
    assert!(nullables.contains(&NonTerminal::new("b")));
    assert!(nullables.contains(&NonTerminal::new("c")));
  }

  #[test]
  fn test_paren_grammar() {
    let g = examples::make_paren();
    let pass_map = PassMap::new(&g);
    let nullables = pass_map.get_pass::<Nullable>();
    assert!(nullables.contains(&NonTerminal::new("expr_list")));
  }
}
