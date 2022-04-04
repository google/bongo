//! A pass is a type of query over the grammar that may be depended on by other passes.
//! This allows us to build each different type of pass in isolation, and then combine them
//! with automatic dependency resolution.

pub mod firsts;
pub mod follows;
pub mod nullable;

use std::{
  any::{Any, TypeId},
  cell::RefCell,
  collections::BTreeMap,
  rc::Rc,
};

use super::{ElemTypes, Grammar};

pub trait BasePassError {
  fn as_any(&self) -> &(dyn std::any::Any + 'static);
  fn as_err(&self) -> &(dyn std::error::Error + 'static);
}

impl<T> BasePassError for T
where
  T: std::any::Any + std::error::Error + 'static,
{
  fn as_any(&self) -> &(dyn Any + 'static) {
    self
  }

  fn as_err(&self) -> &(dyn std::error::Error + 'static) {
    self
  }
}

struct BoxPassError(Box<dyn BasePassError>);

enum PassError<E, P>
where
  P: Pass<E>,
  E: ElemTypes,
{
  ThisPass(P::Error),
  PrevPass(Box<dyn BasePassError>),
}

pub trait Pass<E>
where
  E: ElemTypes,
{
  type Value: Any + 'static;
  type Error;

  fn run_pass(
    pass_map: &PassContext<E>,
  ) -> Result<Self::Value, Self::Error>;
}

/// A map from passes to their associated results.
pub struct PassContext<'a, E>
where
  E: ElemTypes,
{
  grammar: &'a Grammar<E>,
  passes: RefCell<BTreeMap<TypeId, Rc<dyn Any + 'static>>>,
}

impl<'a, E> PassContext<'a, E>
where
  E: ElemTypes,
{
  // Create a new pass map, where passes derive from the given grammar and other passes.
  pub fn new(grammar: &'a Grammar<E>) -> Self {
    PassContext {
      grammar,
      passes: RefCell::new(BTreeMap::new()),
    }
  }

  /// Returns the underlying grammar.
  pub fn grammar(&self) -> &'a Grammar<E> {
    self.grammar
  }

  /// Returns the result of the given pass. Computes it if it hasn't been computed yet. Passes can
  /// depend on other passes.
  pub fn get_pass<P>(&self) -> Result<Rc<P::Value>, P::Error>
  where
    P: Pass<E> + 'static,
  {
    let pass_type = TypeId::of::<P>();

    let contains_key = {
      let guard = self.passes.borrow();
      guard.contains_key(&pass_type)
    };

    if !contains_key {
      let value = P::run_pass(self)?;
      let mut guard = self.passes.borrow_mut();
      guard.insert(pass_type, Rc::new(value));
    };

    let any_pass_ref = {
      let guard = self.passes.borrow();
      guard.get(&pass_type).expect("existence already checked").clone()
    };

    Ok(any_pass_ref.downcast::<P::Value>().expect("type already verified"))
  }
}
