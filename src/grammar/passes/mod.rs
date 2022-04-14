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

use super::Grammar;

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

enum PassError<T, NT, AK, AV, P>
where
  P: Pass<T, NT, AK, AV>,
{
  ThisPass(P::Error),
  PrevPass(Box<dyn BasePassError>),
  RecursionDetected,
}

/// A unique placeholder type to represent the value of a pass that hasn't completed.
///
/// This helps us avoid accidental infinite recursion in the case where a pass depends on itself (directly or indirectly).
struct NoCurrentValue;

/// A
pub trait Pass<T, NT, AK, AV>: Any + Sized + 'static {
  type Error: std::error::Error + 'static;

  fn run_pass(
    pass_map: &PassContext<T, NT, AK, AV>,
  ) -> Result<Self, Self::Error>;
}

/// A map from passes to their associated results.
pub struct PassContext<'a, T, NT, AK, AV> {
  grammar: &'a Grammar<T, NT, AK, AV>,
  passes: RefCell<BTreeMap<TypeId, Rc<dyn Any + 'static>>>,
}

impl<'a, T, NT, AK, AV> PassContext<'a, T, NT, AK, AV> {
  // Create a new pass map, where passes derive from the given grammar and other passes.
  pub fn new(grammar: &'a Grammar<T, NT, AK, AV>) -> Self {
    PassContext {
      grammar,
      passes: RefCell::new(BTreeMap::new()),
    }
  }

  /// Returns the underlying grammar.
  pub fn grammar(&self) -> &'a Grammar<T, NT, AK, AV> {
    self.grammar
  }

  /// Returns the result of the given pass. Computes it if it hasn't been computed yet. Passes can
  /// depend on other passes.
  pub fn get_pass<P>(&self) -> Result<Rc<P>, P::Error>
  where
    P: Pass<T, NT, AK, AV> + 'static,
  {
    let pass_type = TypeId::of::<P>();

    let contains_key = {
      let guard = self.passes.borrow();
      match guard.get(&pass_type) {
        // Check if we already have the result.
        //
        Some(pass) => {
          if pass.downcast_ref::<NoCurrentValue>().is_some() {
            panic!("Detected recursive loop in pass dependencies.")
          } else {
            true
          }
        }
        None => false,
      }
    };

    if !contains_key {
      {
        // Insert a NoCurrentRef as the current value to mark it as in process. This
        // helps us avoid infinite recursion.
        let mut guard = self.passes.borrow_mut();
        guard.insert(pass_type, Rc::new(NoCurrentValue));
      }
      let value = P::run_pass(self)?;
      let mut guard = self.passes.borrow_mut();
      guard.insert(pass_type, Rc::new(value));
    };

    let any_pass_ref = {
      let guard = self.passes.borrow();
      guard
        .get(&pass_type)
        .expect("existence already checked")
        .clone()
    };

    Ok(any_pass_ref.downcast::<P>().expect("type already verified"))
  }
}
