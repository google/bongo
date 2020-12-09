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

pub trait Pass<E>
where
  E: ElemTypes,
{
  type Value: Any + 'static;

  fn run_pass<'a>(pass_map: &PassMap<'a, E>) -> Self::Value;
}

pub struct PassMap<'a, E>
where
  E: ElemTypes,
{
  grammar: &'a Grammar<E>,
  passes: RefCell<BTreeMap<TypeId, Rc<dyn Any + 'static>>>,
}

impl<'a, E> PassMap<'a, E>
where
  E: ElemTypes,
{
  pub fn new(grammar: &'a Grammar<E>) -> Self {
    PassMap {
      grammar,
      passes: RefCell::new(BTreeMap::new()),
    }
  }

  pub fn grammar(&self) -> &'a Grammar<E> {
    &self.grammar
  }

  pub fn get_pass<P>(&self) -> PassValue<E, P>
  where
    P: Pass<E> + 'static,
  {
    let pass_type = TypeId::of::<P>();

    let contains_key = {
      let guard = self.passes.borrow();
      guard.contains_key(&pass_type)
    };

    if !contains_key {
      let value = P::run_pass(self);
      let mut guard = self.passes.borrow_mut();
      guard.insert(pass_type, Rc::new(value));
    };

    let any_pass_val = {
      let guard = self.passes.borrow();
      guard.get(&pass_type).unwrap().clone()
    };

    PassValue {
      value: any_pass_val,
      _phantom: std::marker::PhantomData {},
    }
  }
}

pub struct PassValue<'a, E, P>
where
  E: ElemTypes,
  P: Pass<E>,
{
  value: Rc<dyn Any + 'static>,
  _phantom: std::marker::PhantomData<&'a P::Value>,
}

impl<'a, E, P> std::ops::Deref for PassValue<'a, E, P>
where
  E: ElemTypes,
  P: Pass<E>,
{
  type Target = P::Value;

  fn deref(&self) -> &Self::Target {
    self
      .value
      .as_ref()
      .downcast_ref::<P::Value>()
      .expect("Value must have correct downcast type")
  }
}
