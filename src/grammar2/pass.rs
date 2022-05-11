//! Defines the pass types over grammar.
//!
//! Traditionally, the information about a grammar is built up over several
//! passes, each of which attach data to the entities in the grammar. The
//! [PassSet] type encapsulates this information, taking a reference to a
//! grammar, and having [Pass]es build themselves up within the set.
//!
//! The [Pass] trait is the base trait for all passes. It can then implement
//! one or more of [TermPass], [NonTermPass], or [ProdPass] to give the type
//! of data it stores for each of their respective entities. The [Pass] trait
//! itself provides the [Pass::execute] method, which is used to generate the
//! contents of the pass. Once a pass is executed, the data it contains is
//! immutable.
//!
//! Passes can be dependent on each other. Dependency cycles are not allowed,
//! and will cause a runtime panic.

use std::{cell::RefCell, collections::BTreeMap, marker::PhantomData, rc::Rc};

use crate::utils::type_map::{TypeMap, TypeMapKey};

use super::{
  traits::{NonTermKey, ProdKey, TermKey},
  Grammar, NonTerm, Prod,
};

pub trait Pass<G>: Sized + Ord + Clone + 'static
where
  G: Grammar,
{
  fn execute(&self, pass_context: &mut PassContext<Self, G>);
}

pub trait TermPass<G>: Pass<G>
where
  G: Grammar,
{
  type TermValue;
}

pub trait NonTermPass<G>: Pass<G>
where
  G: Grammar,
{
  type NonTermValue;
}

pub trait ProdPass<G>: Pass<G>
where
  G: Grammar,
{
  type ProdValue;
}

type PassTermValue<P, G> = <P as TermPass<G>>::TermValue;
type PassNonTermValue<P, G> = <P as NonTermPass<G>>::NonTermValue;
type PassProdValue<P, G> = <P as ProdPass<G>>::ProdValue;

macro_rules! define_entity_key {
  ($name:ident, $key_type:ident, $pass_subtype:ident, $pass_value_type:ident) => {
    #[derive(Clone, Debug)]
    struct $name<P, G>(PhantomData<(P, G)>);
    impl<P, G> PartialEq for $name<P, G>
    where
      G: Grammar,
      P: Pass<G>,
    {
      fn eq(&self, _other: &$name<P, G>) -> bool {
        true
      }
    }

    impl<P, G> PartialOrd for $name<P, G>
    where
      G: Grammar,
      P: Pass<G>,
    {
      fn partial_cmp(
        &self,
        _other: &$name<P, G>,
      ) -> Option<std::cmp::Ordering> {
        Some(std::cmp::Ordering::Equal)
      }
    }

    impl<P, G> Eq for $name<P, G>
    where
      G: Grammar,
      P: Pass<G>,
    {
    }

    impl<P, G> Ord for $name<P, G>
    where
      G: Grammar,
      P: Pass<G>,
    {
      fn cmp(&self, _other: &$name<P, G>) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
      }
    }

    impl<P, G> TypeMapKey for $name<P, G>
    where
      P: $pass_subtype<G>,
      G: Grammar + 'static,
    {
      type ValueType = BTreeMap<$key_type<G>, Rc<$pass_value_type<P, G>>>;
    }

    impl<P, G> Default for $name<P, G>
    where
      P: $pass_subtype<G>,
      G: Grammar + 'static,
    {
      fn default() -> Self {
        Self(PhantomData)
      }
    }
  };
}

define_entity_key!(TermTypeKey, TermKey, TermPass, PassTermValue);
define_entity_key!(NonTermTypeKey, NonTermKey, NonTermPass, PassNonTermValue);
define_entity_key!(ProdTypeKey, ProdKey, ProdPass, PassProdValue);

struct SinglePassStorage<P, G> {
  entity_map: TypeMap,
  _phantom: PhantomData<(P, G)>,
}

impl<P, G> SinglePassStorage<P, G>
where
  P: Pass<G>,
  G: Grammar + 'static,
{
  fn new() -> Self {
    SinglePassStorage {
      entity_map: TypeMap::new(),
      _phantom: PhantomData,
    }
  }

  fn get_term_value(&self, term: &G::Term) -> Option<Rc<P::TermValue>>
  where
    P: TermPass<G>,
  {
    let key: TermTypeKey<P, G> = TermTypeKey(PhantomData);
    self.entity_map.get(&key).and_then(|m| m.get(term).cloned())
  }

  fn get_non_term_value(
    &self,
    term: &<G::NonTerm as NonTerm>::Key,
  ) -> Option<Rc<P::NonTermValue>>
  where
    P: NonTermPass<G>,
  {
    let key: NonTermTypeKey<P, G> = NonTermTypeKey(PhantomData);
    self.entity_map.get(&key).and_then(|m| m.get(term).cloned())
  }

  fn get_prod_value(
    &self,
    prod: &<G::Prod as Prod>::Key,
  ) -> Option<Rc<P::ProdValue>>
  where
    P: ProdPass<G>,
  {
    let key: ProdTypeKey<P, G> = ProdTypeKey(PhantomData);
    self.entity_map.get(&key).and_then(|m| m.get(prod).cloned())
  }

  fn get_term_values(&self) -> BTreeMap<TermKey<G>, Rc<P::TermValue>>
  where
    P: TermPass<G>,
  {
    let key: TermTypeKey<P, G> = Default::default();
    self.entity_map.get(&key).cloned().unwrap_or_default()
  }

  fn get_non_term_values(&self) -> BTreeMap<NonTermKey<G>, Rc<P::NonTermValue>>
  where
    P: NonTermPass<G>,
  {
    let key: NonTermTypeKey<P, G> = Default::default();
    self.entity_map.get(&key).cloned().unwrap_or_default()
  }

  fn get_prod_values(&self) -> BTreeMap<ProdKey<G>, Rc<P::ProdValue>>
  where
    P: ProdPass<G>,
  {
    let key: ProdTypeKey<P, G> = Default::default();
    self.entity_map.get(&key).cloned().unwrap_or_default()
  }

  fn insert_term_value(&mut self, term: &G::Term, value: P::TermValue)
  where
    P: TermPass<G>,
  {
    let key: TermTypeKey<P, G> = TermTypeKey(PhantomData);
    self
      .entity_map
      .get_mut_or_else(key, || BTreeMap::new())
      .insert(term.clone(), Rc::new(value));
  }

  fn insert_non_term_value(
    &mut self,
    term: &<G::NonTerm as NonTerm>::Key,
    value: P::NonTermValue,
  ) where
    P: NonTermPass<G>,
  {
    let key: NonTermTypeKey<P, G> = NonTermTypeKey(PhantomData);
    self
      .entity_map
      .get_mut_or_else(key, || BTreeMap::new())
      .insert(term.clone(), Rc::new(value));
  }

  fn insert_prod_value(
    &mut self,
    prod: &<G::Prod as Prod>::Key,
    value: P::ProdValue,
  ) where
    P: ProdPass<G>,
  {
    let key: ProdTypeKey<P, G> = ProdTypeKey(PhantomData);
    self
      .entity_map
      .get_mut_or_else(key, || BTreeMap::new())
      .insert(prod.clone(), Rc::new(value));
  }
}

/// The context available during the [Pass::execute] method. It allows access
/// to the grammar,
pub struct PassContext<'a, P, G> {
  pass: &'a P,
  pass_set: &'a PassSet<G>,
  storage: &'a mut SinglePassStorage<P, G>,
}

impl<'a, P, G> PassContext<'a, P, G>
where
  P: Pass<G>,
  G: Grammar + 'static,
{
  /// Returns the grammar used by the pass.
  pub fn grammar(&self) -> &'a G {
    self.pass_set.grammar()
  }

  pub fn pass(&self) -> &'a P {
    self.pass
  }

  pub fn pass_set(&self) -> &'a PassSet<G> {
    self.pass_set
  }

  pub fn insert_term_value(&mut self, term: &TermKey<G>, value: P::TermValue)
  where
    P: TermPass<G>,
  {
    self.storage.insert_term_value(term, value);
  }

  pub fn insert_non_term_value(
    &mut self,
    term: &<G::NonTerm as NonTerm>::Key,
    value: P::NonTermValue,
  ) where
    P: NonTermPass<G>,
  {
    self.storage.insert_non_term_value(term, value);
  }

  pub fn insert_prod_value(
    &mut self,
    prod: &<G::Prod as Prod>::Key,
    value: P::ProdValue,
  ) where
    P: ProdPass<G>,
  {
    self.storage.insert_prod_value(prod, value);
  }
}

#[derive(Clone)]
struct PassTypeKey<P, G> {
  pass: P,
  _grammar: std::marker::PhantomData<G>,
}

impl<P, G> PartialEq for PassTypeKey<P, G>
where
  P: Ord,
{
  fn eq(&self, other: &Self) -> bool {
    self.pass == other.pass
  }
}

impl<P, G> Eq for PassTypeKey<P, G> where P: Ord {}

impl<P, G> PartialOrd for PassTypeKey<P, G>
where
  P: Ord,
{
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl<P, G> Ord for PassTypeKey<P, G>
where
  P: Ord,
{
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.pass.cmp(&other.pass)
  }
}

impl<P, G> TypeMapKey for PassTypeKey<P, G>
where
  P: Pass<G>,
  G: Grammar + 'static,
{
  type ValueType = Option<Rc<SinglePassStorage<P, G>>>;
}

/// Contains a set of passes over a grammar.
pub struct PassSet<G> {
  grammar: G,
  pass_map: RefCell<TypeMap>,
}

impl<G> PassSet<G>
where
  G: Grammar + 'static,
{
  fn get_single_pass_storage<P>(&self, pass: &P) -> Rc<SinglePassStorage<P, G>>
  where
    P: Pass<G>,
  {
    let pass_key: PassTypeKey<P, G> = PassTypeKey {
      pass: pass.clone(),
      _grammar: PhantomData,
    };
    let guard = self.pass_map.borrow();
    if let Some(pass) = guard.get(&pass_key) {
      return pass.as_ref().expect("not in pass recurstion").clone();
    }

    {
      let mut guard_mut = self.pass_map.borrow_mut();
      guard_mut.insert(pass_key.clone(), None);
    }

    let mut new_single_pass_storage: SinglePassStorage<P, G> =
      SinglePassStorage::new();
    let mut pass_context = PassContext {
      pass,
      pass_set: self,
      storage: &mut new_single_pass_storage,
    };
    pass.execute(&mut pass_context);
    {
      let single_pass_rc = Rc::new(new_single_pass_storage);
      let mut guard_mut = self.pass_map.borrow_mut();
      guard_mut.insert(pass_key, Some(Rc::clone(&single_pass_rc)));
      single_pass_rc
    }
  }
  pub fn grammar(&self) -> &G {
    &self.grammar
  }

  pub fn get_term_value<P>(
    &self,
    pass: &P,
    term: &TermKey<G>,
  ) -> Option<Rc<P::TermValue>>
  where
    P: TermPass<G> + 'static,
  {
    self.get_single_pass_storage(pass).get_term_value(term)
  }

  pub fn get_non_term_value<P>(
    &self,
    pass: &P,
    non_term: &NonTermKey<G>,
  ) -> Option<Rc<P::NonTermValue>>
  where
    P: NonTermPass<G> + 'static,
  {
    self
      .get_single_pass_storage(pass)
      .get_non_term_value(non_term)
  }

  pub fn get_prod_value<P>(
    &self,
    pass: &P,
    prod: &ProdKey<G>,
  ) -> Option<Rc<P::ProdValue>>
  where
    P: ProdPass<G> + 'static,
  {
    self.get_single_pass_storage(pass).get_prod_value(prod)
  }

  pub fn get_term_values<P>(
    &self,
    pass: &P,
  ) -> BTreeMap<TermKey<G>, Rc<P::TermValue>>
  where
    P: TermPass<G> + 'static,
  {
    self.get_single_pass_storage(pass).get_term_values()
  }

  pub fn get_non_term_values<P>(
    &self,
    pass: &P,
  ) -> BTreeMap<NonTermKey<G>, Rc<P::NonTermValue>>
  where
    P: NonTermPass<G> + 'static,
  {
    self.get_single_pass_storage(pass).get_non_term_values()
  }

  pub fn get_prod_values<P>(
    &self,
    pass: &P,
  ) -> BTreeMap<ProdKey<G>, Rc<P::ProdValue>>
  where
    P: ProdPass<G> + 'static,
  {
    self.get_single_pass_storage(pass).get_prod_values()
  }
}
