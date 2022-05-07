use std::{cell::RefCell, collections::BTreeMap, marker::PhantomData, rc::Rc};

use crate::utils::type_map::{TypeMap, TypeMapKey};

use super::{Grammar, NonTerm, Prod};

pub trait Pass: Sized + Ord + Clone + 'static {
  fn execute<G: Grammar>(&self, pass_context: &mut PassContext<Self, G>);
}

pub trait TermPass: Pass {
  type TermValue;
}

pub trait NonTermPass: Pass {
  type NonTermValue;
}

pub trait ProdPass: Pass {
  type ProdValue;
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct TermTypeKey<P, K>(PhantomData<(P, K)>);
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct NonTermTypeKey<P, K>(PhantomData<(P, K)>);
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct ProdTypeKey<P, K>(PhantomData<(P, K)>);

impl<P, K> TypeMapKey for TermTypeKey<P, K>
where
  P: TermPass,
  K: Ord + Clone + 'static,
{
  type ValueType = BTreeMap<K, Rc<P::TermValue>>;
}

impl<P, K> TypeMapKey for NonTermTypeKey<P, K>
where
  P: NonTermPass,
  K: Ord + Clone + 'static,
{
  type ValueType = BTreeMap<K, Rc<P::NonTermValue>>;
}

impl<P, K> TypeMapKey for ProdTypeKey<P, K>
where
  P: ProdPass,
  K: Ord + Clone + 'static,
{
  type ValueType = BTreeMap<K, Rc<P::ProdValue>>;
}

struct SinglePassStorage<P, G> {
  entity_map: TypeMap,
  _phantom: PhantomData<(P, G)>,
}

impl<P, G> SinglePassStorage<P, G>
where
  P: Pass,
  G: Grammar,
{
  fn new() -> Self {
    SinglePassStorage {
      entity_map: TypeMap::new(),
      _phantom: PhantomData,
    }
  }

  fn get_term_value(&self, term: &G::Term) -> Option<Rc<P::TermValue>>
  where
    P: TermPass,
  {
    let key: TermTypeKey<P, G::Term> = TermTypeKey(PhantomData);
    self.entity_map.get(&key).and_then(|m| m.get(term).cloned())
  }

  fn get_non_term_value(
    &self,
    term: &<G::NonTerm as NonTerm>::Key,
  ) -> Option<Rc<P::NonTermValue>>
  where
    P: NonTermPass,
  {
    let key: NonTermTypeKey<P, <G::NonTerm as NonTerm>::Key> =
      NonTermTypeKey(PhantomData);
    self.entity_map.get(&key).and_then(|m| m.get(term).cloned())
  }

  fn get_prod_value(
    &self,
    prod: &<G::Prod as Prod>::Key,
  ) -> Option<Rc<P::ProdValue>>
  where
    P: ProdPass,
  {
    let key: ProdTypeKey<P, <G::Prod as Prod>::Key> = ProdTypeKey(PhantomData);
    self.entity_map.get(&key).and_then(|m| m.get(prod).cloned())
  }

  fn insert_term_value(&mut self, term: &G::Term, value: P::TermValue)
  where
    P: TermPass,
  {
    let key: TermTypeKey<P, G::Term> = TermTypeKey(PhantomData);
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
    P: NonTermPass,
  {
    let key: NonTermTypeKey<P, <G::NonTerm as NonTerm>::Key> =
      NonTermTypeKey(PhantomData);
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
    P: ProdPass,
  {
    let key: ProdTypeKey<P, <G::Prod as Prod>::Key> = ProdTypeKey(PhantomData);
    self
      .entity_map
      .get_mut_or_else(key, || BTreeMap::new())
      .insert(prod.clone(), Rc::new(value));
  }
}

pub struct PassContext<'a, P, G>
where
  P: Pass,
  G: Grammar,
{
  grammar: &'a G,
  pass: &'a P,
  pass_set: &'a PassSet<'a, G>,
  storage: &'a mut SinglePassStorage<P, G>,
}

#[derive(Clone)]
struct PassTypeKey<P, G> {
  pass: P,
  _grammar: std::marker::PhantomData<G>,
}

impl<P, G> PartialEq for PassTypeKey<P, G>
where
  P: Pass,
{
  fn eq(&self, other: &Self) -> bool {
    self.pass == other.pass
  }
}

impl<P, G> Eq for PassTypeKey<P, G> where P: Pass {}

impl<P, G> PartialOrd for PassTypeKey<P, G>
where
  P: Pass,
{
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl<P, G> Ord for PassTypeKey<P, G>
where
  P: Pass,
{
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.pass.cmp(&other.pass)
  }
}

impl<P, G> TypeMapKey for PassTypeKey<P, G>
where
  P: Pass,
  G: Grammar + 'static,
{
  type ValueType = Option<Rc<SinglePassStorage<P, G>>>;
}

pub struct PassSet<'a, G> {
  grammar: &'a G,
  pass_map: RefCell<TypeMap>,
}

impl<'a, G> PassSet<'a, G>
where
  G: Grammar + 'static,
{
  fn get_single_pass_storage<P>(&self, pass: &P) -> Rc<SinglePassStorage<P, G>>
  where
    P: Pass,
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
      grammar: self.grammar,
      pass: pass,
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
  pub fn get_term_value<P>(
    &self,
    pass: &'a P,
    term: &'a G::Term,
  ) -> Option<Rc<P::TermValue>>
  where
    P: TermPass + 'static,
  {
    self.get_single_pass_storage(pass).get_term_value(term)
  }

  pub fn get_non_term_value<P>(
    &self,
    pass: &'a P,
    non_term: &'a <G::NonTerm as NonTerm>::Key,
  ) -> Option<Rc<P::NonTermValue>>
  where
    P: NonTermPass + 'static,
  {
    self
      .get_single_pass_storage(pass)
      .get_non_term_value(non_term)
  }

  pub fn get_prod_value<P>(
    &self,
    pass: &'a P,
    prod: &'a <G::Prod as Prod>::Key,
  ) -> Option<Rc<P::ProdValue>>
  where
    P: ProdPass + 'static,
  {
    self.get_single_pass_storage(pass).get_prod_value(prod)
  }
}
