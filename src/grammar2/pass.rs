use std::collections::BTreeMap;

use crate::utils::type_map::{TypeMapKey, TypeMap};

use super::{Grammar, NonTerm, Prod};

pub trait Pass: Sized + Ord {
  type TermValue;
  type NonTermValue;
  type ProdValue;

  fn execute<G: Grammar>(&self, pass_context: &mut PassContext<Self, G>);
}

struct PassStorage<P, G>
where
  P: Pass,
  G: Grammar,
{
  pub term_values: BTreeMap<G::Term, P::TermValue>,
  pub non_term_values: BTreeMap<<G::NonTerm as NonTerm>::Key, P::NonTermValue>,
  pub prod_values: BTreeMap<<G::Prod as Prod>::Key, P::ProdValue>,
}

impl<P, G> PassStorage<P, G>
where
  P: Pass,
  G: Grammar,
{
  pub fn new() -> Self {
    PassStorage {
      term_values: BTreeMap::new(),
      non_term_values: BTreeMap::new(),
      prod_values: BTreeMap::new(),
    }
  }
}

pub struct PassContext<'a, 'b, P, G>
where
  P: Pass,
  G: Grammar,
  'b: 'a,
{
  grammar: &'a G,
  pass_set: &'a PassSet<'b, G>,
  storage: PassStorage<P, G>,
}

#[derive(Clone)]
struct PassTypeKey<P, G> {
  pass: P,
  _grammar: std::marker::PhantomData<G>,
}

impl<P, G> Ord for PassTypeKey<P, G>
where
  P: Pass,
  G: Grammar,
{
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.pass.cmp(&other.pass)
  }
}

impl<P, G> PartialOrd for PassTypeKey<P, G>
where
  P: Pass,
  G: Grammar,
{
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl<P, G> PartialEq for PassTypeKey<P, G>
where
  P: Pass,
  G: Grammar,
{
  fn eq(&self, other: &Self) -> bool {
    self.pass == other.pass
  }
}

impl<P, G> Eq for PassTypeKey<P, G>
where
  P: Pass,
  G: Grammar,
{
}

impl<P, G> TypeMapKey for PassTypeKey<P, G>
where
  P: Pass + 'static,
  G: Grammar + 'static,
{
  type ValueType = PassStorage<P, G>;
}

pub struct PassSet<'a, G> {
  grammar: &'a G,
  pass_map: TypeMap,
}

impl<'a, G> PassSet<'a, G>
where
  G: Grammar + 'static,
{
  pub fn new(grammar: &'a G) -> Self {
    PassSet {
      grammar,
      pass_map: TypeMap::new(),
    }
  }

  fn get_pass_storage<P>(&self, pass: P) -> &PassStorage<P, G>
  where
    P: Pass + 'static,
  {
    let pass_type_key = PassTypeKey {
      pass,
      _grammar: std::marker::PhantomData,
    };

    self.pass_map.get(&pass_type_key).unwrap()
  }

  pub fn get_term<P>(&self, pass: P, t: &G::Term) -> &P::TermValue
  where
    P: Pass + 'static,
  {
    self.get_pass_storage(pass).term_values.get(t).unwrap()
  }

  pub fn get_non_term<P>(&self, pass: P, nt: &G::NonTerm) -> &P::NonTermValue
  where
    P: Pass + 'static,
  {
    let pass_type_key: PassTypeKey<_, G> = PassTypeKey {
      pass,
      _grammar: std::marker::PhantomData,
    };

    let storage = self.pass_map.get(&pass_type_key).unwrap();
    storage.non_term_values.get(nt.key()).unwrap()
  }

  pub fn insert<P: Pass + 'static>(&mut self, pass: P, grammar: &G) {
    let pass_type_key: PassTypeKey<_, G> = PassTypeKey {
      pass,
      _grammar: std::marker::PhantomData,
    };

    todo!()
  }
}
