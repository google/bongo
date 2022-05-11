pub mod builder;

use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::rc::Rc;

use crate::utils::svec::{IdentityKeyExtractor, KeyExtractor, SVec};

use super::traits::{Grammar, NamedElem, NonTerm, Prod};
use super::Elem;

/// An index into the term list for a grammar.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct TermIndex(usize);

/// An index into the non-terminal list for a grammar.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct NonTermIndex(usize);

/// An index into the production list for a grammar.
#[derive(Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Debug)]
struct ProdIndex(usize);

pub struct NamedElemRef<T, NT, ProdID, AV> {
  grammar: GrammarHandle<T, NT, ProdID, AV>,
  name: Option<Rc<String>>,
  elem: Elem<TermIndex, NonTermIndex>,
}

impl<T, NT, ProdID, AV> NamedElem for NamedElemRef<T, NT, ProdID, AV>
where
  T: Ord + Clone,
  NT: Ord + Clone,
  ProdID: Ord + Clone,
{
  type Term = T;
  type NonTerm = NonTermHandle<T, NT, ProdID, AV>;

  fn name(&self) -> Option<&str> {
    self.name.as_ref().map(|s| s.as_str())
  }

  fn elem(&self) -> Elem<Self::Term, Self::NonTerm> {
    match self.elem {
      Elem::Term(t_i) => {
        Elem::Term(self.grammar.get_term_by_index(t_i).clone())
      }
      Elem::NonTerm(nt_i) => {
        Elem::NonTerm(self.grammar.get_non_term_handle_by_index(nt_i))
      }
    }
  }
}

/// The primary storage of grammar data.
struct GrammarImpl<T, NT, ProdID, AV> {
  terminals: SVec<T, IdentityKeyExtractor>,
  non_terminals: SVec<NonTermImpl<NT>, NonTermImplKeyExtractor>,
  prods: SVec<ProdImpl<ProdID, AV>, ProdImplKeyExtractor>,
  start_nt: NonTermIndex,
}

impl<T, NT, ProdID, AV> GrammarImpl<T, NT, ProdID, AV>
where
  T: Ord,
  NT: Ord,
  ProdID: Ord,
{
  fn new(
    start_nt: NonTermIndex,
    terminals: SVec<T, IdentityKeyExtractor>,
    non_terminals: SVec<NonTermImpl<NT>, NonTermImplKeyExtractor>,
    prods: SVec<ProdImpl<ProdID, AV>, ProdImplKeyExtractor>,
  ) -> Self {
    Self {
      start_nt,
      terminals,
      non_terminals,
      prods,
    }
  }
}

#[derive(Debug)]
struct ProdImpl<ProdID, AV> {
  head: NonTermIndex,
  key: ProdID,
  action_value: AV,
  prod_elems: Vec<NamedElemImpl<TermIndex, NonTermIndex>>,
}

impl<ProdID, AV> ProdImpl<ProdID, AV> {
  fn new(
    head: NonTermIndex,
    key: ProdID,
    action_value: AV,
    prod_elems: Vec<NamedElemImpl<TermIndex, NonTermIndex>>,
  ) -> Self {
    ProdImpl {
      head,
      key,
      action_value,
      prod_elems,
    }
  }
}

#[derive(Debug)]
struct NonTermImpl<NT> {
  key: NT,
  prods: SVec<ProdIndex, IdentityKeyExtractor>,
  nullable: bool,
  firsts: SVec<TermIndex, IdentityKeyExtractor>,
  follows: SVec<TermIndex, IdentityKeyExtractor>,
}

#[derive(Default)]
struct ProdImplKeyExtractor;

impl<ProdID, AV> KeyExtractor<ProdImpl<ProdID, AV>> for ProdImplKeyExtractor {
  type Key = ProdID;

  fn extract_key<'a>(&self, prod: &'a ProdImpl<ProdID, AV>) -> &'a ProdID {
    &prod.key
  }
}

#[derive(Default)]
struct NonTermImplKeyExtractor;

impl<NT> KeyExtractor<NonTermImpl<NT>> for NonTermImplKeyExtractor {
  type Key = NT;

  fn extract_key<'a>(&self, non_term: &'a NonTermImpl<NT>) -> &'a NT {
    &non_term.key
  }
}

pub struct NamedElemImpl<T, NT> {
  name: Option<Rc<String>>,
  elem: Elem<T, NT>,
}

impl<T, NT> NamedElemImpl<T, NT> {
  pub fn new<'a, S: Into<Cow<'a, str>>>(name: S, elem: Elem<T, NT>) -> Self {
    Self {
      name: Some(Rc::new(name.into().into_owned())),
      elem,
    }
  }

  pub fn name(&self) -> Option<&str> {
    self.name.as_ref().map(|n| &***n)
  }

  pub fn elem(&self) -> &Elem<T, NT> {
    &self.elem
  }
}

impl<T, NT> fmt::Debug for NamedElemImpl<T, NT>
where
  T: fmt::Debug,
  NT: fmt::Debug,
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self.name() {
      Some(name) => write!(f, "{}:{:?}", name, self.elem),
      None => write!(f, "{:?}", self.elem),
    }
  }
}

pub struct GrammarHandle<T, NT, ProdID, AV>(Rc<GrammarImpl<T, NT, ProdID, AV>>);

impl<T, NT, ProdID, AV> GrammarHandle<T, NT, ProdID, AV>
where
  T: Ord,
  NT: Ord,
  ProdID: Ord,
{
  fn new(grammar: GrammarImpl<T, NT, ProdID, AV>) -> Self {
    GrammarHandle(Rc::new(grammar))
  }

  fn get_term_by_index(&self, index: TermIndex) -> &T {
    self.0.terminals.get_by_index(index.0).expect("valid index")
  }

  fn get_non_term_impl_by_index(
    &self,
    index: NonTermIndex,
  ) -> &NonTermImpl<NT> {
    self
      .0
      .non_terminals
      .get_by_index(index.0)
      .expect("valid index")
  }

  fn get_prod_impl_by_index(&self, index: ProdIndex) -> &ProdImpl<ProdID, AV> {
    self.0.prods.get_by_index(index.0).expect("valid index")
  }

  fn get_non_term_handle_by_index(
    &self,
    index: NonTermIndex,
  ) -> NonTermHandle<T, NT, ProdID, AV> {
    NonTermHandle {
      grammar: self.clone(),
      non_term_index: index,
    }
  }

  fn get_prod_handle_by_index(
    &self,
    index: ProdIndex,
  ) -> ProdHandle<T, NT, ProdID, AV> {
    ProdHandle {
      grammar: self.clone(),
      prod_index: index,
    }
  }
}

impl<T, NT, ProdID, AV> Clone for GrammarHandle<T, NT, ProdID, AV> {
  fn clone(&self) -> Self {
    GrammarHandle(Rc::clone(&self.0))
  }
}

impl<T, NT, ProdID, AV> fmt::Debug for GrammarHandle<T, NT, ProdID, AV>
where
  T: fmt::Debug,
  NT: fmt::Debug,
  ProdID: fmt::Debug,
  AV: fmt::Debug,
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.debug_struct("GrammarHandle")
      .field("start_non_term", &self.0.start_nt)
      .field("terminals", &self.0.terminals)
      .field("non_terminals", &self.0.non_terminals)
      .field("prods", &self.0.prods)
      .finish()
  }
}

/// A handle for Prod objects.
pub struct ProdHandle<T, NT, ProdID, AV> {
  grammar: GrammarHandle<T, NT, ProdID, AV>,
  prod_index: ProdIndex,
}

impl<T, NT, ProdID, AV> ProdHandle<T, NT, ProdID, AV> {
  fn new(
    grammar: GrammarHandle<T, NT, ProdID, AV>,
    prod_index: ProdIndex,
  ) -> Self {
    ProdHandle {
      grammar,
      prod_index,
    }
  }
}

impl<T, NT, ProdID, AV> Clone for ProdHandle<T, NT, ProdID, AV> {
  fn clone(&self) -> Self {
    ProdHandle {
      grammar: self.grammar.clone(),
      prod_index: self.prod_index,
    }
  }
}

impl<T, NT, ProdID, AV> fmt::Debug for ProdHandle<T, NT, ProdID, AV>
where
  T: fmt::Debug + Ord,
  NT: fmt::Debug + Ord,
  ProdID: fmt::Debug + Ord,
  AV: fmt::Debug,
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let prod_impl = self.grammar.get_prod_impl_by_index(self.prod_index);
    f.debug_struct("ProdHandle")
      .field("head", &prod_impl.head)
      .field("action_id", &prod_impl.key)
      .field("action_value", &prod_impl.action_value)
      .field("prod_elems", &prod_impl.prod_elems)
      .finish()
  }
}

/// A handle for NonTerm objects.
pub struct NonTermHandle<T, NT, ProdID, AV> {
  grammar: GrammarHandle<T, NT, ProdID, AV>,
  non_term_index: NonTermIndex,
}

impl<T, NT, ProdID, AV> NonTermHandle<T, NT, ProdID, AV>
where
  ProdID: Ord,
{
  fn new(
    grammar: GrammarHandle<T, NT, ProdID, AV>,
    non_term_index: NonTermIndex,
  ) -> Self {
    NonTermHandle {
      grammar,
      non_term_index,
    }
  }
}

impl<T, NT, ProdID, AV> Clone for NonTermHandle<T, NT, ProdID, AV> {
  fn clone(&self) -> Self {
    NonTermHandle {
      grammar: self.grammar.clone(),
      non_term_index: self.non_term_index,
    }
  }
}

impl<T, NT, ProdID, AV> fmt::Debug for NonTermHandle<T, NT, ProdID, AV>
where
  T: Ord,
  NT: fmt::Debug + Ord,
  ProdID: fmt::Debug + Ord,
{
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    self
      .grammar
      .get_non_term_impl_by_index(self.non_term_index)
      .fmt(f)
  }
}

impl<T, NT, ProdID, AV> Grammar for GrammarHandle<T, NT, ProdID, AV>
where
  T: Ord + Clone + 'static,
  NT: Ord + Clone + 'static,
  ProdID: Ord + Clone + 'static,
{
  type Term = T;

  type NonTerm = NonTermHandle<T, NT, ProdID, AV>;

  type Prod = ProdHandle<T, NT, ProdID, AV>;

  fn start_non_term(&self) -> Self::NonTerm {
    self.get_non_term_handle_by_index(self.0.start_nt)
  }

  fn terminals(&self) -> Vec<&Self::Term> {
    self.0.terminals.iter().collect()
  }

  fn non_terminals(&self) -> Vec<Self::NonTerm> {
    (0..self.0.non_terminals.len())
      .map(|i| self.get_non_term_handle_by_index(NonTermIndex(i)))
      .collect()
  }

  fn prods(&self) -> Vec<Self::Prod> {
    (0..self.0.prods.len())
      .map(|i| self.get_prod_handle_by_index(ProdIndex(i)))
      .collect()
  }

  fn get_prod(
    &self,
    prod_id: &<Self::Prod as Prod>::Key,
  ) -> Option<Self::Prod> {
    self
      .0
      .prods
      .get_index_of(prod_id)
      .map(|i| self.get_prod_handle_by_index(ProdIndex(i)))
  }

  fn get_non_term(
    &self,
    key: &<Self::NonTerm as NonTerm>::Key,
  ) -> Option<Self::NonTerm> {
    self
      .0
      .non_terminals
      .get_index_of(key)
      .map(|i| self.get_non_term_handle_by_index(NonTermIndex(i)))
  }
}

impl<T, NT, ProdID, AV> Prod for ProdHandle<T, NT, ProdID, AV>
where
  T: Ord + Clone + 'static,
  NT: Ord + Clone + 'static,
  ProdID: Ord + Clone + 'static,
{
  type Term = T;

  type NonTerm = NonTermHandle<T, NT, ProdID, AV>;

  type Key = ProdID;

  type ActionValue = AV;

  type NamedElem = NamedElemRef<T, NT, ProdID, AV>;

  fn head(&self) -> Self::NonTerm {
    let prod_impl = self.grammar.get_prod_impl_by_index(self.prod_index);
    self.grammar.get_non_term_handle_by_index(prod_impl.head)
  }

  fn key(&self) -> &Self::Key {
    let prod_impl = self.grammar.get_prod_impl_by_index(self.prod_index);
    &prod_impl.key
  }

  fn action_value(&self) -> &Self::ActionValue {
    let prod_impl = self.grammar.get_prod_impl_by_index(self.prod_index);
    &prod_impl.action_value
  }

  fn prod_elements(&self) -> Vec<Self::NamedElem> {
    let prod_impl = self.grammar.get_prod_impl_by_index(self.prod_index);
    prod_impl
      .prod_elems
      .iter()
      .map(|elem| NamedElemRef {
        grammar: self.grammar.clone(),
        name: elem.name.clone(),
        elem: elem.elem.clone(),
      })
      .collect()
  }
}

impl<T, NT, ProdID, AV> NonTerm for NonTermHandle<T, NT, ProdID, AV>
where
  T: Ord + Clone + 'static,
  NT: Ord + Clone + 'static,
  ProdID: Ord + Clone + 'static,
{
  type Key = NT;
  type Prod = ProdHandle<T, NT, ProdID, AV>;

  fn key(&self) -> &Self::Key {
    let non_term_impl =
      self.grammar.get_non_term_impl_by_index(self.non_term_index);
    &non_term_impl.key
  }

  fn prods(&self) -> Vec<Self::Prod> {
    let non_term_impl =
      self.grammar.get_non_term_impl_by_index(self.non_term_index);
    non_term_impl
      .prods
      .iter()
      .map(|prod_index| self.grammar.get_prod_handle_by_index(*prod_index))
      .collect()
  }
}

fn insert_with_change<K, V>(
  map: &mut BTreeMap<K, BTreeSet<V>>,
  key: K,
  value: V,
  changed: &mut bool,
) where
  K: Ord,
  V: Ord,
{
  let set = map.entry(key).or_insert(BTreeSet::new());
  if set.insert(value) {
    *changed = true;
  }
}

impl<T, NT, ProdID, AV> GrammarImpl<T, NT, ProdID, AV>
where
  T: Ord + Clone + 'static,
  NT: Ord + Clone + 'static,
  ProdID: Ord + Clone + 'static,
{
  fn calculate_nullables(&mut self) {
    let mut changed = true;
    while changed {
      changed = false;
      for prod_impl in self.prods.iter() {
        if self
          .non_terminals
          .get_by_index(prod_impl.head.0)
          .unwrap()
          .nullable
        {
          continue;
        }

        let is_head_nullable = prod_impl
          .prod_elems
          .iter()
          .map(|e| &e.elem)
          .all(|e| match e {
            Elem::Term(_) => false,
            Elem::NonTerm(nt_i) => {
              self.non_terminals.get_by_index(nt_i.0).unwrap().nullable
            }
          });
        if is_head_nullable {
          self
            .non_terminals
            .get_mut_by_index(prod_impl.head.0)
            .unwrap()
            .nullable = true;
          changed = true;
        }
      }
    }
  }

  fn find_firsts(&self) -> BTreeMap<NonTermIndex, BTreeSet<TermIndex>> {
    let mut firsts = (0..self.non_terminals.len())
      .into_iter()
      .map(|i| (NonTermIndex(i), BTreeSet::new()))
      .collect();
    let mut changed = true;
    while changed {
      changed = false;
      for prod_impl in self.prods.iter() {
        'inner: for elem in prod_impl.prod_elems.iter() {
          match &elem.elem {
            Elem::Term(t_i) => {
              insert_with_change(
                &mut firsts,
                prod_impl.head,
                *t_i,
                &mut changed,
              );
              break 'inner;
            }
            Elem::NonTerm(nt_i) => {
              let curr_firsts = firsts.get_mut(&nt_i).unwrap().clone();
              for t_i in curr_firsts {
                insert_with_change(
                  &mut firsts,
                  prod_impl.head,
                  t_i,
                  &mut changed,
                );
              }

              if !self.non_terminals.get_by_index(nt_i.0).unwrap().nullable {
                break 'inner;
              }
            }
          }
        }
      }
    }
    firsts
  }

  fn calculate_firsts(&mut self) {
    let firsts = self.find_firsts();
    for (nt_i, firsts) in firsts {
      self.non_terminals.get_mut_by_index(nt_i.0).unwrap().firsts =
        firsts.into_iter().collect();
    }
  }

  fn find_follows(&self) -> BTreeMap<NonTermIndex, BTreeSet<TermIndex>> {
    let mut follows = (0..self.non_terminals.len())
      .into_iter()
      .map(|i| (NonTermIndex(i), BTreeSet::new()))
      .collect();
    let mut changed = true;
    while changed {
      changed = false;
      for prod_impl in self.prods.iter() {
        'outer: for (i, elem) in prod_impl.prod_elems.iter().enumerate() {
          if let Elem::NonTerm(follow_i) = elem.elem {
            for next_elem in prod_impl.prod_elems[i + 1..].iter() {
              match next_elem.elem {
                Elem::Term(t_i) => {
                  insert_with_change(&mut follows, follow_i, t_i, &mut changed);
                  continue 'outer;
                }
                Elem::NonTerm(nt_i) => {
                  let firsts_of_nt = self
                    .non_terminals
                    .get_by_index(nt_i.0)
                    .unwrap()
                    .firsts
                    .clone();
                  for t_i in firsts_of_nt.iter() {
                    insert_with_change(
                      &mut follows,
                      follow_i,
                      *t_i,
                      &mut changed,
                    );
                  }

                  if !self.non_terminals.get_by_index(nt_i.0).unwrap().nullable
                  {
                    continue 'outer;
                  }
                }
              }
            }

            // If we've got here, then we completed the 'inner loop. This means
            // that all elements of the production were nullable after i. Add
            // the follow set of the head to the follow set of elem.
            let head_follows = follows.get(&prod_impl.head).unwrap().clone();
            for t_i in head_follows.iter() {
              insert_with_change(&mut follows, follow_i, *t_i, &mut changed);
            }
          }
        }
      }
    }
    follows
  }

  fn calculate_follows(&mut self) {
    let follows = self.find_follows();
    for (nt_i, follows) in follows {
      self.non_terminals.get_mut_by_index(nt_i.0).unwrap().follows =
        follows.into_iter().collect();
    }
  }
}
