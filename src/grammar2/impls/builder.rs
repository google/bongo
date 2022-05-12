use std::{
  collections::{btree_map::Entry, BTreeMap, BTreeSet},
  fmt::Debug,
  rc::Rc,
};

use super::Elem;

fn create_index_map<T>(iter: impl IntoIterator<Item = T>) -> BTreeMap<T, usize>
where
  T: Ord,
{
  let mut items: Vec<_> = iter.into_iter().collect();
  items.sort();
  items.into_iter().enumerate().map(|(i, x)| (x, i)).collect()
}

pub struct ProdContents<T, NT, ProdID, AV> {
  id: ProdID,
  head: NT,
  action_value: AV,
  elements: Vec<super::NamedElemImpl<T, NT>>,
}

pub struct GrammarBuilder<T, NT, ProdID, AV> {
  terminals: BTreeSet<T>,
  non_terminals: BTreeMap<NT, BTreeSet<ProdID>>,
  start_nt: NT,
  prods: BTreeMap<ProdID, ProdContents<T, NT, ProdID, AV>>,
}

impl<T, NT, ProdID, AV> GrammarBuilder<T, NT, ProdID, AV>
where
  T: Clone + Ord + 'static,
  NT: Clone + Ord + 'static,
  ProdID: Clone + Ord + Debug + 'static,
{
  pub fn new(start_nt: impl Into<NT>) -> Self {
    let start_nt = start_nt.into();
    GrammarBuilder {
      terminals: BTreeSet::new(),
      non_terminals: [(start_nt.clone(), BTreeSet::new())]
        .into_iter()
        .collect(),
      prods: BTreeMap::new(),
      start_nt,
    }
  }

  pub fn add_prod<F>(
    &mut self,
    id: impl Into<ProdID>,
    head: impl Into<NT>,
    action_value: impl Into<AV>,
    elem_builder: F,
  ) -> &mut Self
  where
    F: for<'a> FnOnce(ElementBuilder<'a, T, NT>),
  {
    let id = id.into();
    let mut elements = Vec::new();
    elem_builder(ElementBuilder::new(&mut elements));
    let prod_contents = match self.prods.entry(id.clone()) {
      Entry::Vacant(vac) => vac.insert(ProdContents {
        id,
        head: head.into(),
        action_value: action_value.into(),
        elements,
      }),
      Entry::Occupied(_) => panic!("duplicate production id: {:?}", id),
    };

    for elem in &prod_contents.elements {
      match &elem.elem {
        Elem::Term(t) => {
          self.terminals.insert(t.clone());
        }
        Elem::NonTerm(nt) => {
          self
            .non_terminals
            .entry(nt.clone())
            .or_insert_with(|| BTreeSet::new());
        }
      }
    }

    self
      .non_terminals
      .entry(prod_contents.head.clone())
      .or_insert_with(|| BTreeSet::new())
      .insert(prod_contents.id.clone());
    self
  }

  pub fn build(self) -> super::GrammarHandle<T, NT, ProdID, AV> {
    // We need to create index maps for the terms, non-terms, and productions to allow
    // us to use the indexes in the internal data structures for the grammar, instead of
    // repeatedly cloning the keys for the various items.
    let term_index_map = create_index_map(self.terminals.iter().cloned());
    let non_term_index_map =
      create_index_map(self.non_terminals.iter().map(|(nt, _)| nt).cloned());
    let prod_index_map =
      create_index_map(self.prods.iter().map(|(id, _)| id).cloned());
    let prods = self
      .prods
      .into_iter()
      .map(|(_, prod_contents)| {
        let ProdContents {
          id,
          head,
          action_value,
          elements,
        } = prod_contents;

        super::ProdImpl::new(
          super::NonTermIndex(non_term_index_map[&head]),
          id,
          action_value,
          elements
            .into_iter()
            .map(|elem| super::NamedElemImpl {
              name: elem.name,
              elem: match elem.elem {
                Elem::Term(t) => {
                  Elem::Term(super::TermIndex(term_index_map[&t]))
                }
                Elem::NonTerm(nt) => {
                  Elem::NonTerm(super::NonTermIndex(non_term_index_map[&nt]))
                }
              },
            })
            .collect(),
        )
      })
      .collect();

    let non_terminals = self
      .non_terminals
      .into_iter()
      .map(|(nt, rule_contents)| super::NonTermImpl {
        key: nt,
        prods: rule_contents
          .into_iter()
          .map(|prod_id| super::ProdIndex(prod_index_map[&prod_id]))
          .collect(),
        nullable: false,
        firsts: vec![].into_iter().collect(),
        follows: vec![].into_iter().collect(),
      })
      .collect();
    let mut grammar_impl = super::GrammarImpl {
      terminals: self
        .terminals
        .into_iter()
        .map(|key| super::TermImpl { key })
        .collect(),
      non_terminals,
      start_nt: super::NonTermIndex(non_term_index_map[&self.start_nt]),
      prods,
    };
    grammar_impl.calculate_nullables();
    grammar_impl.calculate_firsts();
    grammar_impl.calculate_follows();
    super::GrammarHandle::new(grammar_impl)
  }
}

pub struct ElementBuilder<'a, T, NT> {
  elements: &'a mut Vec<super::NamedElemImpl<T, NT>>,
}

impl<'a, T, NT> ElementBuilder<'a, T, NT> {
  fn new(elements: &'a mut Vec<super::NamedElemImpl<T, NT>>) -> Self {
    ElementBuilder { elements }
  }

  fn elem(&mut self, name: Option<Rc<String>>, elem: Elem<T, NT>) -> &mut Self {
    self.elements.push(super::NamedElemImpl { name, elem });
    self
  }

  pub fn term(&mut self, term: impl Into<T>) -> &mut Self {
    self.elem(None, Elem::Term(term.into()))
  }

  pub fn non_term(&mut self, non_term: impl Into<NT>) -> &mut Self {
    self.elem(None, Elem::NonTerm(non_term.into()))
  }

  pub fn named_term(
    &mut self,
    term: impl Into<T>,
    name: impl Into<String>,
  ) -> &mut Self {
    self.elem(Some(Rc::new(name.into())), Elem::Term(term.into()))
  }

  pub fn named_non_term(
    &mut self,
    non_term: impl Into<NT>,
    name: impl Into<String>,
  ) -> &mut Self {
    self.elem(Some(Rc::new(name.into())), Elem::NonTerm(non_term.into()))
  }
}
