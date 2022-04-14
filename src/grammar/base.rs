// Copyright 2018 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     https://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

pub mod builder;
mod cmp_wrappers;
mod element_types;

use {
  crate::utils::{breadth_first_search, Name, ToDoc},
  std::collections::{BTreeMap, BTreeSet},
};

use std::fmt::Debug;

pub use cmp_wrappers::{NoCompare, ParentRef, RefCompare};
pub use element_types::{NonTerminal, Terminal};

/// A single element (terminal or non-terminal).
#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum Elem<T, NT> {
  Term(T),
  NonTerm(NT),
}

impl<T, NT> Elem<T, NT> {
  /// If this element is a terminal, returns a `Some` value containing a
  /// terminal datum. Returns `None` otherwise.
  pub fn as_term(&self) -> Option<&T> {
    match self {
      Elem::NonTerm(_) => None,
      Elem::Term(t) => Some(t),
    }
  }

  /// Gets an element as a nonterm. Returns a `None` value otherwise.
  pub fn as_nonterm(&self) -> Option<&NT> {
    match self {
      Elem::NonTerm(nt) => Some(nt),
      Elem::Term(_) => None,
    }
  }
}

impl<T, NT> ToDoc for Elem<T, NT>
where
  T: ToDoc,
  NT: ToDoc,
{
  fn to_doc<'a, DA: pretty::DocAllocator<'a>>(
    &self,
    da: &'a DA,
  ) -> pretty::DocBuilder<'a, DA>
  where
    DA::Doc: Clone,
  {
    match self {
      Elem::NonTerm(nt) => {
        da.text("<").append(nt.to_doc(da)).append(da.text(">"))
      }
      Elem::Term(t) => t.to_doc(da),
    }
  }
}

impl<T, NT> std::fmt::Debug for Elem<T, NT>
where
  T: Debug,
  NT: Debug,
{
  fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Elem::Term(term) => fmt.write_str(&format!("{:?}", term)),
      Elem::NonTerm(nt) => fmt.write_str(&format!("<{:?}>", nt)),
    }
  }
}

/// An element within a production. Includes an optional identifier.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ProdElement<T, NT> {
  identifier: Option<Name>,
  element: Elem<T, NT>,
}

impl<T, NT> ProdElement<T, NT> {
  /// Returns a ProductionElement that is annotated with "name".
  pub fn new_with_name(name: Name, e: Elem<T, NT>) -> Self {
    ProdElement {
      identifier: Some(name),
      element: e,
    }
  }

  /// Returns a ProductionElement that takes an optional identifier.
  pub fn new(name: Option<Name>, e: Elem<T, NT>) -> Self {
    ProdElement {
      identifier: name,
      element: e,
    }
  }

  /// Returns an unannotated ProductionElement
  pub fn new_empty(e: Elem<T, NT>) -> Self {
    ProdElement {
      identifier: None,
      element: e,
    }
  }

  // Returns the optional id annotation.
  pub fn id(&self) -> Option<&Name> {
    self.identifier.as_ref()
  }

  // Returns the element.
  pub fn elem(&self) -> &Elem<T, NT> {
    &self.element
  }
}

impl<T, NT> ToDoc for ProdElement<T, NT>
where
  T: ToDoc,
  NT: ToDoc,
{
  fn to_doc<'a, DA: pretty::DocAllocator<'a>>(
    &self,
    da: &'a DA,
  ) -> pretty::DocBuilder<'a, DA>
  where
    DA::Doc: Clone,
  {
    let prefix = if let Some(id) = &self.identifier {
      id.to_doc(da).append(da.text(":").append(da.softline_()))
    } else {
      da.nil()
    };

    prefix.append(self.element.to_doc(da))
  }
}

impl<T, NT> std::fmt::Debug for ProdElement<T, NT>
where
  T: Debug,
  NT: Debug,
{
  fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
    if let Some(name) = &self.identifier {
      fmt.write_str(&format!("{}:", name))?;
    }
    std::fmt::Debug::fmt(self.elem(), fmt)
  }
}

impl<T, NT> From<Elem<T, NT>> for ProdElement<T, NT> {
  fn from(e: Elem<T, NT>) -> ProdElement<T, NT> {
    ProdElement {
      identifier: None,
      element: e,
    }
  }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct ProdInner<T, NT, AK, AV> {
  action_key: AK,
  action_value: AV,
  elements: Vec<ProdElement<T, NT>>,
}

impl<T, NT, AK, AV> ProdInner<T, NT, AK, AV> {
  fn new(
    action_key: AK,
    action_value: AV,
    elements: Vec<ProdElement<T, NT>>,
  ) -> Self {
    ProdInner {
      action_key,
      action_value,
      elements,
    }
  }

  pub fn prod_elements(&self) -> &Vec<ProdElement<T, NT>> {
    &self.elements
  }

  pub fn elements_iter(&self) -> impl Iterator<Item = &Elem<T, NT>> + Clone {
    self.elements.iter().map(|prod_elem| &prod_elem.element)
  }

  pub fn element_at(&self, index: usize) -> Option<&Elem<T, NT>> {
    self.elements.get(index).map(|prod_elem| &prod_elem.element)
  }

  pub fn action_key(&self) -> &AK {
    &self.action_key
  }
}
impl<T, NT, AK, AV> ToDoc for ProdInner<T, NT, AK, AV>
where
  T: ToDoc,
  NT: ToDoc,
{
  fn to_doc<'a, DA: pretty::DocAllocator<'a>>(
    &self,
    da: &'a DA,
  ) -> pretty::DocBuilder<'a, DA>
  where
    DA::Doc: Clone,
  {
    if self.elements.is_empty() {
      da.text("ε")
    } else {
      da.intersperse(self.elements.iter().map(|e| e.to_doc(da)), da.softline())
    }
  }
}

/// A key type for production instances.
///
/// In order to be able to transform grammars, we need to have a 'static
/// lifetime key value that allows us to track which production is used. It's
/// possible we may be able to transform that later, but for now this is simple
/// enough.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ProdKey<NT, AK> {
  head: NT,
  action_key: AK,
}

impl<NT, AK> ProdKey<NT, AK> {
  /// Gets the head of this prod key.
  pub fn head(&self) -> &NT {
    &self.head
  }

  /// Gets the action_key of this prod key.
  pub fn action_key(&self) -> &AK {
    &self.action_key
  }
}

/// A concrete raw rule value as stored inside a Grammar struct.
#[derive(Clone, Debug)]
struct RuleInner<T, NT, AK, AV> {
  head: NT,
  prods: Vec<ProdInner<T, NT, AK, AV>>,
}

impl<T, NT, AK, AV> RuleInner<T, NT, AK, AV> {
  pub fn new(head: NT, prods: Vec<ProdInner<T, NT, AK, AV>>) -> Self {
    RuleInner { head, prods }
  }

  pub fn head(&self) -> &NT {
    &self.head
  }

  pub fn prods(&self) -> &Vec<ProdInner<T, NT, AK, AV>> {
    &self.prods
  }
}

impl<T, NT, AK, AV> ToDoc for RuleInner<T, NT, AK, AV>
where
  T: ToDoc,
  NT: ToDoc,
{
  fn to_doc<'a, DA: pretty::DocAllocator<'a>>(
    &self,
    da: &'a DA,
  ) -> pretty::DocBuilder<'a, DA>
  where
    DA::Doc: Clone,
  {
    self
      .head
      .to_doc(da)
      .append(da.text(" =>"))
      .append(da.softline())
      .append(da.intersperse(
        self.prods.iter().map(|prod| prod.to_doc(da)),
        da.text(" |").append(da.softline()),
      ))
  }
}

/// A context-free language grammar.
///
/// This is a context-free grammar consisting of
///
/// - A start nonterminal
/// - A set of rules, each which consist of
///   - A head nonterminal
///   - A set of productions, where each production consist of
///     - A list of production elements, which may have an identifier,
///       and is either a terminal or nonterminal (an element).
///     - An action key (identifier unique to that production)
///     - An action value (Data associated with that action key)
///
/// Grammars are read-only, and the accessors use the lifetime of the
/// grammar object.
#[derive(Clone)]
pub struct Grammar<T, NT, AK, AV> {
  start_symbol: NT,
  rule_set: BTreeMap<NT, RuleInner<T, NT, AK, AV>>,
}

impl<T, NT, AK, AV> std::fmt::Debug for Grammar<T, NT, AK, AV>
where
  T: Debug,
  NT: Debug,
{
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    let mut dbg_struct = f.debug_struct("Grammar");
    dbg_struct.field("Terms", &self.get_terminals().collect::<Vec<_>>());
    dbg_struct.field("NonTerms", &self.get_nonterminals().collect::<Vec<_>>());
    dbg_struct
      .field("Rules", &self.rules().collect::<Vec<Rule<T, NT, AK, AV>>>());
    dbg_struct.finish()
  }
}

impl<T, NT, AK, AV> Grammar<T, NT, AK, AV> {
  /// Returns the start nonterminal for this grammar.
  pub fn start_nt(&self) -> &NT {
    &self.start_symbol
  }

  fn get_elements(&self) -> impl Iterator<Item = &Elem<T, NT>> {
    self
      .rule_set
      .values()
      .flat_map(|r| &r.prods)
      .flat_map(|p| p.elements_iter())
  }

  fn get_terminals(&self) -> impl Iterator<Item = &T> {
    self.get_elements().filter_map(|e| e.as_term())
  }

  fn get_nonterminals(&self) -> impl Iterator<Item = &NT> {
    self.get_elements().filter_map(|e| e.as_nonterm())
  }

  /// Returns an iterator over all of the rules for this grammar.
  pub fn rules(&self) -> impl Iterator<Item = Rule<T, NT, AK, AV>> {
    self.rule_set.iter().map(move |(_, rule)| Rule {
      grammar: ParentRef::new(self),
      rule: RefCompare::new(rule),
    })
  }

  /// Gets an iterator over all productions in the grammar.
  pub fn prods(&self) -> impl Iterator<Item = Prod<T, NT, AK, AV>> {
    self.rules().flat_map(move |rule| rule.prods())
  }
}

impl<T, NT, AK, AV> Grammar<T, NT, AK, AV>
where
  NT: Ord + Clone,
  AK: Ord + Clone,
{
  fn new(
    start: NT,
    rule_set: impl IntoIterator<Item = RuleInner<T, NT, AK, AV>>,
  ) -> Result<Self, GrammarErrors<NT>> {
    let g = Grammar {
      start_symbol: start,
      rule_set: rule_set
        .into_iter()
        .map(|r| (r.head().clone(), r))
        .collect(),
    };

    g.check_grammar().map(|_| g)
  }

  /// Returns a map over rules of the grammar, keyed by the rule's head nonterminal.
  pub fn rule_set(&self) -> BTreeMap<&NT, Rule<T, NT, AK, AV>> {
    self
      .rule_set
      .iter()
      .map(|(k, rule)| (k, Rule::new(self, rule)))
      .collect()
  }

  /// Gets the rule that has the given nonterminal as a head.
  pub fn try_get_rule<'a>(
    &'a self,
    nt: &NT,
  ) -> Option<Rule<'a, T, NT, AK, AV>> {
    self.rule_set.get(nt).map(|rule| Rule::new(self, rule))
  }

  /// Gets the rule that has the given nonterminal as a head.
  pub fn get_rule<'a>(&'a self, nt: &NT) -> Rule<'a, T, NT, AK, AV> {
    self
      .try_get_rule(nt)
      .expect("An NT rule exists in the grammar.")
  }

  fn nonterminals_without_rules(&self) -> BTreeSet<&NT> {
    self
      .get_nonterminals()
      .filter(move |nt| !self.rule_set.contains_key(nt))
      .collect()
  }

  fn rules_without_prods(&self) -> BTreeSet<&NT> {
    let rules = self.rules();
    let prodless_rules = rules
      .into_iter()
      .filter(|r| matches!(r.prods().next(), None));
    let head_iter = prodless_rules.map(|r| r.head());
    head_iter.collect()
  }

  fn reachable_nonterms(&self) -> BTreeSet<&NT> {
    breadth_first_search(std::iter::once(&self.start_symbol), |nt| {
      self
        .get_rule(nt)
        .prods()
        .flat_map(|p| p.elements())
        .filter_map(|e| e.as_nonterm())
        .collect::<BTreeSet<_>>()
    })
  }

  fn unreachable_nonterms(&self) -> BTreeSet<&NT> {
    let reachable_nonterms = self.reachable_nonterms();
    self
      .get_nonterminals()
      .filter(|nt| !reachable_nonterms.contains(nt))
      .collect()
  }
}

impl<T, NT, AK, AV> Grammar<T, NT, AK, AV>
where
  T: ToDoc,
  NT: ToDoc,
{
  pub fn to_pretty(&self) -> String {
    let arena = pretty::Arena::new();
    format!("{}", self.to_doc(&arena).into_doc().pretty(80))
  }
}

#[derive(Clone, Debug)]
pub struct GrammarErrors<NT> {
  unreachable_nonterms: BTreeSet<NT>,
  nonterms_without_rules: BTreeSet<NT>,
  rules_without_prods: BTreeSet<NT>,
}

impl<NT> GrammarErrors<NT> {
  fn into_result(self) -> Result<(), Self> {
    if self.unreachable_nonterms.is_empty()
      && self.nonterms_without_rules.is_empty()
      && self.rules_without_prods.is_empty()
    {
      Ok(())
    } else {
      Err(self)
    }
  }
}

impl<T, NT, AK, AV> Grammar<T, NT, AK, AV>
where
  NT: Ord + Clone,
  AK: Ord + Clone,
{
  fn check_grammar(&self) -> Result<(), GrammarErrors<NT>> {
    GrammarErrors {
      unreachable_nonterms: self
        .unreachable_nonterms()
        .into_iter()
        .cloned()
        .collect(),
      nonterms_without_rules: self
        .nonterminals_without_rules()
        .into_iter()
        .cloned()
        .collect(),
      rules_without_prods: self
        .rules_without_prods()
        .into_iter()
        .cloned()
        .collect(),
    }
    .into_result()
  }
}

impl<T, NT, AK, AV> ToDoc for Grammar<T, NT, AK, AV>
where
  T: ToDoc,
  NT: ToDoc,
{
  fn to_doc<'a, DA: pretty::DocAllocator<'a>>(
    &self,
    da: &'a DA,
  ) -> pretty::DocBuilder<'a, DA>
  where
    DA::Doc: Clone,
  {
    let start_entry = da
      .text("Start =")
      .group()
      .append(da.softline())
      .append(self.start_nt().to_doc(da));
    let rules_entry = da.text("Rules ").append(
      da.softline()
        .append(
          da.concat(self.rule_set.iter().map(|rule| {
            rule.1.to_doc(da).append(da.text(";")).append(da.softline())
          }))
          .nest(2),
        )
        .braces(),
    );

    da.concat(
      vec![start_entry, rules_entry]
        .into_iter()
        .map(|doc| doc.append(da.text(",")).append(da.softline())),
    )
  }
}

// ------------

/// A rule within a grammar.
///
/// A rule consists of a head nonterminal, and zero or more different possible
/// productions.
///
/// When using rules as keys, collections should not include rules from
/// different grammars. Rules will panic if compared with rules from other
/// grammars.
#[derive(Derivative)]
#[derivative(
  Copy(bound = ""),
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = "")
)]
pub struct Rule<'a, T, NT, AK, AV> {
  grammar: ParentRef<'a, Grammar<T, NT, AK, AV>>,
  rule: RefCompare<'a, RuleInner<T, NT, AK, AV>>,
}

impl<'a, T, NT, AK, AV> Rule<'a, T, NT, AK, AV> {
  /// Returns the head nonterminal.
  pub fn head(&self) -> &'a NT {
    &self.rule.head
  }

  /// Returns an iterator over the productions of this rule.
  pub fn prods(&self) -> impl Iterator<Item = Prod<'a, T, NT, AK, AV>> {
    let prods = &self.rule.prods;
    prods.iter().map({
      let grammar = *self.grammar;
      let head = &self.rule.head;
      move |prod| Prod::new(grammar, head, prod)
    })
  }
}

impl<'a, T, NT, AK, AV> Rule<'a, T, NT, AK, AV>
where
  NT: Clone,
  AK: Ord + Clone,
{
  fn new(
    grammar: &'a Grammar<T, NT, AK, AV>,
    rule: &'a RuleInner<T, NT, AK, AV>,
  ) -> Self {
    Rule {
      grammar: ParentRef::new(grammar),
      rule: RefCompare::new(rule),
    }
  }
}

impl<'a, T, NT, AK, AV> std::fmt::Debug for Rule<'a, T, NT, AK, AV>
where
  T: Debug,
  NT: Debug,
{
  fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
    let mut dbg_struct = fmt.debug_struct("Rule");
    dbg_struct.field("head", self.head());
    dbg_struct.field("prods", &self.prods().collect::<Vec<_>>());
    dbg_struct.finish()
  }
}

// ------------

/// A single production in a grammar.
///
/// A production has a head, which is a nonterminal which its reduced to,
/// A sequence of ProductionElements, indicating the body of the production,
/// and an action key which gives this production (along with the head) a unique
/// value.
pub struct Prod<'a, T, NT, AK, AV> {
  grammar: ParentRef<'a, Grammar<T, NT, AK, AV>>,
  head: &'a NT,
  prod: RefCompare<'a, ProdInner<T, NT, AK, AV>>,
}

impl<'a, T, NT, AK, AV> Ord for Prod<'a, T, NT, AK, AV>
where
  NT: Ord,
{
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.prod.cmp(&other.prod)
  }
}

impl<'a, T, NT, AK, AV> PartialOrd for Prod<'a, T, NT, AK, AV>
where
  NT: Ord,
{
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl<'a, T, NT, AK, AV> PartialEq for Prod<'a, T, NT, AK, AV>
where
  NT: Ord,
{
  fn eq(&self, other: &Self) -> bool {
    self.prod == other.prod
  }
}

impl<'a, T, NT, AK, AV> Eq for Prod<'a, T, NT, AK, AV> where NT: Ord {}

impl<'a, T, NT, AK, AV> Copy for Prod<'a, T, NT, AK, AV> {}

impl<'a, T, NT, AK, AV> Prod<'a, T, NT, AK, AV> {
  fn new(
    grammar: &'a Grammar<T, NT, AK, AV>,
    head: &'a NT,
    prod: &'a ProdInner<T, NT, AK, AV>,
  ) -> Self {
    Prod {
      grammar: ParentRef::new(grammar),
      head,
      prod: RefCompare::new(prod),
    }
  }

  /// Returns the head of this production.
  pub fn head(&self) -> &'a NT {
    self.head
  }

  /// Returns the elements of this production, including any identifiers of the production.
  pub fn prod_elements(&self) -> &'a Vec<ProdElement<T, NT>> {
    &self.prod.elements
  }

  /// Returns an iterator over the elements of this production, without any identifiers.
  pub fn elements(&self) -> impl Iterator<Item = &'a Elem<T, NT>> + Clone {
    self.prod.elements_iter()
  }
  /// Returns the number of elements in this production.
  pub fn num_elements(&self) -> usize {
    self.prod.elements.len()
  }

  /// Returns the action key of this production.
  pub fn action_key(&self) -> &'a AK {
    self.prod.action_key()
  }
}

impl<'a, T, NT, AK, AV> Prod<'a, T, NT, AK, AV>
where
  NT: Clone,
  AK: Clone,
{
  /// Returns the prod element at a given index. Panics on out-of-bounds access.
  pub fn prod_element_at(
    &self,
    index: usize,
  ) -> Option<&'a ProdElement<T, NT>> {
    self.prod.elements.get(index)
  }

  /// Returns the element at a given index. Panics on out-of-bounds access.
  pub fn element_at(&self, index: usize) -> Option<&'a Elem<T, NT>> {
    self.prod.element_at(index)
  }

  /// Returns the action value of this production.
  pub fn action_value(&self) -> &'a AV {
    &self.prod.action_value
  }

  /// Returns the `ProdKey` of this production.
  pub fn prod_key(&self) -> ProdKey<NT, AK> {
    ProdKey {
      head: self.head().clone(),
      action_key: self.action_key().clone(),
    }
  }

  /// Returns a `Some` containing the first element of this production, or None if
  /// the production is empty.
  pub fn first_elem(&self) -> Option<&'a Elem<T, NT>> {
    self.prod.elements.first().map(|pe| pe.elem())
  }
}

impl<'a, T, NT, AK, AV> Clone for Prod<'a, T, NT, AK, AV> {
  fn clone(&self) -> Self {
    Prod {
      grammar: self.grammar,
      head: self.head,
      prod: self.prod,
    }
  }
}

impl<'a, T, NT, AK, AV> std::fmt::Debug for Prod<'a, T, NT, AK, AV>
where
  T: Debug,
  NT: Debug,
{
  fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
    let mut dbg_struct = fmt.debug_struct("Prod");
    dbg_struct.field("head", self.head());
    dbg_struct.field("elems", &self.prod_elements());
    dbg_struct.finish()
  }
}
