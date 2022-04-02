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

pub use cmp_wrappers::{NoCompare, ParentRef, RefCompare};
pub use element_types::{BaseElementTypes, ElemTypes, NonTerminal, Terminal};

/// A single element (terminal or non-terminal).
#[derive(Derivative)]
#[derivative(
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = ""),
  PartialOrd = "feature_allow_slow_enum",
  Ord = "feature_allow_slow_enum"
)]
pub enum Elem<E: ElemTypes> {
  Term(E::Term),
  NonTerm(E::NonTerm),
}

impl<E: ElemTypes> Elem<E> {
  /// If this element is a terminal, returns a `Some` value containing a
  /// terminal datum. Returns `None` otherwise.
  pub fn as_term(&self) -> Option<&E::Term> {
    match self {
      Elem::NonTerm(_) => None,
      Elem::Term(t) => Some(t),
    }
  }

  /// Gets an element as a nonterm. Returns a `None` value otherwise.
  pub fn as_nonterm(&self) -> Option<&E::NonTerm> {
    match self {
      Elem::NonTerm(nt) => Some(nt),
      Elem::Term(_) => None,
    }
  }

  /// Clone this element into an element of another ElementTypes instance.
  /// The `E::Term` and `E::NonTerm` datum types must be the same as those in
  /// `E2`.
  pub fn clone_as_other<E2>(&self) -> Elem<E2>
  where
    E2: ElemTypes<Term = E::Term, NonTerm = E::NonTerm>,
  {
    match self {
      Elem::Term(t) => Elem::Term(t.clone()),
      Elem::NonTerm(nt) => Elem::NonTerm(nt.clone()),
    }
  }

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

impl<E: ElemTypes> std::fmt::Debug for Elem<E> {
  fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Elem::Term(term) => fmt.write_str(&format!("{:?}", term)),
      Elem::NonTerm(nt) => fmt.write_str(&format!("<{:?}>", nt)),
    }
  }
}

/// An element within a production. Includes an optional identifier.
#[derive(Derivative)]
#[derivative(
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = "")
)]
pub struct ProdElement<E: ElemTypes> {
  identifier: Option<Name>,
  element: Elem<E>,
}

impl<E: ElemTypes> ProdElement<E> {
  /// Returns a ProductionElement that is annotated with "name".
  pub fn new_with_name(name: Name, e: Elem<E>) -> Self {
    ProdElement {
      identifier: Some(name),
      element: e,
    }
  }

  /// Returns a ProductionElement that takes an optional identifier.
  pub fn new(name: Option<Name>, e: Elem<E>) -> Self {
    ProdElement {
      identifier: name,
      element: e,
    }
  }

  /// Returns an unannotated ProductionElement
  pub fn new_empty(e: Elem<E>) -> Self {
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
  pub fn elem(&self) -> &Elem<E> {
    &self.element
  }

  /// Clone this element into an element of another ElementTypes instance.
  /// The `E::Term` and `E::NonTerm` datum types must be the same as those in
  /// `E2`.
  pub fn clone_as_other<E2>(&self) -> ProdElement<E2>
  where
    E2: ElemTypes<Term = E::Term, NonTerm = E::NonTerm>,
  {
    ProdElement {
      identifier: self.identifier.clone(),
      element: self.element.clone_as_other(),
    }
  }

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

impl<E: ElemTypes> std::fmt::Debug for ProdElement<E> {
  fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
    if let Some(name) = &self.identifier {
      fmt.write_str(&format!("{}:", name))?;
    }
    std::fmt::Debug::fmt(self.elem(), fmt)
  }
}

impl<E: ElemTypes> From<Elem<E>> for ProdElement<E> {
  fn from(e: Elem<E>) -> ProdElement<E> {
    ProdElement {
      identifier: None,
      element: e,
    }
  }
}

#[derive(Derivative)]
#[derivative(
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = ""),
  Debug(bound = "")
)]
struct ProdInner<E: ElemTypes> {
  action_key: E::ActionKey,
  elements: Vec<ProdElement<E>>,
}

impl<E: ElemTypes> ProdInner<E> {
  fn new(action_key: E::ActionKey, elements: Vec<ProdElement<E>>) -> Self {
    ProdInner {
      action_key,
      elements,
    }
  }

  pub fn prod_elements(&self) -> &Vec<ProdElement<E>> {
    &self.elements
  }

  pub fn elements_iter(&self) -> impl Iterator<Item = &Elem<E>> + Clone {
    self.elements.iter().map(|prod_elem| &prod_elem.element)
  }

  pub fn element_at(&self, index: usize) -> Option<&Elem<E>> {
    self.elements.get(index).map(|prod_elem| &prod_elem.element)
  }

  pub fn action_key(&self) -> &E::ActionKey {
    &self.action_key
  }

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
#[derive(Derivative)]
#[derivative(
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = ""),
  Debug(bound = "")
)]
pub struct ProdKey<E: ElemTypes> {
  head: E::NonTerm,
  action_key: E::ActionKey,
}

impl<E: ElemTypes> ProdKey<E> {
  /// Gets the head of this prod key.
  pub fn head(&self) -> &E::NonTerm {
    &self.head
  }

  /// Gets the action_key of this prod key.
  pub fn action_key(&self) -> &E::ActionKey {
    &self.action_key
  }
}

/// A concrete raw rule value as stored inside a Grammar struct.
#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
struct RuleInner<E: ElemTypes> {
  head: E::NonTerm,
  prods: Vec<ProdInner<E>>,
}

impl<E: ElemTypes> RuleInner<E> {
  pub fn new(head: E::NonTerm, prods: Vec<ProdInner<E>>) -> Self {
    RuleInner { head, prods }
  }

  pub fn head(&self) -> &E::NonTerm {
    &self.head
  }

  pub fn prods(&self) -> &Vec<ProdInner<E>> {
    &self.prods
  }

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
#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct Grammar<E: ElemTypes> {
  start_symbol: E::NonTerm,
  rule_set: BTreeMap<E::NonTerm, RuleInner<E>>,
  action_map: BTreeMap<E::ActionKey, E::ActionValue>,
}

impl<E: ElemTypes> std::fmt::Debug for Grammar<E> {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    let mut dbg_struct = f.debug_struct("Grammar");
    dbg_struct.field("Terms", &self.get_terminals().collect::<Vec<_>>());
    dbg_struct.field("NonTerms", &self.get_nonterminals().collect::<Vec<_>>());
    dbg_struct.field("Rules", &self.rules().collect::<Vec<_>>());
    dbg_struct.finish()
  }
}

impl<E: ElemTypes> Grammar<E> {
  fn new(
    start: E::NonTerm,
    rule_set: impl IntoIterator<Item = RuleInner<E>>,
    action_map: BTreeMap<E::ActionKey, E::ActionValue>,
  ) -> Result<Self, GrammarErrors<E>> {
    let g = Grammar {
      start_symbol: start,
      rule_set: rule_set
        .into_iter()
        .map(|r| (r.head().clone(), r))
        .collect(),
      action_map,
    };

    g.check_grammar().map(|_| g)
  }

  /// Returns the start nonterminal for this grammar.
  pub fn start_nt(&self) -> &E::NonTerm {
    &self.start_symbol
  }

  /// Returns an iterator over all of the rules for this grammar.
  pub fn rules<'a>(&'a self) -> impl Iterator<Item = Rule<'a, E>> {
    self.rule_set.iter().map(move |(_, rule)| Rule {
      grammar: ParentRef::new(self),
      rule: RefCompare::new(rule),
    })
  }

  /// Returns a map over rules of the grammar, keyed by the rule's head nonterminal.
  pub fn rule_set<'a>(&'a self) -> BTreeMap<&'a E::NonTerm, Rule<'a, E>> {
    self
      .rule_set
      .iter()
      .map(|(k, rule)| (k, Rule::new(self, rule)))
      .collect()
  }

  /// Gets the rule that has the given nonterminal as a head.
  pub fn try_get_rule<'a>(&'a self, nt: &E::NonTerm) -> Option<Rule<'a, E>> {
    self.rule_set.get(nt).map(|rule| Rule::new(self, rule))
  }

  /// Gets the rule that has the given nonterminal as a head.
  pub fn get_rule<'a>(&'a self, nt: &E::NonTerm) -> Rule<'a, E> {
    self
      .try_get_rule(nt)
      .expect("An NT rule exists in the grammar.")
  }

  /// Gets an iterator over all productions in the grammar.
  pub fn prods<'a>(&'a self) -> impl Iterator<Item = Prod<'a, E>> {
    self.rules().flat_map(move |rule| rule.prods())
  }

  pub fn to_pretty(&self) -> String {
    let arena = pretty::Arena::new();
    format!("{}", self.to_doc(&arena).into_doc().pretty(80))
  }

  fn get_elements(&self) -> impl Iterator<Item = &Elem<E>> {
    self
      .rule_set
      .values()
      .flat_map(|r| &r.prods)
      .flat_map(|p| p.elements_iter())
  }

  fn get_terminals(&self) -> impl Iterator<Item = &E::Term> {
    self.get_elements().filter_map(|e| e.as_term())
  }

  fn get_nonterminals(&self) -> impl Iterator<Item = &E::NonTerm> {
    self.get_elements().filter_map(|e| e.as_nonterm())
  }

  fn nonterminals_without_rules(&self) -> BTreeSet<&E::NonTerm> {
    self
      .get_nonterminals()
      .filter(move |nt| !self.rule_set.contains_key(nt))
      .collect()
  }

  fn rules_without_prods<'a>(&'a self) -> BTreeSet<&'a E::NonTerm> {
    let rules = self.rules();
    let prodless_rules = rules
      .into_iter()
      .filter(|r| matches!(r.prods().next(), None));
    let head_iter = prodless_rules.map(|r| r.head());
    head_iter.collect()
  }

  fn reachable_nonterms(&self) -> BTreeSet<&E::NonTerm> {
    breadth_first_search(std::iter::once(&self.start_symbol), |nt| {
      self
        .get_rule(nt)
        .prods()
        .flat_map(|p| p.elements())
        .filter_map(|e| e.as_nonterm())
        .collect::<BTreeSet<_>>()
    })
  }

  fn unreachable_nonterms(&self) -> BTreeSet<&E::NonTerm> {
    let reachable_nonterms = self.reachable_nonterms();
    self
      .get_nonterminals()
      .filter(|nt| !reachable_nonterms.contains(nt))
      .collect()
  }
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub struct GrammarErrors<E: ElemTypes> {
  unreachable_nonterms: BTreeSet<E::NonTerm>,
  nonterms_without_rules: BTreeSet<E::NonTerm>,
  rules_without_prods: BTreeSet<E::NonTerm>,
}

impl<E: ElemTypes> GrammarErrors<E> {
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

impl<E: ElemTypes> Grammar<E> {
  fn check_grammar(&self) -> Result<(), GrammarErrors<E>> {
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

impl<E: ElemTypes> ToDoc for Grammar<E> {
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
pub struct Rule<'a, E: ElemTypes> {
  grammar: ParentRef<'a, Grammar<E>>,
  rule: RefCompare<'a, RuleInner<E>>,
}

impl<'a, E: ElemTypes> Rule<'a, E> {
  fn new(grammar: &'a Grammar<E>, rule: &'a RuleInner<E>) -> Self {
    Rule {
      grammar: ParentRef::new(grammar),
      rule: RefCompare::new(rule),
    }
  }
  /// Returns the head nonterminal.
  pub fn head(&self) -> &'a E::NonTerm {
    &self.rule.head
  }

  /// Returns an iterator over the productions of this rule.
  pub fn prods(&self) -> impl Iterator<Item = Prod<'a, E>> {
    let prods = &self.rule.prods;
    prods.into_iter().map({
      let grammar = *self.grammar;
      let head = &self.rule.head;
      move |prod| {
        Prod::new(
          grammar,
          head,
          prod,
          grammar.action_map.get(&prod.action_key).unwrap(),
        )
      }
    })
  }
}

impl<'a, E: ElemTypes> std::fmt::Debug for Rule<'a, E> {
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
#[derive(Derivative)]
#[derivative(
  Copy(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = "")
)]
pub struct Prod<'a, E: ElemTypes> {
  grammar: ParentRef<'a, Grammar<E>>,
  head: &'a E::NonTerm,
  prod: RefCompare<'a, ProdInner<E>>,
  action_value: NoCompare<&'a E::ActionValue>,
}

impl<'a, E: ElemTypes> Prod<'a, E> {
  fn new(
    grammar: &'a Grammar<E>,
    head: &'a E::NonTerm,
    prod: &'a ProdInner<E>,
    action_value: &'a E::ActionValue,
  ) -> Self {
    Prod {
      grammar: ParentRef::new(grammar),
      head,
      prod: RefCompare::new(prod),
      action_value: NoCompare::new(action_value),
    }
  }

  /// Returns the head of this production.
  pub fn head(&self) -> &'a E::NonTerm {
    self.head
  }

  /// Returns the elements of this production, including any identifiers of the production.
  pub fn prod_elements(&self) -> &'a Vec<ProdElement<E>> {
    &self.prod.elements
  }

  /// Returns the number of elements in this production.
  pub fn num_elements(&self) -> usize {
    self.prod_elements().len()
  }
  
  /// Returns an iterator over the elements of this production, without any identifiers.
  pub fn elements(&self) -> impl Iterator<Item = &'a Elem<E>> + Clone {
    self.prod.elements_iter()
  }

  /// Returns the prod element at a given index. Panics on out-of-bounds access.
  pub fn prod_element_at(&self, index: usize) -> Option<&'a ProdElement<E>> {
    self.prod.elements.get(index)
  }

  /// Returns the element at a given index. Panics on out-of-bounds access.
  pub fn element_at(&self, index: usize) -> Option<&'a Elem<E>> {
    self.prod.element_at(index)
  }

  /// Returns the action key of this production.
  pub fn action_key(&self) -> &'a E::ActionKey {
    self.prod.action_key()
  }

  /// Returns the action value of this production.
  pub fn action_value(&self) -> &'a E::ActionValue {
    *self.action_value
  }

  /// Returns the `ProdKey` of this production.
  pub fn prod_key(&self) -> ProdKey<E> {
    ProdKey {
      head: self.head().clone(),
      action_key: self.action_key().clone(),
    }
  }

  /// Returns a `Some` containing the first element of this production, or None if
  /// the production is empty.
  pub fn first_elem(&self) -> Option<&'a Elem<E>> {
    self.prod.elements.first().map(|pe| pe.elem())
  }
}

impl<'a, E: ElemTypes> Clone for Prod<'a, E> {
  fn clone(&self) -> Self {
    Prod {
      grammar: self.grammar.clone(),
      head: self.head,
      prod: self.prod.clone(),
      action_value: self.action_value.clone(),
    }
  }
}

impl<'a, E: ElemTypes> std::fmt::Debug for Prod<'a, E> {
  fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
    let mut dbg_struct = fmt.debug_struct("Prod");
    dbg_struct.field("head", self.head());
    dbg_struct.field("elems", &self.prod_elements());
    dbg_struct.finish()
  }
}
