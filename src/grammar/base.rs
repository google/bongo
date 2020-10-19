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
mod start_grammar;

use {
  crate::utils::{breadth_first_search, Name},
  std::collections::{BTreeMap, BTreeSet},
};

pub use cmp_wrappers::{NoCompare, ParentRef, RefCompare};
pub use element_types::{
  BaseElementTypes, ElementTypes, NonTerminal, Terminal,
};

/// A single element (terminal or non-terminal).
#[derive(Derivative)]
#[derivative(
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = ""),
  Debug(bound = ""),
  PartialOrd = "feature_allow_slow_enum",
  Ord = "feature_allow_slow_enum"
)]
pub enum Element<E: ElementTypes> {
  Term(E::Term),
  NonTerm(E::NonTerm),
}

impl<E: ElementTypes> Element<E> {
  /// If this element is a terminal, returns a `Some` value containing a
  /// terminal datum. Returns `None` otherwise.
  pub fn as_term(&self) -> Option<&E::Term> {
    match self {
      Element::NonTerm(_) => None,
      Element::Term(t) => Some(t),
    }
  }

  /// Gets an element as a nonterm. Returns a `None` value otherwise.
  pub fn as_nonterm(&self) -> Option<&E::NonTerm> {
    match self {
      Element::NonTerm(nt) => Some(nt),
      Element::Term(_) => None,
    }
  }

  /// Clone this element into an element of another ElementTypes instance.
  /// The `E::Term` and `E::NonTerm` datum types must be the same as those in
  /// `E2`.
  pub fn clone_as_other<E2>(&self) -> Element<E2>
  where
    E2: ElementTypes<Term = E::Term, NonTerm = E::NonTerm>,
  {
    match self {
      Element::Term(t) => Element::Term(t.clone()),
      Element::NonTerm(nt) => Element::NonTerm(nt.clone()),
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
  Ord(bound = ""),
  Debug(bound = "")
)]
pub struct ProductionElement<E: ElementTypes> {
  identifier: Option<Name>,
  element: Element<E>,
}

impl<E: ElementTypes> ProductionElement<E> {
  pub fn new_with_name(name: Name, e: Element<E>) -> Self {
    ProductionElement {
      identifier: Some(name),
      element: e,
    }
  }

  pub fn new(name: Option<Name>, e: Element<E>) -> Self {
    ProductionElement {
      identifier: name,
      element: e,
    }
  }

  pub fn new_empty(e: Element<E>) -> Self {
    ProductionElement {
      identifier: None,
      element: e,
    }
  }

  pub fn id(&self) -> Option<&Name> {
    self.identifier.as_ref()
  }

  pub fn elem(&self) -> &Element<E> {
    &self.element
  }

  pub fn clone_as_other<E2>(&self) -> ProductionElement<E2>
  where
    E2: ElementTypes<Term = E::Term, NonTerm = E::NonTerm>,
  {
    ProductionElement {
      identifier: self.identifier.clone(),
      element: self.element.clone_as_other(),
    }
  }
}

impl<E: ElementTypes> From<Element<E>> for ProductionElement<E> {
  fn from(e: Element<E>) -> ProductionElement<E> {
    ProductionElement {
      identifier: None,
      element: e,
    }
  }
}

/// A production within a rule.
#[derive(Derivative)]
#[derivative(
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = ""),
  Debug(bound = "")
)]
struct ProductionInner<E: ElementTypes> {
  action_key: E::ActionKey,
  elements: Vec<ProductionElement<E>>,
}

impl<E: ElementTypes> ProductionInner<E> {
  fn new(
    action_key: E::ActionKey,
    elements: Vec<ProductionElement<E>>,
  ) -> Self {
    ProductionInner {
      action_key,
      elements,
    }
  }

  pub fn prod_elements(&self) -> &Vec<ProductionElement<E>> {
    &self.elements
  }

  pub fn elements_iter(&self) -> impl Iterator<Item = &Element<E>> {
    self.elements.iter().map(|prod_elem| &prod_elem.element)
  }

  pub fn element_at(&self, index: usize) -> Option<&Element<E>> {
    self.elements.get(index).map(|prod_elem| &prod_elem.element)
  }

  pub fn action_key(&self) -> &E::ActionKey {
    &self.action_key
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
pub struct ProdKey<E: ElementTypes> {
  head: E::NonTerm,
  action_key: E::ActionKey,
}

/// A concrete raw rule value as stored inside a Grammar struct.
#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
struct RuleInner<E: ElementTypes> {
  head: E::NonTerm,
  prods: Vec<ProductionInner<E>>,
}

impl<E: ElementTypes> RuleInner<E> {
  pub fn new(head: E::NonTerm, prods: Vec<ProductionInner<E>>) -> Self {
    RuleInner { head, prods }
  }

  pub fn head(&self) -> &E::NonTerm {
    &self.head
  }

  pub fn prods(&self) -> &Vec<ProductionInner<E>> {
    &self.prods
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
pub struct Grammar<E: ElementTypes> {
  start_symbol: E::NonTerm,
  rule_set: BTreeMap<E::NonTerm, RuleInner<E>>,
  action_map: BTreeMap<E::ActionKey, E::ActionValue>,
}

impl<E: ElementTypes> std::fmt::Debug for Grammar<E> {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    let mut dbg_struct = f.debug_struct("Grammar");
    dbg_struct.field("Terms", &self.get_terminals().collect::<Vec<_>>());
    dbg_struct.finish()
  }
}

impl<E: ElementTypes> Grammar<E> {
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
      .map(|(k, rule)| {
        (
          k,
          Rule {
            grammar: ParentRef::new(self),
            rule: RefCompare::new(rule),
          },
        )
      })
      .collect()
  }

  /// Gets the rule that has the given nonterminal as a head.
  pub fn get_rule<'a>(&'a self, nt: &E::NonTerm) -> Option<Rule<'a, E>> {
    self.rule_set.get(nt).map(|rule| Rule {
      grammar: ParentRef::new(self),
      rule: RefCompare::new(rule),
    })
  }

  /// Gets an iterator over all productions in the grammar.
  pub fn prods<'a>(&'a self) -> impl Iterator<Item = Prod<'a, E>> {
    self.rules().flat_map(|rule| rule.prods())
  }

  fn get_elements(&self) -> impl Iterator<Item = &Element<E>> {
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
    let prodless_rules = rules.into_iter().filter(|r| r.prods().is_empty());
    let head_iter = prodless_rules.map(|r| r.head());
    head_iter.collect()
  }

  fn reachable_nonterms(&self) -> BTreeSet<&E::NonTerm> {
    breadth_first_search(std::iter::once(&self.start_symbol), |nt| {
      match self.get_rule(nt) {
        Some(rule) => rule
          .prods()
          .iter()
          .flat_map(|p| p.elements_iter())
          .filter_map(|e| e.as_nonterm())
          .collect(),
        None => BTreeSet::new(),
      }
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
pub struct GrammarErrors<E: ElementTypes> {
  unreachable_nonterms: BTreeSet<E::NonTerm>,
  nonterms_without_rules: BTreeSet<E::NonTerm>,
  rules_without_prods: BTreeSet<E::NonTerm>,
}

impl<E: ElementTypes> GrammarErrors<E> {
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

impl<E: ElementTypes> Grammar<E> {
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

// ------------

#[derive(Derivative)]
#[derivative(
  Copy(bound = ""),
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = ""),
  Debug(bound = "")
)]
pub struct Rule<'a, E: ElementTypes> {
  grammar: ParentRef<'a, Grammar<E>>,
  rule: RefCompare<'a, RuleInner<E>>,
}

impl<'a, E: ElementTypes> Rule<'a, E> {
  pub fn head(&self) -> &'a E::NonTerm {
    &self.rule.head
  }

  pub fn prods(&self) -> Vec<Prod<'a, E>> {
    self
      .rule
      .prods
      .iter()
      .map(|prod| Prod {
        grammar: self.grammar,
        head: &self.rule.head,
        prod: RefCompare::new(prod),
        action_value: NoCompare::new(
          self.grammar.action_map.get(&prod.action_key).unwrap(),
        ),
      })
      .collect()
  }
}

// ------------

#[derive(Derivative)]
#[derivative(
  Copy(bound = ""),
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = ""),
  Debug(bound = "")
)]
pub struct Prod<'a, E: ElementTypes> {
  grammar: ParentRef<'a, Grammar<E>>,
  head: &'a E::NonTerm,
  prod: RefCompare<'a, ProductionInner<E>>,
  action_value: NoCompare<&'a E::ActionValue>,
}

impl<'a, E: ElementTypes> Prod<'a, E> {
  pub fn head(&self) -> &'a E::NonTerm {
    self.head
  }

  pub fn prod_elements(&self) -> &'a Vec<ProductionElement<E>> {
    &self.prod.elements
  }

  pub fn elements_iter(&self) -> impl Iterator<Item = &'a Element<E>> {
    self.prod.elements_iter()
  }

  pub fn prod_element_at(
    &self,
    index: usize,
  ) -> Option<&'a ProductionElement<E>> {
    self.prod.elements.get(index)
  }

  pub fn element_at(&self, index: usize) -> Option<&'a Element<E>> {
    self.prod.element_at(index)
  }

  pub fn action_key(&self) -> &'a E::ActionKey {
    self.prod.action_key()
  }

  pub fn action_value(&self) -> &'a E::ActionValue {
    *self.action_value
  }

  pub fn prod_key(&self) -> ProdKey<E> {
    ProdKey {
      head: self.head().clone(),
      action_key: self.action_key().clone(),
    }
  }
}
