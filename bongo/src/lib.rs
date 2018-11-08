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

mod pdisplay;
mod states;

use codefmt::Layout;
use crate::pdisplay::LayoutDisplay;
use std::collections::BTreeSet;

/// A refcounted name type, used to avoid duplicating common string values
/// throughout an AST.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Name(std::rc::Rc<String>);

impl Name {
  /// Creates a new Name containing the given string.
  pub fn new(s: &(impl AsRef<str> + ?Sized)) -> Self {
    Name(std::rc::Rc::new(s.as_ref().to_string()))
  }

  /// Returns a reference to the internal ref.
  fn str(&self) -> &str {
    &**self.0
  }

  /// Returns a mutable reference to a string to modify this name. Will not
  /// alter any other names.
  fn make_mut(&mut self) -> &mut String {
    std::rc::Rc::make_mut(&mut self.0)
  }

  fn layout(&self) -> codefmt::Layout {
    Layout::text(self.str())
  }
}

impl AsRef<str> for Name {
  fn as_ref(&self) -> &str {
    return self.str();
  }
}

/// A terminal element.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Terminal(Name);

impl Terminal {
  pub fn new(s: &str) -> Self {
    Terminal(Name::new(s))
  }
}

impl LayoutDisplay for Terminal {
  fn disp(&self) -> codefmt::Layout {
    let name = self.0.str();
    Layout::text(name)
  }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct NonTerminal(Name);

impl NonTerminal {
  pub fn new(s: &str) -> Self {
    NonTerminal(Name::new(s))
  }
}

impl LayoutDisplay for NonTerminal {
  fn disp(&self) -> codefmt::Layout {
    Layout::juxtapose(&[
      Layout::text("<"),
      Layout::text(self.0.str()),
      Layout::text(">"),
    ])
  }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Element {
  Term(Terminal),
  NonTerm(NonTerminal),
}

impl LayoutDisplay for Element {
  fn disp(&self) -> codefmt::Layout {
    match self {
      Element::Term(t) => t.disp(),
      Element::NonTerm(nt) => nt.disp(),
    }
  }
}

impl From<Terminal> for Element {
  fn from(t: Terminal) -> Element {
    Element::Term(t)
  }
}

impl From<NonTerminal> for Element {
  fn from(nt: NonTerminal) -> Element {
    Element::NonTerm(nt)
  }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct ProductionElement {
  identifier: Option<Name>,
  element: Element,
}

impl LayoutDisplay for ProductionElement {
  fn disp(&self) -> codefmt::Layout {
    match &self.identifier {
      Some(name) => Layout::juxtapose(&[
        name.layout(),
        Layout::text(": "),
        self.element.disp(),
      ]),
      None => self.element.disp(),
    }
  }
}

impl From<Element> for ProductionElement {
  fn from(e: Element) -> ProductionElement {
    ProductionElement {
      identifier: None,
      element: e,
    }
  }
}

impl From<Terminal> for ProductionElement {
  fn from(t: Terminal) -> ProductionElement {
    let e: Element = t.into();
    e.into()
  }
}

impl From<NonTerminal> for ProductionElement {
  fn from(nt: NonTerminal) -> ProductionElement {
    let e: Element = nt.into();
    e.into()
  }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Production {
  action_name: Name,
  elements: Vec<ProductionElement>,
}

impl Production {
  pub fn new(name: &str, elements: Vec<ProductionElement>) -> Production {
    Production {
      action_name: Name::new(name),
      elements: elements,
    }
  }
}

impl LayoutDisplay for Production {
  fn disp(&self) -> Layout {
    let elements =
      Layout::wrap(self.elements.iter().map(|x| x.disp()).collect::<Vec<_>>());
    Layout::juxtapose(&[
      elements,
      Layout::text(" => "),
      self.action_name.layout(),
    ])
  }
}

#[derive(Clone)]
pub struct ProductionSet(Vec<Production>);

impl ProductionSet {
  pub fn new(prods: Vec<Production>) -> Self {
    ProductionSet(prods)
  }
}

impl LayoutDisplay for ProductionSet {
  fn disp(&self) -> Layout {
    let prod_layouts: Vec<_> = self.0.iter().map(|prod| prod.disp()).collect();
    Layout::stack(prod_layouts)
  }
}

#[derive(Clone)]
pub struct Grammar {
  start_symbol: NonTerminal,
  rule_set: std::collections::BTreeMap<NonTerminal, ProductionSet>,
  nullable_cache: std::cell::RefCell<Option<BTreeSet<NonTerminal>>>,
}

impl Grammar {
  pub fn new(
    start: NonTerminal,
    rule_set: std::collections::BTreeMap<NonTerminal, ProductionSet>,
  ) -> Self {
    Grammar {
      start_symbol: start,
      rule_set: rule_set,
      nullable_cache: std::cell::RefCell::new(None),
    }
  }

  pub fn is_nullable(&self, nt: &NonTerminal) -> bool {
    let mut borrow = self.nullable_cache.borrow_mut();
    if borrow.is_none() {
      let nullables = fixed_point(BTreeSet::new(), |nullables| {
        is_nullable_fp(self, nullables)
      });
      *borrow = Some(nullables);
    }
    borrow.as_ref().unwrap().contains(nt)
  }
}

impl LayoutDisplay for Grammar {
  fn disp(&self) -> Layout {
    let mut stack = Vec::new();
    for (k, v) in &self.rule_set {
      let name_layout = if &self.start_symbol == k {
        Layout::juxtapose(&[Layout::text("*"), k.disp()])
      } else {
        k.disp()
      };

      stack.push(Layout::juxtapose(&[name_layout, Layout::text(":")]));
      stack.push(Layout::juxtapose(&[Layout::text("  "), v.disp()]));
    }
    Layout::stack(stack)
  }
}

fn fixed_point<T: Eq>(start: T, mut apply: impl FnMut(&T) -> T) -> T {
  let mut curr = start;
  loop {
    let next = apply(&curr);
    if next == curr {
      break curr;
    }
    curr = next;
  }
}

fn is_prod_nullable(
  nullables: &BTreeSet<NonTerminal>,
  prod: &Production,
) -> bool {
  for elem in &prod.elements {
    match &elem.element {
      Element::Term(_) => return false,
      Element::NonTerm(nt) => {
        if !nullables.contains(nt) {
          return false;
        }
      }
    }
  }
  true
}

fn are_any_prods_nullable(
  nullables: &BTreeSet<NonTerminal>,
  prod_set: &ProductionSet,
) -> bool {
  for prod in &prod_set.0 {
    if is_prod_nullable(nullables, prod) {
      return true;
    }
  }
  false
}

fn is_nullable_fp(
  grammar: &Grammar,
  prev_nullables: &BTreeSet<NonTerminal>,
) -> BTreeSet<NonTerminal> {
  let mut curr_nullables = prev_nullables.clone();
  for (nt, prod_set) in &grammar.rule_set {
    if are_any_prods_nullable(prev_nullables, prod_set) {
      curr_nullables.insert(nt.clone());
    }
  }
  curr_nullables
}

/// A state of a production within a parse state.
///
/// A production state keeps track of a particular production, the nonterminal
/// to which the production belongs, and an index into the production, which is
/// the current location of the parse state. For example:
///
/// ```
/// A => a <b> . c
/// ```
///
/// This indicates that the head is A, the production is a <b> c, and the
/// current location is just before the final c.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct ProductionState {
  head: NonTerminal,
  prod: Production,
  index: usize,
}

impl ProductionState {
  fn from_start(head: NonTerminal, prod: Production) -> Self {
    ProductionState {
      head,
      prod,
      index: 0,
    }
  }

  fn next(&self) -> Option<&ProductionElement> {
    if self.index < self.prod.elements.len() {
      Some(&self.prod.elements[self.index])
    } else {
      None
    }
  }

  fn advance(&self) -> Option<ProductionState> {
    if self.index < self.prod.elements.len() {
      let mut result = self.clone();
      result.index += 1;
      Some(result)
    } else {
      None
    }
  }

  /// Return Some(state) which is this state advanced if
  /// the next element type is elem.
  fn advance_if(&self, elem: Element) -> Option<ProductionState> {
    if self.next().map(|e| &e.element) == Some(&elem) {
      self.advance()
    } else {
      None
    }
  }
}

impl LayoutDisplay for ProductionState {
  fn disp(&self) -> Layout {
    let mut layouts = Vec::new();
    for elem in &self.prod.elements[..self.index] {
      layouts.push(elem.disp());
    }
    layouts.push(codefmt::Layout::text("."));
    for elem in &self.prod.elements[self.index..] {
      layouts.push(elem.disp())
    }
    let body = pdisplay::join_layout(layouts, codefmt::Layout::text(" "));
    codefmt::Layout::juxtapose(vec![
      self.head.disp(),
      codefmt::Layout::text(" => "),
      body,
    ])
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn test_grammar_print() {
    let t_a = Terminal::new("A");
    let nt_x = NonTerminal::new("x");

    let v1: Vec<ProductionElement> =
      vec![t_a.clone().into(), nt_x.clone().into(), t_a.into()];
    let v2: Vec<ProductionElement> = vec![];

    let prod_set = ProductionSet::new(vec![
      Production::new("Recursive", v1),
      Production::new("Empty", v2),
    ]);

    let mut rules = std::collections::BTreeMap::new();
    rules.insert(nt_x.clone(), prod_set);

    let g = Grammar::new(nt_x.clone(), rules);

    println!("{}", g.disp().layout(80));

    assert!(g.is_nullable(&nt_x));
  }
}
