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

use codefmt::Layout;
use crate::pdisplay::LayoutDisplay;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};

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

  pub fn prod_elements(&self) -> &Vec<ProductionElement> {
    &self.elements
  }

  pub fn elements_iter(&self) -> impl Iterator<Item = &Element> {
    self.elements.iter().map(|prod_elem| &prod_elem.element)
  }

  pub fn element_at(&self, index: usize) -> Option<&Element> {
    self.elements.get(index).map(|prod_elem| &prod_elem.element)
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

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Rule {
  head: NonTerminal,
  prods: Vec<Production>,
}

impl Rule {
  pub fn new(head: NonTerminal, prods: Vec<Production>) -> Self {
    Rule { head, prods }
  }

  pub fn head(&self) -> &NonTerminal {
    &self.head
  }

  pub fn prods(&self) -> &Vec<Production> {
    &self.prods
  }
}

impl PartialOrd for Rule {
  fn partial_cmp(&self, other: &Rule) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for Rule {
  fn cmp(&self, other: &Rule) -> Ordering {
    self
      .head
      .cmp(&other.head)
      .then_with(|| self.prods.cmp(&other.prods))
  }
}

impl LayoutDisplay for Rule {
  fn disp(&self) -> Layout {
    let prod_layouts: Vec<_> =
      self.prods.iter().map(|prod| prod.disp()).collect();
    Layout::stack(prod_layouts)
  }
}

#[derive(Clone)]
pub struct Grammar {
  start_symbol: NonTerminal,
  rule_set: BTreeMap<NonTerminal, Rule>,
}

impl Grammar {
  pub fn new(start: NonTerminal, rule_set: impl IntoIterator<Item = Rule>) -> Self {
    Grammar {
      start_symbol: start,
      rule_set: rule_set.into_iter().map(|r| (r.head().clone(), r)).collect(),
    }
  }

  pub fn rule_set(&self) -> &BTreeMap<NonTerminal, Rule> {
    &self.rule_set
  }

  pub fn get_rule(&self, nt: &NonTerminal) -> Option<&Rule> {
    self.rule_set.get(nt)
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
