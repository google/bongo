// Copyright 2019 Google LLC
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

use {
  super::{
    Element, ElementTypes, Grammar, GrammarErrors, Name, ProductionElement,
    ProductionInner, RuleInner,
  },
  std::collections::BTreeMap,
};

/// A helper trait to allow builder methods to either take a type `T`, or a
/// reference to `T` if it is clonable.
pub trait BuilderInto<T> {
  /// Consumes self and produces a value of type `T`.
  fn builder_into(self) -> T;
}

impl<T> BuilderInto<T> for T {
  fn builder_into(self) -> T {
    self
  }
}

impl<'a, T> BuilderInto<T> for &'a T
where
  T: Clone,
{
  fn builder_into(self) -> T {
    self.clone()
  }
}

impl BuilderInto<Name> for &'_ str {
  fn builder_into(self) -> Name {
    Name::new(self)
  }
}

pub struct ProductionBuilder<E: ElementTypes> {
  action_key: E::ActionKey,
  elems: Vec<ProductionElement<E>>,
}

impl<E: ElementTypes> ProductionBuilder<E> {
  fn new(action_key: E::ActionKey) -> Self {
    ProductionBuilder {
      action_key,
      elems: Vec::new(),
    }
  }

  fn build(self) -> ProductionInner<E> {
    let ProductionBuilder { action_key, elems } = self;
    ProductionInner::new(action_key, elems)
  }

  pub fn add_term(&mut self, term: impl BuilderInto<E::Term>) -> &mut Self {
    self.elems.push(ProductionElement::new_empty(Element::Term(
      term.builder_into(),
    )));
    self
  }

  pub fn add_named_term(
    &mut self,
    name: impl BuilderInto<Name>,
    term: impl BuilderInto<E::Term>,
  ) -> &mut Self {
    self.elems.push(ProductionElement::new_with_name(
      name.builder_into(),
      Element::Term(term.builder_into()),
    ));
    self
  }

  pub fn add_nonterm(
    &mut self,
    nonterm: impl BuilderInto<E::NonTerm>,
  ) -> &mut Self {
    self
      .elems
      .push(ProductionElement::new_empty(Element::NonTerm(
        nonterm.builder_into(),
      )));
    self
  }

  pub fn add_named_nonterm(
    &mut self,
    name: impl BuilderInto<Name>,
    nonterm: impl BuilderInto<E::NonTerm>,
  ) -> &mut Self {
    self.elems.push(ProductionElement::new_with_name(
      name.builder_into(),
      Element::NonTerm(nonterm.builder_into()),
    ));
    self
  }
}

// ----------------

pub struct RuleBuilder<'a, E: ElementTypes> {
  action_map: &'a mut BTreeMap<E::ActionKey, E::ActionValue>,
  head: E::NonTerm,
  prods: Vec<ProductionInner<E>>,
}

impl<'a, E: ElementTypes> RuleBuilder<'a, E> {
  fn new(
    action_map: &'a mut BTreeMap<E::ActionKey, E::ActionValue>,
    head: E::NonTerm,
  ) -> Self {
    RuleBuilder {
      action_map,
      head,
      prods: Vec::new(),
    }
  }

  fn build(self) -> RuleInner<E> {
    let RuleBuilder { head, prods, .. } = self;
    RuleInner::new(head, prods)
  }

  pub fn add_prod(
    &mut self,
    action_key: impl BuilderInto<E::ActionKey>,
    action_value: impl BuilderInto<E::ActionValue>,
    build_fn: impl FnOnce(&mut ProductionBuilder<E>),
  ) -> &mut Self {
    let action_key = action_key.builder_into();
    self
      .action_map
      .insert(action_key.clone(), action_value.builder_into());
    let mut builder = ProductionBuilder::new(action_key);
    build_fn(&mut builder);
    self.prods.push(builder.build());
    self
  }

  pub fn add_prod_with_elems(
    &mut self,
    action_key: impl BuilderInto<E::ActionKey>,
    action_value: impl BuilderInto<E::ActionValue>,
    elems: impl BuilderInto<Vec<ProductionElement<E>>>,
  ) -> &mut Self {
    let action_key = action_key.builder_into();
    self
      .action_map
      .insert(action_key.clone(), action_value.builder_into());
    self.prods.push(ProductionInner {
      action_key,
      elements: elems.builder_into(),
    });
    self
  }
}

// ----------------

pub struct GrammarBuilder<E: ElementTypes> {
  start: E::NonTerm,
  rules: Vec<RuleInner<E>>,
  action_map: BTreeMap<E::ActionKey, E::ActionValue>,
}

impl<E: ElementTypes> GrammarBuilder<E> {
  fn new(start: E::NonTerm) -> Self {
    GrammarBuilder {
      start,
      rules: Vec::new(),
      action_map: BTreeMap::new(),
    }
  }

  fn build(self) -> Result<Grammar<E>, GrammarErrors<E>> {
    let GrammarBuilder {
      start,
      rules,
      action_map,
    } = self;
    Grammar::new(start, rules, action_map)
  }

  pub fn add_rule<F>(
    &mut self,
    head: impl BuilderInto<E::NonTerm>,
    build_fn: F,
  ) -> &mut Self
  where
    F: FnOnce(&mut RuleBuilder<E>),
  {
    let mut rule_builder =
      RuleBuilder::new(&mut self.action_map, head.builder_into());
    build_fn(&mut rule_builder);
    self.rules.push(rule_builder.build());
    self
  }
}

/// Builds a grammar using a builder function.
///
/// Example:
///
/// ```rust
/// # use bongo::grammar::{Terminal, NonTerminal, BaseElementTypes,
/// # Grammar};
/// # use bongo::utils::Name;
/// let t_a = Terminal::new("A");
/// let nt_x = NonTerminal::new("x");
/// let g: Grammar<BaseElementTypes> =
///   bongo::grammar::build(&nt_x, |gb| {
///     gb.add_rule(&nt_x, |rb| {
///       rb.add_prod(Name::new("Recursive"), (), |pb| {
///         pb.add_term(&t_a).add_nonterm(&nt_x).add_term(&t_a);
///       })
///       .add_prod(Name::new("Empty"), (), |_pb| {});
///     });
///   }).unwrap();
/// ```
///
/// Note that arguments that take `E::Term`, `E::NonTerm`, or `E::Action` can
/// either take a non-reference value, or a cloneable reference value.
pub fn build<E>(
  start: impl BuilderInto<E::NonTerm>,
  build_fn: impl FnOnce(&mut GrammarBuilder<E>),
) -> Result<Grammar<E>, GrammarErrors<E>>
where
  E: ElementTypes,
{
  let mut builder = GrammarBuilder::new(start.builder_into());
  build_fn(&mut builder);
  builder.build()
}
