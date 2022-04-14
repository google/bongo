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

use super::{
  Elem, Grammar, GrammarErrors, Name, ProdElement, ProdInner, RuleInner,
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

pub struct ProductionBuilder<T, NT, AK, AV> {
  action_key: AK,
  action_value: AV,
  elems: Vec<ProdElement<T, NT>>,
}

impl<T, NT, AK, AV> ProductionBuilder<T, NT, AK, AV> {
  fn new(action_key: AK, action_value: AV) -> Self {
    ProductionBuilder {
      action_key,
      action_value,
      elems: Vec::new(),
    }
  }

  fn build(self) -> ProdInner<T, NT, AK, AV> {
    let ProductionBuilder {
      action_key,
      action_value,
      elems,
    } = self;
    ProdInner::new(action_key, action_value, elems)
  }

  pub fn add_term(&mut self, term: impl BuilderInto<T>) -> &mut Self {
    self
      .elems
      .push(ProdElement::new_empty(Elem::Term(term.builder_into())));
    self
  }

  pub fn add_named_term(
    &mut self,
    name: impl BuilderInto<Name>,
    term: impl BuilderInto<T>,
  ) -> &mut Self {
    self.elems.push(ProdElement::new_with_name(
      name.builder_into(),
      Elem::Term(term.builder_into()),
    ));
    self
  }

  pub fn add_nonterm(&mut self, nonterm: impl BuilderInto<NT>) -> &mut Self {
    self.elems.push(ProdElement::new_empty(Elem::NonTerm(
      nonterm.builder_into(),
    )));
    self
  }

  pub fn add_named_nonterm(
    &mut self,
    name: impl BuilderInto<Name>,
    nonterm: impl BuilderInto<NT>,
  ) -> &mut Self {
    self.elems.push(ProdElement::new_with_name(
      name.builder_into(),
      Elem::NonTerm(nonterm.builder_into()),
    ));
    self
  }
}

// ----------------

pub struct RuleBuilder<T, NT, AK, AV> {
  head: NT,
  prods: Vec<ProdInner<T, NT, AK, AV>>,
}

impl<T, NT, AK, AV> RuleBuilder<T, NT, AK, AV>
where
  AK: Ord + Clone,
{
  fn new(head: NT) -> Self {
    RuleBuilder {
      head,
      prods: Vec::new(),
    }
  }

  fn build(self) -> RuleInner<T, NT, AK, AV> {
    let RuleBuilder { head, prods, .. } = self;
    RuleInner::new(head, prods)
  }

  pub fn add_prod(
    &mut self,
    action_key: impl BuilderInto<AK>,
    action_value: impl BuilderInto<AV>,
    build_fn: impl FnOnce(&mut ProductionBuilder<T, NT, AK, AV>),
  ) -> &mut Self {
    let action_key = action_key.builder_into();
    let action_value = action_value.builder_into();
    let mut builder = ProductionBuilder::new(action_key, action_value);
    build_fn(&mut builder);
    self.prods.push(builder.build());
    self
  }

  pub fn add_prod_with_elems(
    &mut self,
    action_key: impl BuilderInto<AK>,
    action_value: impl BuilderInto<AV>,
    elems: impl BuilderInto<Vec<ProdElement<T, NT>>>,
  ) -> &mut Self {
    let action_key = action_key.builder_into();
    let action_value = action_value.builder_into();
    self.prods.push(ProdInner {
      action_key,
      action_value,
      elements: elems.builder_into(),
    });
    self
  }
}

// ----------------

pub struct GrammarBuilder<T, NT, AK, AV> {
  start: NT,
  rules: Vec<RuleInner<T, NT, AK, AV>>,
}

impl<T, NT, AK, AV> GrammarBuilder<T, NT, AK, AV>
where
  NT: Ord + Clone,
  AK: Ord + Clone,
{
  fn new(start: NT) -> Self {
    GrammarBuilder {
      start,
      rules: Vec::new(),
    }
  }

  fn build(self) -> Result<Grammar<T, NT, AK, AV>, GrammarErrors<NT>> {
    let GrammarBuilder { start, rules } = self;
    Grammar::new(start, rules)
  }

  pub fn add_rule<F>(
    &mut self,
    head: impl BuilderInto<NT>,
    build_fn: F,
  ) -> &mut Self
  where
    F: FnOnce(&mut RuleBuilder<T, NT, AK, AV>),
  {
    let mut rule_builder = RuleBuilder::new(head.builder_into());
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
/// # use bongo::grammar::{Terminal, NonTerminal, Grammar};
/// # use bongo::utils::Name;
/// let t_a = Terminal::new("A");
/// let nt_x = NonTerminal::new("x");
/// let g: Grammar<Terminal, NonTerminal, Name, ()> =
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
pub fn build<T, NT, AK, AV>(
  start: impl BuilderInto<NT>,
  build_fn: impl FnOnce(&mut GrammarBuilder<T, NT, AK, AV>),
) -> Result<Grammar<T, NT, AK, AV>, GrammarErrors<NT>>
where
  NT: Ord + Clone,
  AK: Ord + Clone,
{
  let mut builder = GrammarBuilder::new(start.builder_into());
  build_fn(&mut builder);
  builder.build()
}
