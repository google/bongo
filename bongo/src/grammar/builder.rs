use super::{
  Element, ElementTypes, Grammar, Name, Production, ProductionElement, Rule,
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
  action: E::Action,
  elems: Vec<ProductionElement<E>>,
}

impl<E: ElementTypes> ProductionBuilder<E> {
  fn new(action: E::Action) -> Self {
    ProductionBuilder {
      action,
      elems: Vec::new(),
    }
  }

  fn build(self) -> Production<E> {
    let ProductionBuilder { action, elems } = self;
    Production::new(action, elems)
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

pub struct RuleBuilder<E: ElementTypes> {
  head: E::NonTerm,
  prods: Vec<Production<E>>,
}

impl<E: ElementTypes> RuleBuilder<E> {
  fn new(head: E::NonTerm) -> Self {
    RuleBuilder {
      head,
      prods: Vec::new(),
    }
  }

  fn build(self) -> Rule<E> {
    let RuleBuilder { head, prods } = self;
    Rule::new(head, prods)
  }

  pub fn add_prod(
    &mut self,
    action: impl BuilderInto<E::Action>,
    build_fn: impl FnOnce(&mut ProductionBuilder<E>),
  ) -> &mut Self {
    let mut builder = ProductionBuilder::new(action.builder_into());
    build_fn(&mut builder);
    self.prods.push(builder.build());
    self
  }
}

// ----------------

pub struct GrammarBuilder<E: ElementTypes> {
  start: E::NonTerm,
  rules: Vec<Rule<E>>,
}

impl<E: ElementTypes> GrammarBuilder<E> {
  fn new(start: E::NonTerm) -> Self {
    GrammarBuilder {
      start,
      rules: Vec::new(),
    }
  }

  fn build(self) -> Grammar<E> {
    let GrammarBuilder { start, rules } = self;
    Grammar::new(start, rules)
  }

  pub fn add_rule<F>(
    &mut self,
    head: impl BuilderInto<E::NonTerm>,
    build_fn: F,
  ) -> &mut Self
  where
    F: FnOnce(&mut RuleBuilder<E>),
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
/// # use bongo::grammar::{Terminal, NonTerminal, BaseElementTypes,
/// # Grammar};
/// # use bongo::utils::Name;
/// let t_a = Terminal::new("A");
/// let nt_x = NonTerminal::new("x");
/// let g: Grammar<BaseElementTypes> =
///   bongo::grammar::builder::build(&nt_x, |gb| {
///     gb.add_rule(&nt_x, |rb| {
///       rb.add_prod(Name::new("Recursive"), |pb| {
///         pb.add_term(&t_a).add_nonterm(&nt_x).add_term(&t_a);
///       })
///       .add_prod(Name::new("Empty"), |_pb| {});
///     });
///   });
/// ```
///
/// Note that arguments that take `E::Term`, `E::NonTerm`, or `E::Action` can
/// either take a non-reference value, or a cloneable reference value.
pub fn build<E>(
  start: impl BuilderInto<E::NonTerm>,
  build_fn: impl FnOnce(&mut GrammarBuilder<E>),
) -> Grammar<E>
where
  E: ElementTypes,
{
  let mut builder = GrammarBuilder::new(start.builder_into());
  build_fn(&mut builder);
  builder.build()
}
