use super::{
  Element, ElementTypes, Grammar, Name, Production, ProductionElement, Rule,
};

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

  pub fn add_term(&mut self, term: E::Term) -> &mut Self {
    self
      .elems
      .push(ProductionElement::new_empty(Element::Term(term)));
    self
  }

  pub fn add_nonterm(&mut self, nonterm: E::NonTerm) -> &mut Self {
    self
      .elems
      .push(ProductionElement::new_empty(Element::NonTerm(nonterm)));
    self
  }

  pub fn add_named_nonterm(
    &mut self,
    name: Name,
    nonterm: E::NonTerm,
  ) -> &mut Self {
    self
      .elems
      .push(ProductionElement::new(name, Element::NonTerm(nonterm)));
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
    action: E::Action,
    build_fn: impl FnOnce(&mut ProductionBuilder<E>),
  ) -> &mut Self {
    let mut builder = ProductionBuilder::new(action);
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

  pub fn add_rule<F>(&mut self, head: E::NonTerm, build_fn: F) -> &mut Self
  where
    F: FnOnce(&mut RuleBuilder<E>),
  {
    let mut rule_builder = RuleBuilder::new(head);
    build_fn(&mut rule_builder);
    self.rules.push(rule_builder.build());
    self
  }
}

pub fn build<E>(
  start: E::NonTerm,
  build_fn: impl FnOnce(&mut GrammarBuilder<E>),
) -> Grammar<E>
where
  E: ElementTypes,
{
  let mut builder = GrammarBuilder::new(start);
  build_fn(&mut builder);
  builder.build()
}
