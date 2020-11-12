use bongo::grammar::{
  build, BaseElementTypes, Element, Grammar, NonTerminal, ProductionElement,
};
use bongo::utils::Name;
use bongo::parsers::tree::TreeOwner;
use bongo::parsers::earley::parse;

fn main() {
  let a_nt = NonTerminal::new("A");
  let g: Grammar<BaseElementTypes> = build(&a_nt, |b| {
    b.add_rule(&a_nt, |br| {
      br.add_prod_with_elems(&Name::new("a_empty"), (), vec![])
        .add_prod_with_elems(
          &Name::new("a_recurse"),
          (),
          vec![ProductionElement::new_with_name(
            Name::new("recurse"),
            Element::NonTerm(a_nt.clone()),
          )],
        );
    });
  })
  .unwrap();

  println!("Grammar: {:#?}", g);

  let tree: TreeOwner<_, ()> = TreeOwner::new();

  parse(&g, &tree.handle(), vec![]);
}
