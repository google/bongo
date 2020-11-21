use bongo::grammar::{
  build, BaseElementTypes, Elem, Grammar, NonTerminal, ProdElement,
};
use bongo::utils::Name;
use bongo::parsers::tree::TreeOwner;
use bongo::parsers::earley::parse;
use bongo::start_grammar::wrap_grammar_with_start;

fn main() {
  let a_nt = NonTerminal::new("A");
  let g: Grammar<BaseElementTypes> = build(&a_nt, |b| {
    b.add_rule(&a_nt, |br| {
      br.add_prod_with_elems(&Name::new("a_empty"), (), vec![])
        .add_prod_with_elems(
          &Name::new("a_recurse"),
          (),
          vec![ProdElement::new_with_name(
            Name::new("recurse"),
            Elem::NonTerm(a_nt.clone()),
          )],
        );
    });
  })
  .unwrap();

  let g = wrap_grammar_with_start(g).unwrap();

  eprintln!("Grammar: {:#?}", g);

  let tree: TreeOwner<_, ()> = TreeOwner::new();

  let node = parse(&g, &tree.handle(), vec![]).unwrap();

  println!("{}", node.to_dot());
}
