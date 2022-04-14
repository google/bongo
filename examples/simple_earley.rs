use bongo::grammar::{
  build, Elem, Grammar, NonTerminal, ProdElement, Terminal,
};
use bongo::parsers::earley::parse;
use bongo::parsers::{tree::TreeOwner, Token};
use bongo::start_grammar::wrap_grammar_with_start;
use bongo::utils::Name;

fn main() {
  let a_nt = NonTerminal::new("a");
  let lp_t = Terminal::new("LPAREN");
  let rp_t = Terminal::new("RPAREN");
  let v_t = Terminal::new("VALUE");
  let g: Grammar<Terminal, NonTerminal, Name, ()> = build(&a_nt, |b| {
    b.add_rule(&a_nt, |br| {
      br.add_prod_with_elems(
        &Name::new("value"),
        (),
        vec![ProdElement::new_with_name(
          Name::new("val"),
          Elem::Term(v_t.clone()),
        )],
      )
      .add_prod_with_elems(
        &Name::new("parens"),
        (),
        vec![
          ProdElement::new_empty(Elem::Term(lp_t.clone())),
          ProdElement::new_with_name(
            Name::new("contents"),
            Elem::NonTerm(a_nt.clone()),
          ),
          ProdElement::new_empty(Elem::Term(rp_t.clone())),
        ],
      );
    });
  })
  .unwrap();

  let g = wrap_grammar_with_start(g).unwrap();

  eprintln!("Grammar: {}", g.to_pretty());

  let tree: TreeOwner<_, _, &'static str> = TreeOwner::new();

  let node = parse(
    &g,
    &tree.handle(),
    vec![
      Token::new(lp_t.clone(), "("),
      Token::new(v_t.clone(), "Hello!"),
      Token::new(rp_t.clone(), ")"),
    ],
  )
  .unwrap();

  println!("{}", node.to_dot());
}
