use crate::grammar::builder::build;
use crate::grammar::{BaseElementTypes, Grammar, Name, NonTerminal, Terminal};

pub fn make_simple() -> Grammar<BaseElementTypes> {
  let start = NonTerminal::new("start");
  let a_nt = NonTerminal::new("a");
  let b_nt = NonTerminal::new("b");
  let a_t = Terminal::new("A");
  let b_t = Terminal::new("B");

  build(&start, |b| {
    b.add_rule(&start, |builder| {
      builder
        .add_prod(Name::new("a_prod"), |b| {
          b.add_named_nonterm(Name::new("value"), &a_nt);
        })
        .add_prod(Name::new("b_prod"), |b| {
          b.add_named_nonterm(Name::new("value"), &b_nt);
        });
    })
    .add_rule(&a_nt, |b| {
      b.add_prod(Name::new("a_term_prod"), |b| {
        b.add_term(&a_t);
      });
    })
    .add_rule(&b_nt, |b| {
      b.add_prod(Name::new("b_term_prod"), |b| {
        b.add_term(&b_t);
      });
    });
  })
}
