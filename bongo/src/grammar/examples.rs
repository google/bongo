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

use crate::grammar::build;
use crate::grammar::{BaseElementTypes, Grammar, NonTerminal, Terminal};

pub fn make_simple() -> Grammar<BaseElementTypes> {
  let start = NonTerminal::new("start");
  let a_nt = NonTerminal::new("a");
  let b_nt = NonTerminal::new("b");
  let a_t = Terminal::new("A");
  let b_t = Terminal::new("B");

  build(&start, |b| {
    b.add_rule(&start, |builder| {
      builder
        .add_prod("a_prod", (), |b| {
          b.add_named_nonterm("value", &a_nt);
        })
        .add_prod("b_prod", (), |b| {
          b.add_named_nonterm("value", &b_nt);
        });
    })
    .add_rule(&a_nt, |b| {
      b.add_prod("a_term_prod", (), |b| {
        b.add_term(&a_t);
      });
    })
    .add_rule(&b_nt, |b| {
      b.add_prod("b_term_prod", (), |b| {
        b.add_term(&b_t);
      });
    }); 
  }).unwrap()
}

pub fn make_simple_nullable() -> Grammar<BaseElementTypes> {
  let start = NonTerminal::new("start");
  let a_nt = NonTerminal::new("a");
  let b_nt = NonTerminal::new("b");
  let c_nt = NonTerminal::new("c");

  build(&start, |b| {
    b.add_rule(&start, |b| {
      b.add_prod("start", (), |b| {
        b.add_named_nonterm("value", &c_nt);
      });
    })
    .add_rule(&a_nt, |b| {
      b.add_prod("a_value", (), |_| {
        // Empty list.
      });
    })
    .add_rule(&b_nt, |b| {
      b.add_prod("b_value", (), |_| {
        // Empty list.
      });
    })
    .add_rule(&c_nt, |b| {
      b.add_prod("c_value", (), |b| {
        b.add_named_nonterm("left", &a_nt)
          .add_named_nonterm("right", &b_nt);
      });
    });
  }).unwrap()
}

pub fn make_paren() -> Grammar<BaseElementTypes> {
  let start = NonTerminal::new("start");
  let expr = NonTerminal::new("expr");
  let expr_list = NonTerminal::new("expr_list");

  let lparen = Terminal::new("LPAREN");
  let rparen = Terminal::new("RPAREN");

  build(&start, |b| {
    b.add_rule(&start, |b| {
      b.add_prod("start", (), |b| {
        b.add_named_nonterm("expr", &expr);
      });
    })
    .add_rule(&expr, |b| {
      b.add_prod("paren_expr", (), |b| {
        b.add_term(&lparen)
          .add_named_nonterm("contents", &expr_list)
          .add_term(&rparen);
      });
    })
    .add_rule(&expr_list, |b| {
      b.add_prod("empty", (), |_| {
        // Empty list.
      })
      .add_prod("elem", (), |b| {
        b.add_named_nonterm("left", &expr)
          .add_named_nonterm("right", &expr_list);
      });
    });
  }).unwrap()
}

pub fn make_ambiguous_nullable() -> Grammar<BaseElementTypes> {
  let start = NonTerminal::new("start");
  let a_nt = NonTerminal::new("a");
  let b_nt = NonTerminal::new("b");
  let c_nt = NonTerminal::new("c");

  build(&start, |b| {
    b.add_rule(&start, |b| {
      b.add_prod("start", (), |b| {
        b.add_named_nonterm("value", &c_nt);
      });
    })
    .add_rule(&a_nt, |b| {
      b.add_prod("a_value", (), |_| {
        // Empty list.
      });
    })
    .add_rule(&b_nt, |b| {
      b.add_prod("b_value", (), |_| {
        // Empty list.
      });
    })
    .add_rule(&c_nt, |b| {
      b.add_prod("c_left", (), |b| {
        b.add_named_nonterm("value", &a_nt);
      }).add_prod("c_right", (), |b| {
        b.add_named_nonterm("value", &b_nt);
      });
    });
  }).unwrap()
}
