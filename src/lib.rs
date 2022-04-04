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

//! Bongo is a library for generating Marpa parsers, and executing them on inputs.

#![allow(dead_code)]

#[macro_use]
extern crate derivative;

pub mod grammar;
pub mod parsers;
pub mod start_grammar;
pub mod state;
pub mod utils;

#[cfg(test)]
mod tests {
  use crate::grammar::build;
  use crate::grammar::{
    passes::{nullable::Nullable, PassContext},
    BaseElementTypes, Grammar, NonTerminal, Terminal,
  };
  use crate::utils::Name;

  fn base_grammar() -> Grammar<BaseElementTypes> {
    let t_a = Terminal::new("A");
    let nt_x = NonTerminal::new("x");

    build(&nt_x, |gb| {
      gb.add_rule(&nt_x, |rb| {
        rb.add_prod(Name::new("Recursive"), (), |pb| {
          pb.add_term(&t_a).add_nonterm(&nt_x).add_term(&t_a);
        })
        .add_prod(Name::new("Empty"), (), |_pb| {});
      });
    })
    .unwrap()
  }

  fn create_arithmetic_grammar() -> Grammar<BaseElementTypes> {
    let t_add = Terminal::new("+");
    let t_sub = Terminal::new("-");
    let t_mul = Terminal::new("*");
    let t_div = Terminal::new("/");
    let t_lparen = Terminal::new("(");
    let t_rparen = Terminal::new(")");
    let t_num = Terminal::new("num");

    let nt_expr = NonTerminal::new("expr");

    build(&nt_expr, |gb| {
      gb.add_rule(&nt_expr, |rb| {
        rb.add_prod(Name::new("Number"), (), |pb| {
          pb.add_term(&t_num);
        })
        .add_prod(Name::new("Paren"), (), |pb| {
          pb.add_term(&t_lparen)
            .add_nonterm(&nt_expr)
            .add_term(&t_rparen);
        });

        let mut add_binop = |name, op| {
          rb.add_prod(name, (), |pb| {
            pb.add_nonterm(&nt_expr).add_term(op).add_nonterm(&nt_expr);
          });
        };

        add_binop("Add", &t_add);
        add_binop("Sub", &t_sub);
        add_binop("Mul", &t_mul);
        add_binop("Div", &t_div);
      });
    })
    .unwrap()
  }

  #[test]
  fn test_arithemtic_grammar() {
    let grammar = create_arithmetic_grammar();
    let pass_map = PassContext::new(&grammar);
    let nullable = pass_map.get_pass::<Nullable<_>>().unwrap();
    assert!(!nullable.is_nullable(grammar.start_nt()));
  }

  #[test]
  fn test_grammar_print() {
    let g = base_grammar();

    let pass_map = PassContext::new(&g);
    let nullable = pass_map.get_pass::<Nullable<_>>().unwrap();

    assert!(nullable.is_nullable(&NonTerminal::new("x")));
  }

  #[test]
  fn test_grammar_nullable() {
    let g = base_grammar();
    println!("{:#?}", g);
  }
}
