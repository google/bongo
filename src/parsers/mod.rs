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

use std::sync::Arc;

use crate::grammar::ElemTypes;

pub mod earley;
pub mod tree;
pub mod lalr;

#[derive(Clone, Copy)]
pub struct Token<K, T> {
  kind: K,
  value: T,
}

impl<K, T> Token<K, T> {
  pub fn new(kind: K, value: T) -> Self {
    Token { kind, value }
  }
}

pub trait TokenStream<K, T>: Clone {
  fn next(&self) -> Option<(Token<K, T>, Self)>;
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct VecTokenStream<K, T> {
  tokens: Arc<Vec<Token<K, T>>>,
  position: usize,
}

impl<K, T> VecTokenStream<K, T> {
  pub fn from_into_iter<C: IntoIterator<Item = Token<K, T>>>(
    collection: C,
  ) -> Self {
    VecTokenStream {
      tokens: Arc::new(collection.into_iter().collect()),
      position: 0,
    }
  }
}

impl<K, T> TokenStream<K, T> for VecTokenStream<K, T>
where
  K: Clone,
  T: Clone,
{
  fn next(&self) -> Option<(Token<K, T>, Self)> {
    if self.tokens.len() < self.position {
      Some((
        self.tokens[self.position].clone(),
        VecTokenStream {
          tokens: self.tokens.clone(),
          position: self.position + 1,
        },
      ))
    } else {
      assert_eq!(self.tokens.len(), self.position);
      None
    }
  }
}

pub trait Parser<E: ElemTypes, T> {
  fn parse<S: TokenStream<E::Term, T>>(
    &self,
    stream: S,
  ) -> anyhow::Result<tree::Node<E, T>>;
}
