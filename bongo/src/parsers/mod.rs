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

use crate::ElementTypes;

mod earley;

pub trait TokenStream<T, V>: Clone {
  fn next(&self) -> Option<(T, V, Self)>;
}

pub enum ParseTree<E: ElementTypes, LeafValue> {
  Term(E::Term, LeafValue),
  NonTerminal(E::NonTerm, E::ActionKey, Vec<ParseTree<E, LeafValue>>),
}

pub trait Parser<E: ElementTypes, Leaf> {
  fn parse<S: TokenStream<E, Leaf>>(
    &self,
    stream: &S,
  ) -> Result<ParseTree<E, Leaf>, Box<dyn std::error::Error>>;
}
