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
