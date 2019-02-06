use std::collections::BTreeMap;

use codefmt::Layout;

pub fn fixed_point<T: Eq>(start: T, mut apply: impl FnMut(&T) -> T) -> T {
  let mut curr = start;
  loop {
    let next = apply(&curr);
    if next == curr {
      break curr;
    }
    curr = next;
  }
}

/// A refcounted name type, used to avoid duplicating common string values
/// throughout an AST.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Name(std::rc::Rc<String>);

impl Name {
  /// Creates a new Name containing the given string.
  pub fn new(s: &(impl AsRef<str> + ?Sized)) -> Self {
    Name(std::rc::Rc::new(s.as_ref().to_string()))
  }

  /// Returns a reference to the internal ref.
  pub fn str(&self) -> &str {
    &**self.0
  }

  /// Returns a mutable reference to a string to modify this name. Will not
  /// alter any other names.
  pub fn make_mut(&mut self) -> &mut String {
    std::rc::Rc::make_mut(&mut self.0)
  }

  pub fn layout(&self) -> codefmt::Layout {
    Layout::text(self.str())
  }
}

impl AsRef<str> for Name {
  fn as_ref(&self) -> &str {
    return self.str();
  }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum TreeValue<L, V> {
  Node(Box<TreeNode<L, V>>),
  Leaf(V),
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct TreeNode<L, V> {
  action_name: L,
  params: BTreeMap<Name, TreeValue<L, V>>,
}

impl<L: Ord, V> TreeNode<L, V> {
  pub fn from_action(action: L) -> Self {
    TreeNode {
      action_name: action,
      params: BTreeMap::new(),
    }
  }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum Void {}
