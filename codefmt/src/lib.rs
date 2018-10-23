#![feature(rust_2018_preview, uniform_paths)]
//! Simple efficient composable primitives for pretty printing.
//!
//! The implementation is derived from the paper ["A New Approach to Optimal Code Formatting"][pp-paper]
//! by Phillip M. Yelland.
//!
//! [pp-paper]: https://static.googleusercontent.com/media/research.google.com/en//pubs/archive/44667.pdf

extern crate unicode_width;

mod knot_column;
mod knot_set;
mod linear_value;
mod resolved_layout;
mod shared_string;

use std::borrow::Borrow;
use std::collections as col;
use std::fmt;
use std::rc;

use shared_string::SharedString;

const OVERFLOW_COST: f32 = 100.0;
const NEWLINE_COST: f32 = 1.0;

/// The primary representation of pretty-printed text. Logically, it represents a set of
/// possible permissible layouts of text.
///
/// Layouts are constructed with four different primitives:
///
/// 1. Text
///
///    A literal string which is laid out as written. Its length is obtained using the
///    `unicode-width` package.
///
/// 2. Stack
///
///    Vertically stacks two layouts. The left margins of the two layouts will be
///    aligned.
///
/// 3. Juxtapose
///
///    Horizontally juxtapose two layouts. The left margin of the second layout
///    will be aligned with the end of the first layout's last line.
///
/// 4. Choice
///
///    Creates a choice between two or more layouts. The choice is made at layout
///    time based on the cost of each possible layout. See the `layout` method.
#[derive(Clone)]
pub struct Layout(rc::Rc<LayoutContents>);

impl Layout {
  fn with_contents(contents: LayoutContents) -> Layout {
    Layout(rc::Rc::new(contents))
  }

  fn contents(&self) -> &LayoutContents {
    &*self.0
  }

  /// Create a text layout value. This is a literal string with no layout variance.
  pub fn text(lit_text: impl Into<String>) -> Layout {
    Layout::with_contents(LayoutContents::Text(lit_text.into().into()))
  }

  fn choice_pair(first: &Layout, second: &Layout) -> Layout {
    // We should canonicalize this so that the choices are always in the same order.
    Layout::with_contents(LayoutContents::Choice(first.clone(), second.clone()))
  }

  fn stack_pair(top: &Layout, bottom: &Layout) -> Layout {
    Layout::with_contents(LayoutContents::Stack(top.clone(), bottom.clone()))
  }

  /// Create a layout that is the juxtaposition of two other layouts.
  ///
  /// This lays out the latter layout immediately to the right of the last line
  /// of the left layout.
  pub fn juxtapose_pair(left: &Layout, right: &Layout) -> Layout {
    let mut builder = JuxtaposeBuilder::new(right);
    builder.juxtapose(left)
  }

  /// Creates a layout that is the lowest-cost choice of layouts provided.
  pub fn choices<S, T>(items: S) -> Layout
  where
    S: AsRef<[T]>,
    T: Borrow<Layout>,
  {
    let items = items.as_ref();
    assert!(items.len() > 0);
    let mut curr_layout = items[0].borrow().clone();
    for item in &items[1..] {
      curr_layout = Layout::choice_pair(&curr_layout, item.borrow());
    }

    curr_layout
  }

  /// Create a stack of the layouts, where each of the layouts are
  /// vertically stacked on top of each other in the order provided.
  pub fn stack<'a, S, T>(items: S) -> Layout
  where
    S: AsRef<[T]>,
    T: Borrow<Layout>,
  {
    let items = items.as_ref();
    if items.len() == 0 {
      return Layout::text("");
    }
    assert!(items.len() > 0);
    let mut curr_layout = items[0].borrow().clone();
    for item in &items[1..] {
      curr_layout = Layout::stack_pair(&curr_layout, item.borrow());
    }
    curr_layout
  }

  /// Creates a juxtoposition of layouts, with each layout immediately
  /// following the last line of the previous layout.
  pub fn juxtapose<S, T>(items: S) -> Layout
  where
    S: AsRef<[T]>,
    T: Borrow<Layout>,
  {
    let items = items.as_ref();
    assert!(items.len() > 0);
    let mut curr_layout = items[0].borrow().clone();
    for item in &items[1..] {
      curr_layout = Layout::juxtapose_pair(&curr_layout, item.borrow());
    }
    curr_layout
  }

  /// Lays out the contents of this layout for a text buffer of width margin.
  pub fn layout(&self, margin: u16) -> String {
    self.layout_with_costs(margin, OVERFLOW_COST, NEWLINE_COST)
  }

  /// Lays out the contents of this layout for a text buffer of width margin, and with
  /// newline costs and overflow costs as provided.
  ///
  /// For each character over the margin width, the overflow cost is incurred. For each
  /// newline in the result, the newline cost is incurred. The layout chosen is that with
  /// the smallest overall cost.
  pub fn layout_with_costs(&self, margin: u16, overflow_cost: f32, newline_cost: f32) -> String {
    let final_layout = knot_set::do_layout(self, overflow_cost, newline_cost, margin);
    return final_layout.to_text(0);
  }

  #[doc(hidden)]
  pub fn debug_num_nodes(&self) -> usize {
    let mut counter = NodeCounter::new();
    counter.visit_node(self);
    counter.get_count()
  }

  fn wrap_inner<T: Borrow<Layout>>(layouts: &[T]) -> (Layout, Vec<(Layout, Option<Layout>)>) {
    if layouts.len() == 0 {
      (Layout::text(""), vec![])
    } else if layouts.len() == 1 {
      let singleton = layouts[0].borrow();
      (
        singleton.borrow().clone(),
        vec![(singleton.borrow().clone(), None)],
      )
    } else {
      let first = layouts[0].borrow();
      let rest = &layouts[1..];
      let (block, line_blocks) = Layout::wrap_inner(rest);

      let mut new_line_pairs: Vec<_> = line_blocks
        .into_iter()
        .map(|(l, b)| {
          (
            Layout::juxtapose(&[first.borrow(), &Layout::text(" "), &l]),
            b,
          )
        }).collect();
      new_line_pairs.push((first.borrow().clone(), Some(block)));

      let new_choices: Vec<_> = new_line_pairs
        .iter()
        .map(|(k, v)| match v {
          Some(b) => Layout::stack_pair(k, b),
          None => k.clone(),
        }).collect();

      (Layout::choices(&new_choices), new_line_pairs)
    }
  }

  pub fn wrap<S, T>(layouts: S) -> Layout
  where
    S: AsRef<[T]>,
    T: Borrow<Layout>,
  {
    let slice = layouts.as_ref();
    let (result, _) = Layout::wrap_inner(slice);
    result
  }
}

impl fmt::Debug for Layout {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    <LayoutContents as fmt::Debug>::fmt(&*self.0, f)
  }
}

#[derive(Debug)]
enum LayoutContents {
  Text(SharedString),
  Choice(Layout, Layout),
  Stack(Layout, Layout),
  Juxtapose(SharedString, Layout),
}

struct JuxtaposeBuilder {
  right: Layout,
  memo: col::BTreeMap<*const LayoutContents, Layout>,
}

impl JuxtaposeBuilder {
  fn new(right: &Layout) -> Self {
    JuxtaposeBuilder {
      right: right.clone(),
      memo: col::BTreeMap::new(),
    }
  }

  fn juxtapose(&mut self, left: &Layout) -> Layout {
    if let Some(v) = self.memo.get(&(left.contents() as *const LayoutContents)) {
      return v.clone();
    }

    let new_layout = match left.contents() {
      LayoutContents::Text(t) => {
        Layout::with_contents(LayoutContents::Juxtapose(t.clone(), self.right.clone()))
      }
      LayoutContents::Choice(first, second) => Layout::with_contents(LayoutContents::Choice(
        self.juxtapose(first),
        self.juxtapose(second),
      )),
      LayoutContents::Stack(top, bottom) => {
        Layout::with_contents(LayoutContents::Stack(top.clone(), self.juxtapose(bottom)))
      }
      LayoutContents::Juxtapose(text, jux_right) => Layout::with_contents(
        LayoutContents::Juxtapose(text.clone(), self.juxtapose(jux_right)),
      ),
    };

    self
      .memo
      .insert(left.contents() as *const LayoutContents, new_layout.clone());
    new_layout
  }
}

struct NodeCounter {
  seen: col::BTreeSet<*const LayoutContents>,
}

impl NodeCounter {
  fn new() -> Self {
    NodeCounter {
      seen: col::BTreeSet::new(),
    }
  }
  fn visit_node(&mut self, layout: &Layout) {
    if self.seen.insert(layout.contents() as *const LayoutContents) {
      use LayoutContents::*;
      match layout.contents() {
        Text(_) => {}
        Choice(first, second) => {
          self.visit_node(first);
          self.visit_node(second);
        }
        Stack(top, bottom) => {
          self.visit_node(top);
          self.visit_node(bottom);
        }
        Juxtapose(_left, right) => {
          self.visit_node(right);
        }
      }
    }
  }

  fn get_count(&self) -> usize {
    self.seen.len()
  }
}

#[cfg(test)]
mod test {
  use super::Layout;

  #[test]
  fn test_simple_set() {
    let foo = Layout::text("foo");
    let pair = Layout::juxtapose(&[&foo, &foo]);
    assert_eq!(pair.layout(10), "foofoo");
  }

  #[test]
  fn test_simple_stack() {
    let foo = Layout::text("foo");
    let pair = Layout::stack(&[&foo, &foo]);
    assert_eq!(pair.layout(10), "foo\nfoo");
  }

  #[test]
  fn test_simple_choice() {
    let foo = Layout::text("foo");
    let pair = Layout::choices(&[
      &Layout::juxtapose(&[&foo, &foo]),
      &Layout::stack(&[&foo, &foo]),
    ]);
    assert_eq!(pair.layout(10), "foofoo");
    assert_eq!(pair.layout(4), "foo\nfoo");
  }

  #[test]
  fn test_wrap() {
    let lorem = &"Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Nullam suscipit dolor sed dolor facilisis ultrices ut id nisl. Nam
    malesuada malesuada dolor, eget dapibus nibh euismod at. Vestibulum
    aliquam aliquam mi at tincidunt. Sed imperdiet eget purus in egestas.
    Suspendisse id odio in augue cursus tempor. Duis imperdiet imperdiet eros
    et lobortis. Vestibulum vestibulum condimentum dui, sit amet fringilla libero
    commodo at. Sed aliquam mauris vitae egestas convallis. Integer sed est vitae
    nibh pellentesque sodales. Suspendisse sit amet dignissim arcu. Quisque hendrerit
    pellentesque egestas. Nulla at elementum est. Maecenas lectus turpis, placerat
    vitae est sit amet, mattis pharetra orci. Praesent.";
    let words: Vec<_> = lorem.split_whitespace().map(|k| Layout::text(k)).collect();
    let layout = Layout::wrap(words);
    println!("{}", layout.debug_num_nodes());
    assert_eq!(layout.layout(80).chars().filter(|c| c == &'\n').count(), 9);
    assert_eq!(layout.layout(100).chars().filter(|c| c == &'\n').count(), 7);
  }

  #[test]
  fn test_large_stack() {
    let foo = Layout::text("foo");
    let bar = Layout::text("bar");
    let foo_stack = Layout::stack(&[&foo, &bar]);
    let foo_jux = Layout::juxtapose(&[&foo, &bar]);
    let foo_choice = Layout::choices(&[&foo_stack, &foo_jux]);

    let choice = Layout::juxtapose(&vec![&foo_choice; 1000]);

    println!("Done creating layout.");
    println!("Num nodes: {}", choice.debug_num_nodes());

    choice.layout(500);
  }
}