#![feature(rust_2018_preview, uniform_paths)]

extern crate unicode_width;

mod knot_column;
mod knot_set;
mod linear_value;
mod resolved_layout;
mod shared_string;

use std::collections as col;
use std::fmt;
use std::rc;

use shared_string::SharedString;

const OVERFLOW_COST: f32 = 100.0;
const NEWLINE_COST: f32 = 1.0;

#[derive(Clone)]
pub struct Layout(rc::Rc<LayoutContents>);

impl Layout {
    fn with_contents(contents: LayoutContents) -> Layout {
        Layout(rc::Rc::new(contents))
    }

    fn contents(&self) -> &LayoutContents {
        &*self.0
    }

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

    pub fn juxtapose_pair(left: &Layout, right: &Layout) -> Layout {
        let mut builder = JuxtaposeBuilder::new(right);
        builder.juxtapose(left)
    }

    pub fn choices<'a>(items: &impl AsRef<[&'a Layout]>) -> Layout {
        let items = items.as_ref();
        assert!(items.len() > 0);
        let mut curr_layout = items[0].clone();
        for item in &items[1..] {
            curr_layout = Layout::choice_pair(&curr_layout, item);
        }

        curr_layout
    }

    pub fn stack<'a>(items: &impl AsRef<[&'a Layout]>) -> Layout {
        let items = items.as_ref();
        assert!(items.len() > 0);
        let mut curr_layout = items[0].clone();
        for item in &items[1..] {
            curr_layout = Layout::stack_pair(&curr_layout, item);
        }
        curr_layout
    }

    pub fn juxtapose<'a>(items: &impl AsRef<[&'a Layout]>) -> Layout {
        let items = items.as_ref();
        assert!(items.len() > 0);
        let mut curr_layout = items[0].clone();
        for item in &items[1..] {
            curr_layout = Layout::juxtapose_pair(&curr_layout, item);
        }
        curr_layout
    }

    pub fn layout(&self, margin: u16) -> String {
        let final_layout = knot_set::do_layout(self, OVERFLOW_COST, NEWLINE_COST, margin);
        return final_layout.to_text(0);
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

        self.memo
            .insert(left.contents() as *const LayoutContents, new_layout.clone());
        new_layout
    }
}

#[macro_export]
macro_rules! pp_stack {
    () => { Layout::text("".to_owned()) };
    ($only:expr) => { $only };
    ($first:expr, $($rest:expr),*) => { Layout::stack($first, pp_stack!($($rest),*)) };
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
    fn text_simple_choice() {
        let foo = Layout::text("foo");
        let pair = Layout::choices(&[
            &Layout::juxtapose(&[&foo, &foo]),
            &Layout::stack(&[&foo, &foo]),
        ]);
        assert_eq!(pair.layout(10), "foofoo");
        assert_eq!(pair.layout(4), "foo\nfoo");
    }

    #[test]
    #[ignore]
    fn text_large_stack() {
        let foo = Layout::text("foo");
        let bar = Layout::text("bar");
        let foo_stack = Layout::stack(&[&foo, &bar]);
        let foo_jux = Layout::juxtapose(&[&foo, &bar]);
        let foo_choice = Layout::choices(&[&foo_stack, &foo_jux]);

        let choice = Layout::juxtapose(&vec![&foo_choice; 1000]);

        println!("Done creating layout.");

        choice.layout(500);
    }
}
