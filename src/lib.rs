#![feature(rust_2018_preview, uniform_paths)]

mod resolved_layout;
mod knot_column;
mod linear_value;
mod knot_set;

use std::collections as col;
use std::rc;

const OVERFLOW_COST: f32 = 100.0;
const NEWLINE_COST: f32 = 1.0;

#[derive(Clone, Debug)]
pub struct Layout(rc::Rc<LayoutContents>);

impl Layout {
    fn with_contents(contents: LayoutContents) -> Layout {
        Layout(rc::Rc::new(contents))
    }

    fn contents(&self) -> &LayoutContents {
        &*self.0
    }

    pub fn text(lit_text: String) -> Layout {
        Layout::with_contents(LayoutContents::Text(lit_text))
    }

    pub fn choice(first: &Layout, second: &Layout) -> Layout {
        // We should canonicalize this so that the choices are always in the same order.
        Layout::with_contents(LayoutContents::Choice(first.clone(), second.clone()))
    }

    pub fn stack(top: &Layout, bottom: &Layout) -> Layout {
        Layout::with_contents(LayoutContents::Stack(top.clone(), bottom.clone()))
    }

    pub fn juxtapose(left: &Layout, right: &Layout) -> Layout {
        let mut builder = JuxtaposeBuilder::new(right);
        builder.juxtapose(left)
    }

    pub fn choices(items: &[&Layout]) -> Layout {
        assert!(items.len() > 0);
        let mut curr_layout = items[0].clone();
        for i in 1..items.len() {
            curr_layout = Layout::choice(&curr_layout, &items[i]);
        }

        curr_layout
    }

    pub fn layout(&self, margin: u16) -> String {
        let final_layout = knot_set::do_layout(self, OVERFLOW_COST, NEWLINE_COST, margin);
        return final_layout.to_text(0);
    }
}

#[derive(Debug)]
enum LayoutContents {
    Text(String),
    Choice(Layout, Layout),
    Stack(Layout, Layout),
    Juxtapose(String, Layout),
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
        let foo = Layout::text("foo".to_owned());
        let pair = Layout::juxtapose(&foo, &foo);
        assert_eq!(pair.layout(10), "foofoo");
    }

    #[test]
    fn test_simple_stack() {
        let foo = Layout::text("foo".to_owned());
        let pair = Layout::stack(&foo, &foo);
        assert_eq!(pair.layout(10), "foo\nfoo");
    }

    #[test]
    fn text_simple_choice() {
        let foo = Layout::text("foo".to_owned());
        let pair = Layout::choice(&Layout::juxtapose(&foo, &foo), &Layout::stack(&foo, &foo));
        assert_eq!(pair.layout(10), "foofoo");
        assert_eq!(pair.layout(4), "foo\nfoo");
    }

    #[test]
    fn text_large_stack() {
        let foo = Layout::text("foo".to_owned());
        let bar = Layout::text("bar".to_owned());
        let foo_stack = Layout::stack(&foo, &bar);
        let foo_jux = Layout::juxtapose(&foo, &bar);
        let foo_choice = Layout::choice(&foo_stack, &foo_jux);

        let mut curr_choice = foo_choice.clone();
        for _ in 0..1000 {
            curr_choice = Layout::juxtapose(&foo_choice, &curr_choice);
        }

        println!("Done creating layout.");

        curr_choice.layout(500);

        assert!(false);
    }
}
