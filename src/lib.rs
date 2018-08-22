#![feature(rust_2018_preview, uniform_paths)]

use std::cmp;
use std::collections as col;
use std::ops;
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
        let builder = KnotSetBuilder {
            margin: margin,
            overflow_cost: OVERFLOW_COST,
            newline_cost: NEWLINE_COST,
            memo: std::cell::RefCell::new(col::BTreeMap::new()),
        };

        let final_knot_set = builder.get_knot_set(self);
        println!("Final memo size: {}", builder.memo.borrow().len());
        let final_layout = final_knot_set.knot_data_at(KnotColumn::new(0)).resolved_layout.clone();
        println!("Number of knots in final: {}", final_knot_set.knot_values().len());
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

mod resolved_layout;
mod knot_column;
mod linear_value;

use resolved_layout::ResolvedLayoutRef;
use knot_column::KnotColumn;
use linear_value::{BinaryResult, LinearValue};

#[derive(Clone, Debug)]
struct KnotData {
    // The layout as resolved.
    resolved_layout: ResolvedLayoutRef,

    /// The number of characters in the last line
    span: u16,

    /// The cost value at knot start
    value: LinearValue,
}

#[derive(Copy, Clone, Debug)]
enum SearchDirection {
    LessEq,
}

fn search_map<'a, T, K, V>(
    map: &'a col::BTreeMap<K, V>,
    search_key: &T,
    direction: SearchDirection,
) -> Option<(&'a K, &'a V)>
where
    K: std::borrow::Borrow<T> + cmp::Ord,
    T: cmp::Ord,
{
    match direction {
        SearchDirection::LessEq => {
            let mut range = map.range((ops::Bound::Unbounded, ops::Bound::Included(search_key)));
            range.next_back()
        }
    }
}

#[derive(Clone, Debug)]
struct KnotSet {
    knots: col::BTreeMap<KnotColumn, KnotData>,
}

impl KnotSet {
    fn new_knot_set(
        knot_cols: &col::BTreeSet<KnotColumn>,
        func: impl Fn(KnotColumn) -> KnotData,
    ) -> KnotSet {
        let mut new_knots = col::BTreeMap::new();
        for &col in knot_cols {
            new_knots.insert(col, func(col));
        }
        KnotSet { knots: new_knots }
    }

    fn knot_data_at(&self, col: KnotColumn) -> KnotData {
        let (&knot_col, knot_value) = search_map(&self.knots, &col, SearchDirection::LessEq)
            .expect("There should always be a knot at 0");
        let column_distance = (col - knot_col).col();
        KnotData {
            resolved_layout: knot_value.resolved_layout.clone(),
            span: knot_value.span,
            value: knot_value.value.advance(column_distance),
        }
    }

    fn knot_values(&self) -> col::BTreeSet<KnotColumn> {
        self.knots.keys().cloned().collect()
    }

    fn knot_values_between(
        &self,
        begin: KnotColumn,
        end: Option<KnotColumn>,
    ) -> col::BTreeSet<KnotColumn> {
        let right_bound = match end {
            Some(c) => ops::Bound::Excluded(c),
            None => ops::Bound::Unbounded,
        };
        self.knots
            .range((ops::Bound::Included(begin), right_bound))
            .map(|(k, _)| *k)
            .collect()
    }
}

fn column_intervals(columns: &col::BTreeSet<KnotColumn>) -> Vec<(KnotColumn, Option<KnotColumn>)> {
    let mut iter = columns.iter();
    let first = iter.next().expect("Always at least a knot at 0");
    let mut curr_start = *first;
    let mut result = Vec::new();
    for &col in iter {
        result.push((curr_start, Some(col)));
        curr_start = col;
    }
    result.push((curr_start, None));
    result
}

struct KnotSetBuilder {
    margin: u16,
    overflow_cost: f32,
    newline_cost: f32,
    memo: std::cell::RefCell<col::BTreeMap<*const LayoutContents, rc::Rc<KnotSet>>>,
}

impl KnotSetBuilder {
    pub fn new_text_impl(&self, text: impl Into<String>) -> KnotSet {
        let text_str = text.into();
        let text_len = text_str.len() as u16;
        let layout = ResolvedLayoutRef::new_text(text_str);
        let mut knots = col::BTreeMap::new();
        if text_len < self.margin {
            let flat_data = KnotData {
                resolved_layout: layout.clone(),
                span: text_len,
                value: LinearValue::new(0.0, 0.0),
            };
            knots.insert(KnotColumn::new(0), flat_data);

            let rise_data = KnotData {
                resolved_layout: layout,
                span: text_len,
                value: LinearValue::new(0.0, self.overflow_cost),
            };

            knots.insert(KnotColumn::new(self.margin - text_len), rise_data);
        } else {
            let rise_data = KnotData {
                resolved_layout: layout,
                span: text_len,
                value: LinearValue::new(
                    self.overflow_cost * ((text_len - self.margin) as f32),
                    self.overflow_cost,
                ),
            };
            knots.insert(KnotColumn::new(0), rise_data);
        }
        KnotSet { knots: knots }
    }

    pub fn new_vert_impl(&self, top: &KnotSet, bottom: &KnotSet) -> KnotSet {
        let new_knot_values = top
            .knot_values()
            .union(&bottom.knot_values())
            .cloned()
            .collect();

        KnotSet::new_knot_set(&new_knot_values, |col| {
            let top_data = top.knot_data_at(col);
            let bottom_data = bottom.knot_data_at(col);
            KnotData {
                resolved_layout: ResolvedLayoutRef::new_vert(
                    top_data.resolved_layout.clone(),
                    bottom_data.resolved_layout.clone(),
                ),
                span: bottom_data.span,
                value: top_data.value
                    + bottom_data.value
                    + LinearValue::new_flat(self.newline_cost),
            }
        })
    }

    pub fn new_horiz_impl(&self, left: impl Into<String>, right: &KnotSet) -> KnotSet {
        let left = left.into();
        let left_width = left.len() as u16;
        let shifted_left_knot_values = right
            .knot_values_between(KnotColumn::new(left_width), None)
            .into_iter()
            .map(|col| col - KnotColumn::new(left_width))
            .collect();

        let text_knot_set = self.new_text_impl(left);
        let new_knot_values: col::BTreeSet<_> = text_knot_set
            .knot_values()
            .union(&shifted_left_knot_values)
            .cloned()
            .collect();

        KnotSet::new_knot_set(&new_knot_values, |col| {
            let right_knot_col = col + KnotColumn::new(left_width);
            let left_data = text_knot_set.knot_data_at(col);
            let right_data = right.knot_data_at(right_knot_col);

            let sub_factor;
            if col > KnotColumn::new(self.margin) {
                sub_factor = LinearValue::new(0.0, self.overflow_cost)
                    .advance((col - KnotColumn::new(self.margin)).col());
            } else {
                sub_factor = LinearValue::identity();
            }
            KnotData {
                resolved_layout: ResolvedLayoutRef::new_horiz(
                    left_data.resolved_layout.clone(),
                    right_data.resolved_layout.clone(),
                ),
                span: left_data.span + right_data.span,
                value: left_data.value + right_data.value - sub_factor,
            }
        })
    }

    pub fn new_choice_impl(&self, choice1: &KnotSet, choice2: &KnotSet) -> KnotSet {
        // Set "L" in the paper
        let base_knots: col::BTreeSet<_> = choice1
            .knot_values()
            .union(&choice2.knot_values())
            .cloned()
            .collect();
        let mut extra_knots = col::BTreeSet::new();
        for (start, end_opt) in column_intervals(&base_knots) {
            // Find the Chi_K value for this range.
            let choice1_data = choice1.knot_data_at(start);
            let choice2_data = choice2.knot_data_at(start);

            let intersect = choice1_data.value.forward_intersection(choice2_data.value);

            if let Some(intersect) = intersect {
                let intersect_delta = KnotColumn::new(intersect);

                let in_range = match end_opt {
                    Some(k) => start + intersect_delta < k,
                    None => false,
                };

                if in_range {
                    extra_knots.insert(start + intersect_delta);
                }
            }
        }

        let all_knots: col::BTreeSet<_> = base_knots.union(&extra_knots).cloned().collect();

        KnotSet::new_knot_set(&all_knots, |col| {
            let choice1_data = choice1.knot_data_at(col);
            let choice2_data = choice2.knot_data_at(col);

            match choice1_data.value.max_initial_value(choice2_data.value) {
                BinaryResult::Left => choice2_data,
                BinaryResult::Right => choice1_data,
            }
        })
    }

    fn get_knot_set(&self, layout: &Layout) -> rc::Rc<KnotSet> {
        use self::LayoutContents::*;
        let contents = layout.contents();
        let memo_key = contents as *const LayoutContents;
        {
            let borrow = self.memo.borrow();
            if let Some(memoized) = borrow.get(&memo_key) {
                return memoized.clone();
            }
        }

        let new_knot_set = match contents {
            Text(text) => self.new_text_impl(text.clone()),
            Stack(top, bottom) => {
                self.new_vert_impl(&*self.get_knot_set(top), &*self.get_knot_set(bottom))
            }
            Juxtapose(left, right) => self.new_horiz_impl(left.clone(), &*self.get_knot_set(right)),
            Choice(left, right) => {
                self.new_choice_impl(&*self.get_knot_set(left), &*self.get_knot_set(right))
            }
        };

        {
            let mut borrow = self.memo.borrow_mut();
            borrow.insert(memo_key, rc::Rc::new(new_knot_set));
            borrow.get(&memo_key).unwrap().clone()
        }
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
