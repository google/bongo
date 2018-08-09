#![feature(rust_2018_preview, nll)]

use std::cmp;
use std::collections as col;
use std::ops;
use std::rc;

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
        JuxtaposeBuilder { right: right.clone(), memo: col::BTreeMap::new() }
    }

    fn juxtapose(&mut self, left: &Layout) -> Layout {
        if let Some(v) = self.memo.get(&(left.contents() as *const LayoutContents)) {
            return v.clone();
        }

        let new_layout = match left.contents() {
            LayoutContents::Text(t) => Layout::with_contents(LayoutContents::Juxtapose(t.clone(), self.right.clone())),
            LayoutContents::Choice(first, second) =>
                Layout::with_contents(LayoutContents::Choice(self.juxtapose(first), self.juxtapose(second))),
            LayoutContents::Stack(top, bottom) =>
                Layout::with_contents(LayoutContents::Stack(top.clone(), self.juxtapose(bottom))),
            LayoutContents::Juxtapose(text, jux_right) =>
                Layout::with_contents(LayoutContents::Juxtapose(text.clone(), self.juxtapose(jux_right))),
        };

        self.memo.insert(left.contents() as *const LayoutContents, new_layout.clone());
        new_layout
    }
}

#[derive(Clone, Debug)]
enum ResolvedLayout {
    Text(String),
    Horiz(ResolvedLayoutRef, ResolvedLayoutRef),
    Vert(ResolvedLayoutRef, ResolvedLayoutRef),
}

#[derive(Clone, Debug)]
struct ResolvedLayoutRef(std::rc::Rc<ResolvedLayout>);

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
struct KnotColumn(u16);

#[derive(Clone, Debug)]
struct KnotData {
    // The layout as resolved.
    resolved_layout: ResolvedLayoutRef,

    /// The number of characters in the last line
    span: u16,

    /// The cost value at knot start
    intercept: f32,

    /// The cost increment per column after knot start
    rise: f32,
}

#[derive(Copy, Clone, Debug)]
enum SearchDirection {
    Less,
    LessEq,
    Greater,
    GreaterEq,
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
        SearchDirection::Less => {
            let mut range = map.range((ops::Bound::Unbounded, ops::Bound::Excluded(search_key)));
            range.next_back()
        }
        SearchDirection::LessEq => {
            let mut range = map.range((ops::Bound::Unbounded, ops::Bound::Included(search_key)));
            range.next_back()
        }
        SearchDirection::Greater => {
            let mut range = map.range((ops::Bound::Excluded(search_key), ops::Bound::Unbounded));
            range.next()
        }
        SearchDirection::GreaterEq => {
            let mut range = map.range((ops::Bound::Included(search_key), ops::Bound::Unbounded));
            range.next()
        }
    }
}

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
        let (knot_col, knot_value) = search_map(&self.knots, &col, SearchDirection::LessEq)
            .expect("There should always be a knot at 0");
        let column_distance = col.0 - knot_col.0;
        KnotData {
            resolved_layout: knot_value.resolved_layout.clone(),
            span: knot_value.span,
            intercept: knot_value.intercept + knot_value.rise * (column_distance as f32),
            rise: knot_value.rise,
        }
    }

    fn knot_values(&self) -> col::BTreeSet<KnotColumn> {
        self.knots.keys().cloned().collect()
    }

    fn knot_values_between(&self, begin: KnotColumn, end: KnotColumn) -> col::BTreeSet<KnotColumn> {
        self.knots
            .range((ops::Bound::Included(begin), ops::Bound::Excluded(end)))
            .map(|(k, _)| *k)
            .collect()
    }

    fn left_knot_data(&self, col: KnotColumn) -> Option<(KnotColumn, &KnotData)> {
        search_map(&self.knots, &col, SearchDirection::Less).map(|(k, v)| (*k, v))
    }

    fn right_knot_data(&self, col: KnotColumn) -> Option<(KnotColumn, &KnotData)> {
        search_map(&self.knots, &col, SearchDirection::GreaterEq).map(|(k, v)| (*k, v))
    }

    fn intervals(&self) -> Vec<KnotInterval> {
        let mut knot_iter = self.knots.iter();
        let mut curr_col;
        let mut curr_data;
        let (first_col, first_data) = knot_iter.next().expect("KnotSets should have at least one element.");
        curr_col = *first_col;
        curr_data = first_data.clone();
        let mut result = Vec::with_capacity(self.knots.len());

        for (col, data) in knot_iter {
            result.push(KnotInterval {
                start: curr_col,
                end: Some(*col),
                data: curr_data,
            });
            curr_col = *col;
            curr_data = data.clone();
        }

        result.push(KnotInterval {
            start: curr_col,
            end: None,
            data: curr_data,
        });
        result
    }
}

struct KnotInterval {
    // The inclusive starting column of a knot. Minimum 0.
    start: KnotColumn,

    // The exclusive ending column of a knot, or None if unbounded.
    end: Option<KnotColumn>,

    data: KnotData,
}

struct KnotSetBuilder {
    margin: u16,
    overflow_cost: f32,
    newline_cost: f32,
}

impl KnotSetBuilder {
    pub fn new_text(&self, text: impl Into<String>) -> KnotSet {
        let text_str = text.into();
        let text_len = text_str.len() as u16;
        let layout = ResolvedLayoutRef(rc::Rc::new(ResolvedLayout::Text(text_str)));
        let mut knots = col::BTreeMap::new();
        if text_len < self.margin {
            let flat_data = KnotData {
                resolved_layout: layout.clone(),
                span: text_len,
                intercept: 0.0,
                rise: 0.0,
            };
            knots.insert(KnotColumn(0), flat_data);

            let rise_data = KnotData {
                resolved_layout: layout,
                span: text_len,
                intercept: 0.0,
                rise: self.overflow_cost,
            };

            knots.insert(KnotColumn(self.margin - text_len), rise_data);
        } else {
            let rise_data = KnotData {
                resolved_layout: layout,
                span: text_len,
                intercept: self.overflow_cost * ((text_len - self.margin) as f32),
                rise: self.overflow_cost,
            };
            knots.insert(KnotColumn(0), rise_data);
        }
        KnotSet { knots: knots }
    }

    pub fn new_vert(&self, top: KnotSet, bottom: KnotSet) -> KnotSet {
        let new_knot_values = top
            .knot_values()
            .union(&bottom.knot_values())
            .cloned()
            .collect();

        KnotSet::new_knot_set(&new_knot_values, |col| {
            let top_data = top.knot_data_at(col);
            let bottom_data = bottom.knot_data_at(col);
            KnotData {
                resolved_layout: ResolvedLayoutRef(rc::Rc::new(ResolvedLayout::Vert(
                    top_data.resolved_layout.clone(),
                    bottom_data.resolved_layout.clone(),
                ))),
                span: bottom_data.span,
                intercept: top_data.intercept + bottom_data.intercept + self.newline_cost,
                rise: top_data.rise + bottom_data.rise,
            }
        })
    }

    pub fn new_horiz(&self, _left: KnotSet, _right: KnotSet) -> KnotSet {
        unimplemented!()
    }

    pub fn new_choice(&self, _choice1: KnotSet, _choice2: KnotSet) -> KnotSet {
        unimplemented!()
    }
}
