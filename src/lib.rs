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

#[derive(Clone, Debug)]
enum ResolvedLayout {
    Text(String),
    Horiz(ResolvedLayoutRef, ResolvedLayoutRef),
    Vert(ResolvedLayoutRef, ResolvedLayoutRef),
}

#[derive(Clone, Debug)]
struct ResolvedLayoutRef(std::rc::Rc<ResolvedLayout>);

impl ResolvedLayoutRef {
    fn new(inner: ResolvedLayout) -> ResolvedLayoutRef {
        ResolvedLayoutRef(rc::Rc::new(inner))
    }

    pub fn new_text(text: impl Into<String>) -> ResolvedLayoutRef {
        ResolvedLayoutRef::new(ResolvedLayout::Text(text.into()))
    }

    pub fn new_horiz(left: ResolvedLayoutRef, right: ResolvedLayoutRef) -> ResolvedLayoutRef {
        ResolvedLayoutRef::new(ResolvedLayout::Horiz(left, right))
    }

    pub fn new_vert(top: ResolvedLayoutRef, bottom: ResolvedLayoutRef) -> ResolvedLayoutRef {
        ResolvedLayoutRef::new(ResolvedLayout::Vert(top, bottom))
    }
}

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
        let (first_col, first_data) = knot_iter
            .next()
            .expect("KnotSets should have at least one element.");
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
}

impl KnotSetBuilder {
    pub fn new_text(&self, text: impl Into<String>) -> KnotSet {
        let text_str = text.into();
        let text_len = text_str.len() as u16;
        let layout = ResolvedLayoutRef::new_text(text_str);
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
                resolved_layout: ResolvedLayoutRef::new_vert(
                    top_data.resolved_layout.clone(),
                    bottom_data.resolved_layout.clone(),
                ),
                span: bottom_data.span,
                intercept: top_data.intercept + bottom_data.intercept + self.newline_cost,
                rise: top_data.rise + bottom_data.rise,
            }
        })
    }

    pub fn new_horiz(&self, left: impl Into<String>, right: KnotSet) -> KnotSet {
        let left = left.into();
        let left_width = left.len() as u16;

        let shifted_left_knot_values = right
            .knot_values_between(KnotColumn(left_width), None)
            .into_iter()
            .map(|KnotColumn(col)| KnotColumn(col - left_width))
            .collect();

        let text_knot_set = self.new_text(left);
        let new_knot_values: col::BTreeSet<_> = text_knot_set
            .knot_values()
            .union(&shifted_left_knot_values)
            .cloned()
            .collect();

        KnotSet::new_knot_set(&new_knot_values, |col| {
            let right_knot_col = KnotColumn(col.0 + left_width);
            let left_data = text_knot_set.knot_data_at(col);
            let right_data = right.knot_data_at(right_knot_col);

            let sub_intercept_factor;
            let sub_rise_factor;
            if col.0 > self.margin {
                sub_intercept_factor = (col.0 - self.margin) as f32 * self.overflow_cost;
                sub_rise_factor = self.overflow_cost;
            } else {
                sub_intercept_factor = 0.0;
                sub_rise_factor = 0.0;
            }
            KnotData {
                resolved_layout: ResolvedLayoutRef::new_horiz(
                    right_data.resolved_layout.clone(),
                    left_data.resolved_layout.clone(),
                ),
                span: left_data.span + right_data.span,
                intercept: left_data.intercept + right_data.intercept - sub_intercept_factor,
                rise: left_data.rise + right_data.rise - sub_rise_factor,
            }
        })
    }

    pub fn new_choice(&self, choice1: KnotSet, choice2: KnotSet) -> KnotSet {
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

            let chi_k = (choice2_data.intercept - choice1_data.intercept)
                / (choice1_data.rise - choice2_data.rise);

            let intersect_delta = chi_k.ceil() as u16;

            let in_range = match end_opt {
                Some(k) => start.0 + intersect_delta < k.0,
                None => false,
            };

            if in_range {
                extra_knots.insert(KnotColumn(start.0 + intersect_delta));
            }
        }

        let all_knots: col::BTreeSet<_> = base_knots.union(&extra_knots).cloned().collect();

        unimplemented!()
    }
}
