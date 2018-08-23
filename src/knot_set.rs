use std::cmp;
use std::collections as col;
use std::ops;
use std::rc;

use crate::{Layout, LayoutContents};
use knot_column::KnotColumn;
use linear_value::{BinaryResult, LinearValue};
use resolved_layout::ResolvedLayoutRef;

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

    pub fn get_primary_layout(&self) -> ResolvedLayoutRef {
        self.knot_data_at(KnotColumn::new(0))
            .resolved_layout
            .clone()
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

pub struct KnotSetBuilder {
    margin: u16,
    overflow_cost: f32,
    newline_cost: f32,
    memo: std::cell::RefCell<col::BTreeMap<*const LayoutContents, rc::Rc<KnotSet>>>,
}

impl KnotSetBuilder {
    fn new_text_impl(&self, text: impl Into<String>) -> KnotSet {
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

    fn new_vert_impl(&self, top: &KnotSet, bottom: &KnotSet) -> KnotSet {
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

    fn new_horiz_impl(&self, left: impl Into<String>, right: &KnotSet) -> KnotSet {
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

    fn new_choice_impl(&self, choice1: &KnotSet, choice2: &KnotSet) -> KnotSet {
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

pub fn do_layout(
    layout: &Layout,
    overflow_cost: f32,
    newline_cost: f32,
    margin: u16,
) -> ResolvedLayoutRef {
    let builder = KnotSetBuilder {
        margin: margin,
        overflow_cost: overflow_cost,
        newline_cost: newline_cost,
        memo: std::cell::RefCell::new(col::BTreeMap::new()),
    };

    let final_knot_set = builder.get_knot_set(layout);
    final_knot_set.get_primary_layout()
}
