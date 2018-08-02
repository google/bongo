#![feature(rust_2018_preview, nll)]

use std::collections as col;
use std::cmp;
use std::ops;
use std::rc;

#[derive(Clone, Debug)]
struct LayoutRef(rc::Rc<LayoutContents>);

impl LayoutRef {
    fn contents(&self) -> &LayoutContents {
        &*self.0
    }
}

impl ops::Deref for LayoutRef {
    type Target = LayoutContents;
    fn deref(&self) -> &LayoutContents {
        self.contents()
    }
}

impl AsRef<LayoutContents> for LayoutRef {
    fn as_ref(&self) -> &LayoutContents {
        self.contents()
    }
}

#[derive(Debug)]
enum LayoutContents {
    Text(String),
    Horiz(LayoutRef, LayoutRef),
    Vert(LayoutRef, LayoutRef),
    Choice(LayoutRef, LayoutRef),
}

trait OrdAs {
    type AsType: Eq + PartialEq + Ord + PartialOrd;
    fn ord_as(&self) -> Self::AsType;
}

struct OrdAsKey<T>(T);

impl<T> ops::Deref for OrdAsKey<T> {
    type Target = T;
    fn deref(&self) -> &T { &self.0 }
}

impl<T> ops::DerefMut for OrdAsKey<T> {
    fn deref_mut(&mut self) -> &mut T { &mut self.0 }
}

impl<T: OrdAs> Ord for OrdAsKey<T> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.0.ord_as().cmp(&other.0.ord_as())
    }
}

impl<T: OrdAs> PartialOrd for OrdAsKey<T> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: OrdAs> Eq for OrdAsKey<T> {

}

impl<T: OrdAs> PartialEq for OrdAsKey<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.ord_as().eq(&other.0.ord_as())
    }
}


#[derive(Copy, Clone, Debug)]
struct RefMapKey<'a, K: 'a>(&'a K);

impl<'a, K: 'a> OrdAs for RefMapKey<'a, K> {
    type AsType = *const K;
    fn ord_as(&self) -> *const K { self.0 }
}

struct RefMap<'a, K: 'a, V> {
    map: col::BTreeMap<OrdAsKey<RefMapKey<'a, K>>, V>,
}

impl<'a, K: 'a, V> RefMap<'a, K, V> {
    fn insert(&mut self, key: &'a (impl AsRef<K>), value: V) -> Option<V> {
        self.map.insert(OrdAsKey(RefMapKey(key.as_ref())), value)
    }

    fn iter<'b: 'a>(&'b self) -> impl Iterator<Item = (&'a K, &'b V)> {
        self.map.iter().map(|(k, v)| ((k.0).0, v))
    }
}

// ReducedLayout

#[derive(Debug)]
enum ReducedLayout {
    Text(String),
    Horiz(String, ReducedLayoutRef),
    Vert(ReducedLayoutRef, ReducedLayoutRef),
    Choice(ReducedLayoutRef, ReducedLayoutRef),
}

#[derive(Clone, Debug)]
struct ReducedLayoutRef(rc::Rc<ReducedLayout>);

impl ReducedLayoutRef {
    fn new(layout: ReducedLayout) -> Self { ReducedLayoutRef(rc::Rc::new(layout)) }
}

impl ops::Deref for ReducedLayoutRef {
    type Target = ReducedLayout;
    fn deref(&self) -> &ReducedLayout {
        &*self.0
    }
}

mod layout_reducer {

use super::LayoutContents;
use super::ReducedLayout;
use super::ReducedLayoutRef;
use super::LayoutRef;

use std::collections as col;

struct LayoutReducer {
    memo: col::BTreeMap<(*const LayoutContents, Option<*const ReducedLayout>), ReducedLayoutRef>,
}

impl LayoutReducer {
    fn new() -> Self {
        LayoutReducer { memo: col::BTreeMap::new() }
    }

    fn reduce(&mut self, layout: &LayoutRef, trailer: Option<&ReducedLayoutRef>) -> ReducedLayoutRef {
        let layout_ptr = &**layout;
        let trailer_ptr = trailer.map(|t| &**t as *const ReducedLayout);
        match self.memo.get(&(layout_ptr, trailer_ptr)) {
            Some(v) => v.clone(),
            None => {
                let result = self.reduce_real(layout, trailer);
                self.memo.insert((layout_ptr, trailer_ptr), result.clone());
                result
            }
        }
    }

    fn reduce_real(&mut self, layout: &LayoutRef, trailer: Option<&ReducedLayoutRef>) -> ReducedLayoutRef {
        match layout.contents() {
            LayoutContents::Text(text) => match trailer {
                Some(trailer_val) => ReducedLayoutRef::new(ReducedLayout::Horiz(text.clone(), trailer_val.clone())),
                None => ReducedLayoutRef::new(ReducedLayout::Text(text.clone())),
            }
            LayoutContents::Horiz(left, right) => {
                let reduced_right = self.reduce(right, trailer);
                self.reduce(left, Some(&reduced_right))
            }
            LayoutContents::Vert(top, bottom) => {
                let reduced_top = self.reduce(top, None);
                let reduced_bottom = self.reduce(bottom, trailer);
                ReducedLayoutRef::new(ReducedLayout::Vert(reduced_top, reduced_bottom))
            }
            LayoutContents::Choice(left, right) => {
                let reduced_left = self.reduce(left, trailer);
                let reduced_right = self.reduce(right, trailer);
                ReducedLayoutRef::new(ReducedLayout::Choice(reduced_left, reduced_right))
            }
        }
    }
}

pub(super) fn reduce_layout(layout: &LayoutRef) -> ReducedLayoutRef {
    let mut reducer = LayoutReducer::new();
    reducer.reduce(layout, None)
}

}

use layout_reducer::reduce_layout;

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

fn search_map<'a, T, K, V>(map: &'a col::BTreeMap<K, V>, search_key: &T, direction: SearchDirection) -> Option<(&'a K, &'a V)>
where K: std::borrow::Borrow<T> + cmp::Ord, T: cmp::Ord {
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
    knots: col::BTreeMap<KnotColumn, KnotData>
}

impl KnotSet {
    fn cost_at(&self, col: KnotColumn) -> f32 {
        let _prev_knot = search_map(&self.knots, &col, SearchDirection::LessEq)
            .expect("There should always be a knot at 0");
            unimplemented!()
    }

    fn left_knot_data(&self, col: KnotColumn) -> Option<(KnotColumn, &KnotData)> {
        search_map(&self.knots, &col, SearchDirection::Less)
            .map(|(k, v)| (*k, v))
    }

    fn right_knot_data(&self, col: KnotColumn) -> Option<(KnotColumn, &KnotData)> {
        search_map(&self.knots, &col, SearchDirection::GreaterEq)
            .map(|(k, v)| (*k, v))
    }
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
        KnotSet {
            knots: knots,
        }
    }

    pub fn new_vert(&self, _top: KnotSet, _bottom: KnotSet) -> KnotSet {
        unimplemented!()
    }

    pub fn new_horiz(&self, _left: KnotSet, _right: KnotSet) -> KnotSet {
        unimplemented!()
    }

    pub fn new_choice(&self, _choice1: KnotSet, _choice2: KnotSet) -> KnotSet {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
