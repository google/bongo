use std::rc;

#[derive(Clone, Debug)]
enum ResolvedLayout {
    Text(String),
    Horiz(ResolvedLayoutRef, ResolvedLayoutRef),
    Vert(ResolvedLayoutRef, ResolvedLayoutRef),
}

#[derive(Clone, Debug)]
pub struct ResolvedLayoutRef(std::rc::Rc<ResolvedLayout>);

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

    pub fn to_text(&self, curr_indent: u16) -> String {
        use self::ResolvedLayout::*;
        match &*self.0 {
            Text(text) => text.clone(),
            Horiz(left_ref, right_ref) => {
                left_ref.to_text(curr_indent) + &right_ref.to_text(curr_indent + left_ref.size())
            }
            Vert(top, bottom) => {
                top.to_text(curr_indent)
                    + "\n"
                    + &(" ".repeat(curr_indent as usize))
                    + &bottom.to_text(curr_indent)
            }
        }
    }

    pub fn size(&self) -> u16 {
        use self::ResolvedLayout::*;
        match &*self.0 {
            Text(text) => text.len() as u16,
            Horiz(left, right) => left.size() + right.size(),
            Vert(_, bottom) => bottom.size(),
        }
    }
}