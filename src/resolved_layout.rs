use std::rc;

use shared_string::SharedString;

#[derive(Clone, Debug)]
enum ResolvedLayout {
    Text(SharedString),
    Horiz(ResolvedLayoutRef, ResolvedLayoutRef),
    Vert(ResolvedLayoutRef, ResolvedLayoutRef),
}

#[derive(Clone, Debug)]
pub struct ResolvedLayoutRef(std::rc::Rc<ResolvedLayout>);

impl ResolvedLayoutRef {
    fn new(inner: ResolvedLayout) -> ResolvedLayoutRef {
        ResolvedLayoutRef(rc::Rc::new(inner))
    }

    pub fn new_text(text: SharedString) -> ResolvedLayoutRef {
        ResolvedLayoutRef::new(ResolvedLayout::Text(text))
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
            Text(text) => text.to_string(),
            Horiz(left_ref, right_ref) => {
                left_ref.to_text(curr_indent) + &right_ref.to_text(curr_indent + left_ref.display_width())
            }
            Vert(top, bottom) => {
                top.to_text(curr_indent)
                    + "\n"
                    + &(" ".repeat(curr_indent as usize))
                    + &bottom.to_text(curr_indent)
            }
        }
    }

    pub fn display_width(&self) -> u16 {
        use self::ResolvedLayout::*;
        match &*self.0 {
            Text(text) => text.display_width(),
            Horiz(left, right) => left.display_width() + right.display_width(),
            Vert(_, bottom) => bottom.display_width(),
        }
    }
}
