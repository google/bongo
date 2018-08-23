use std::rc;
use std::ops;
use std::fmt;

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct SharedString(rc::Rc<String>);

impl SharedString {
    pub fn new(s: impl Into<String>) -> Self {
        SharedString(rc::Rc::new(s.into()))
    }
}

impl fmt::Debug for SharedString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <String as fmt::Debug>::fmt(&*self.0, f)
    }
}

impl ops::Deref for SharedString {
    type Target = str;
    fn deref(&self) -> &str { &*self.0 }
}

impl From<SharedString> for String {
    fn from(other: SharedString) -> Self {
        let SharedString(str_ref) = other;
        match rc::Rc::try_unwrap(str_ref) {
            Ok(s) => s,
            Err(str_ref) => (&*str_ref).clone(),
        }
    }
}

impl From<String> for SharedString {
    fn from(other: String) -> Self {
        SharedString::new(other)
    }
}

impl From<&str> for SharedString {
    fn from(other: &str) -> Self {
        SharedString::new(other.to_owned())
    }
}

impl AsRef<str> for SharedString {
    fn as_ref(&self) -> &str {
        &*self
    }
}