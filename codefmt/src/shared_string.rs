// Copyright 2018 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     https://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use std::fmt;
use std::ops;
use std::rc;

use unicode_width::UnicodeWidthStr;

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct SharedString(rc::Rc<String>);

impl SharedString {
  pub fn new(s: impl Into<String>) -> Self {
    SharedString(rc::Rc::new(s.into()))
  }

  pub fn as_str(&self) -> &str {
    &*self.0
  }

  pub fn to_string(&self) -> String {
    (&*self.0).clone()
  }

  pub fn into_string(self) -> String {
    let SharedString(str_ref) = self;
    match rc::Rc::try_unwrap(str_ref) {
      Ok(s) => s,
      Err(str_ref) => (&*str_ref).clone(),
    }
  }

  pub fn display_width(&self) -> u16 {
    UnicodeWidthStr::width_cjk(self.as_str()) as u16
  }
}

impl fmt::Debug for SharedString {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    <String as fmt::Debug>::fmt(&*self.0, f)
  }
}

impl ops::Deref for SharedString {
  type Target = str;
  fn deref(&self) -> &str {
    &*self.0
  }
}

impl From<SharedString> for String {
  fn from(other: SharedString) -> Self {
    other.into_string()
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
