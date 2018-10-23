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

use std::ops;

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct KnotColumn(u16);

impl KnotColumn {
  pub fn new(col: u16) -> KnotColumn {
    KnotColumn(col)
  }
  pub fn col(self) -> u16 {
    self.0
  }
}

impl ops::Add for KnotColumn {
  type Output = KnotColumn;

  fn add(self, other: Self) -> KnotColumn {
    KnotColumn(self.0.checked_add(other.0).expect("Must not overflow"))
  }
}

impl ops::Sub for KnotColumn {
  type Output = KnotColumn;

  fn sub(self, other: Self) -> KnotColumn {
    KnotColumn(self.0.checked_sub(other.0).expect("Must not overflow"))
  }
}
