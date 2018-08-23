
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
