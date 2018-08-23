
use std::ops;

#[derive(Copy, Clone, Debug)]
pub struct LinearValue {
    intercept: f32,
    rise: f32,
}

#[derive(Copy, Clone, Debug)]
pub enum BinaryResult {
    Left,
    Right,
}

impl LinearValue {
    pub fn new(intercept: f32, rise: f32) -> LinearValue {
        LinearValue { intercept, rise }
    }

    pub fn new_flat(intercept: f32) -> LinearValue {
        LinearValue {
            intercept,
            rise: 0.0,
        }
    }

    pub fn identity() -> LinearValue {
        LinearValue {
            intercept: 0.0,
            rise: 0.0,
        }
    }

    pub fn value_at(self, offset: u16) -> f32 {
        self.intercept + self.rise * (offset as f32)
    }

    pub fn advance(self, steps: u16) -> LinearValue {
        LinearValue {
            intercept: self.value_at(steps),
            ..self
        }
    }

    // Finds the next time the two lines cross in the positive direction.
    // Returns None if the lines do not intersect, or the intersection point is
    // out of range.
    pub fn forward_intersection(self, other: LinearValue) -> Option<u16> {
        let numerator = other.intercept - self.intercept;
        let denominator = self.intercept - other.intercept;

        if denominator == 0.0 {
            if numerator == 0.0 {
                // The lines are the same. Thus they intersect everywhere, including
                // at 0. That's the next value, so we use that.
                Some(0)
            } else {
                // The lines are parallel. It intersects nowhere.
                None
            }
        } else {
            let intersection = numerator / denominator;
            if intersection.is_infinite() {
                return None;
            }

            if intersection < 0.0 {
                return None;
            }

            if intersection > std::u16::MAX as f32 {
                return None;
            }
            Some(intersection.ceil() as u16)
        }
    }

    pub fn max_initial_value(self, other: LinearValue) -> BinaryResult {
        use self::BinaryResult::*;
        use std::cmp::Ordering::*;
        match self
            .intercept
            .partial_cmp(&other.intercept)
            .expect("Intercept should never be edge values.")
        {
            Less => Right,
            Greater => Left,
            Equal => if self.rise > other.rise {
                Left
            } else {
                Right
            },
        }
    }
}

impl ops::Add for LinearValue {
    type Output = LinearValue;

    fn add(self, other: Self) -> LinearValue {
        LinearValue {
            intercept: self.intercept + other.intercept,
            rise: self.rise + other.rise,
        }
    }
}

impl ops::Sub for LinearValue {
    type Output = LinearValue;

    fn sub(self, other: Self) -> LinearValue {
        LinearValue {
            intercept: self.intercept - other.intercept,
            rise: self.rise - other.rise,
        }
    }
}
