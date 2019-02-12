//! Text buffers and related types.
use std::cmp::{self, Ordering};
use std::rc::Rc;

/// A text buffer. This is refcounted behind-the-scenes, so copies are fairly
/// cheap.
#[derive(Clone)]
struct Buffer(Rc<BufferInner>);

#[derive(Clone)]
pub struct BufferRange {
  start_index: usize,
  end_index: usize,
  buf: Buffer,
}

impl BufferRange {
  pub fn new(source_name: impl AsRef<str>, text: impl AsRef<str>) -> Self {
    let text = text.as_ref().to_string();
    let mut line_ranges = Vec::new();
    let mut line_start = Some(0usize);

    let mut char_iter = text.char_indices();

    loop {
      match char_iter.next() {
        Some((offset, ch)) => match ch {
          '\r' => {
            let mut lookahead_iter = char_iter.clone();
            if let Some((_, '\n')) = lookahead_iter.next() {
              char_iter = lookahead_iter;
            }

            if let Some(start) = line_start {
              line_ranges.push((start, offset));
            } else {
              line_ranges.push((offset, offset));
            }
            line_start = None;
          }
          '\n' => {
            if let Some(start) = line_start {
              line_ranges.push((start, offset));
            } else {
              line_ranges.push((offset, offset));
            }
            line_start = None;
          }
          _ => {
            if let None = line_start {
              line_start = Some(offset);
            }
          }
        },
        None => {
          let offset = text.len();
          if let Some(start) = line_start {
            line_ranges.push((start, offset));
          } else {
            line_ranges.push((offset, offset));
          }
          break;
        }
      }
    }
    
    let buffer = Buffer(Rc::new(BufferInner {
      source_name: source_name.as_ref().to_string(),
      text,
      line_ranges,
    }));

    BufferRange {
      start_index: 0,
      end_index: buffer.0.text.len(),
      buf: buffer.clone(),
    }
  }
  pub fn as_str(&self) -> &str {
    &self.buf.0.text[self.start_index..self.end_index]
  }
  pub fn byte_len(&self) -> usize {
    self.end_index - self.start_index
  }
  pub fn empty(&self) -> bool {
    self.start_index == self.end_index
  }

  pub fn read_char(&self) -> Option<(char, BufferRange)> {
    let mut chars = self.as_str().char_indices();
    match chars.next() {
      None => None,
      Some((_, next_ch)) => {
        let new_start_index = match chars.next() {
          None => self.end_index,
          Some((new_offset, _)) => self.start_index + new_offset,
        };

        Some((
          next_ch,
          BufferRange {
            start_index: new_start_index,
            end_index: self.end_index,
            buf: self.buf.clone(),
          },
        ))
      }
    }
  }

  /// `suffix` must be a suffix of this buf range in terms of its own range, not
  /// just in terms of its contents.
  pub fn remove_suffix(&self, suffix: &BufferRange) -> BufferRange {
    assert!(self.buf.ref_eq(&suffix.buf));
    assert_eq!(self.end_index, suffix.end_index);
    assert!(self.start_index <= suffix.start_index);
    BufferRange {
      start_index: self.start_index,
      end_index: suffix.start_index,
      buf: self.buf.clone(),
    }
  }
}

/// A key form of BufferRangeKey. Only the contents of the string can be
/// accessed through this key, so there is no confusion about location of the
/// key in the buffer, or the source name.
#[derive(Clone)]
pub struct BufferRangeKey(BufferRange);

impl BufferRangeKey {
  pub fn as_str(&self) -> &str { self.0.as_str() }
}

impl cmp::Ord for BufferRangeKey {
  fn cmp(&self, other: &Self) -> Ordering {
    self.as_str().cmp(other.as_str())
  }
}

impl cmp::PartialOrd for BufferRangeKey {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl cmp::PartialEq for BufferRangeKey {
  fn eq(&self, other: &Self) -> bool {
    self.cmp(other) == Ordering::Equal
  }
}

impl cmp::Eq for BufferRangeKey {}

impl Buffer {
  fn ref_eq(&self, other: &Buffer) -> bool {
    &*self.0 as *const BufferInner == &*other.0 as *const BufferInner
  }
}

struct BufferInner {
  source_name: String,
  text: String,
  line_ranges: Vec<(usize, usize)>,
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_simple_read_char() {
    let range = BufferRange::new("<test1>", "Hello\nWorld!\n");
    let (ch, next_range) = range.read_char().unwrap();
    assert_eq!('H', ch);
    assert_eq!("ello\nWorld!\n", next_range.as_str());
  }

  #[test]
  fn test_unicode_read_char() {
    let range = BufferRange::new("<test>", "あいうえお");
    let (ch, next_range) = range.read_char().unwrap();
    assert_eq!('あ', ch);
    assert_eq!("いうえお", next_range.as_str());
  }

  #[test]
  fn test_empty_read_char() {
    let range = BufferRange::new("<test>", "");
    assert!(range.read_char().is_none());
  }

  #[test]
  fn test_simple_remove_suffix() {
    let range = BufferRange::new("<test1>", "Hello\nWorld!\n");
    let (_, next_range) = range.read_char().unwrap();
    let (_, next_range) = next_range.read_char().unwrap();
    let (_, next_range) = next_range.read_char().unwrap();

    let prefix = range.remove_suffix(&next_range);
    assert_eq!("Hel", prefix.as_str());
  }

  #[test]
  fn test_unicode_remove_suffix() {
    let range = BufferRange::new("<test1>", "あいうえお");
    let (_, next_range) = range.read_char().unwrap();
    let (_, next_range) = next_range.read_char().unwrap();
    let (_, next_range) = next_range.read_char().unwrap();

    let prefix = range.remove_suffix(&next_range);
    assert_eq!("あいう", prefix.as_str());
  }
}