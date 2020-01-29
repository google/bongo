// Copyright 2019 Google LLC
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

  pub fn read_char_mut(&mut self) -> Option<char> {
    let mut chars = self.as_str().char_indices();
    match chars.next() {
      None => None,
      Some((_, next_ch)) => {
        self.start_index = match chars.next() {
          None => self.end_index,
          Some((new_offset, _)) => self.start_index + new_offset,
        };

        Some(next_ch)
      }
    }
  }

  pub fn read_char(&self) -> Option<(char, BufferRange)> {
    let mut copy = self.clone();
    copy.read_char_mut().map(|ch| (ch, copy))
  }

  pub fn advance_n_mut(&mut self, n: usize) -> Option<&str> {
    let old_start = self.start_index;
    for _ in 0..n {
      if self.read_char_mut().is_none() {
        self.start_index = old_start;
        return None;
      }
    }
    Some(&self.buf.0.text[old_start..self.start_index])
  }

  pub fn advance_n(&self, n: usize) -> Option<(&str, BufferRange)> {
    let mut copy = self.clone();
    let advance_succeeded = copy.advance_n_mut(n).is_some();
    if advance_succeeded {
      let s = &self.buf.0.text[self.start_index..copy.start_index];
      Some((s, copy))
    } else {
      None
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

  pub fn start_pos(&self) -> TextPos {
    self.buf.get_text_pos_from_byte_offset(self.start_index)
  }

  pub fn end_pos(&self) -> TextPos {
    self.buf.get_text_pos_from_byte_offset(self.end_index)
  }
}

/// A key form of BufferRangeKey. Only the contents of the string can be
/// accessed through this key, so there is no confusion about location of the
/// key in the buffer, or the source name.
#[derive(Clone)]
pub struct BufferRangeKey(BufferRange);

impl BufferRangeKey {
  pub fn as_str(&self) -> &str {
    self.0.as_str()
  }
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

  fn get_line_str_from_byte_offset(
    &self,
    byte_offset: usize,
  ) -> (&str, usize, usize) {
    assert!(byte_offset <= self.0.text.len());

    let search_result = self.0.line_ranges.binary_search_by(|&(start, end)| {
      if byte_offset < start {
        Ordering::Greater
      } else if byte_offset > end {
        Ordering::Less
      } else {
        Ordering::Equal
      }
    });

    let line_range_index =
      search_result.unwrap_or_else(|insert_pos| insert_pos - 1);
    let (start_byte_index, end_byte_index) =
      self.0.line_ranges[line_range_index];
    let line = &self.0.text[start_byte_index..end_byte_index];

    let line_byte_offset = if byte_offset <= end_byte_index {
      byte_offset
    } else {
      end_byte_index
    } - start_byte_index;

    (line, line_range_index, line_byte_offset)
  }

  fn get_text_pos_from_byte_offset(&self, byte_offset: usize) -> TextPos {
    use unicode_segmentation::UnicodeSegmentation;
    let (line, line_number, line_byte_offset) =
      self.get_line_str_from_byte_offset(byte_offset);

    // Linear search within the line for the grapheme index.
    let mut col_index = 0usize;
    for (grapheme_offset, _) in line.grapheme_indices(true) {
      if grapheme_offset >= line_byte_offset {
        break;
      }
      col_index += 1;
    }

    TextPos {
      line: line_number,
      column: col_index,
      byte_offset: byte_offset,
    }
  }
}

struct BufferInner {
  source_name: String,
  text: String,
  line_ranges: Vec<(usize, usize)>,
}

#[derive(Copy, Clone)]
pub struct TextPos {
  line: usize,
  column: usize,
  byte_offset: usize,
}

impl TextPos {
  fn line(&self) -> usize {
    self.line
  }
  fn column(&self) -> usize {
    self.column
  }
  fn byte_offset(&self) -> usize {
    self.byte_offset
  }
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
  fn test_simple_read_char_mut() {
    let mut range = BufferRange::new("<test1>", "Hello\nWorld!\n");
    let ch = range.read_char_mut().unwrap();
    assert_eq!('H', ch);
    assert_eq!("ello\nWorld!\n", range.as_str());
  }

  #[test]
  fn test_unicode_read_char() {
    let range = BufferRange::new("<test>", "あいうえお");
    let (ch, next_range) = range.read_char().unwrap();
    assert_eq!('あ', ch);
    assert_eq!("いうえお", next_range.as_str());
  }

  #[test]
  fn test_unicode_read_char_mut() {
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
  fn test_simple_advance_n_mut() {
    let mut range = BufferRange::new("<test1>", "Hello\nWorld!\n");
    let prefix = range.advance_n_mut(3).unwrap();
    assert_eq!("Hel", prefix);
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

  #[test]
  fn test_simple_text_pos() {
    let range = BufferRange::new("<test1>", "Hello\nWorld!\n");
    let start = range.start_pos();
    assert_eq!(start.line(), 0);
    assert_eq!(start.column(), 0);

    let end = range.end_pos();
    assert_eq!(end.line(), 2);
    assert_eq!(end.column(), 0);
  }
}
