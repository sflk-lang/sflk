use std::ops::{Add, AddAssign};
use std::rc::Rc;

#[derive(Debug)]
pub struct SourceCodeUnit {
	name: String,
	pub content: String,
	line_offsets: Vec<usize>,
}

impl SourceCodeUnit {
	pub fn from_filename(filename: &str) -> SourceCodeUnit {
		let src = std::fs::read_to_string(filename)
			.expect(&format!("source file `{}` couldn't be read", filename));
		SourceCodeUnit::from_str(&src, filename.to_string())
	}

	pub fn from_str(s: &str, name: String) -> SourceCodeUnit {
		let line_offsets_iter = s.bytes().enumerate().filter_map(|(i, ch)| {
			if ch as char == '\n' {
				Some(i + 1)
			} else {
				None
			}
		});
		let mut line_offsets: Vec<usize> =
			Some(0usize).into_iter().chain(line_offsets_iter).collect();
		let mut content = s.to_string();
		if *line_offsets.last().unwrap() != content.len() {
			content += "\n";
			line_offsets.push(content.len());
			// If the content didn't end by a `\n`, then now it does.
		}
		SourceCodeUnit {
			name,
			content,
			line_offsets,
		}
	}
}

#[derive(Debug, Clone)]
pub struct Loc {
	pub scu: Rc<SourceCodeUnit>,
	pub line_start: usize,
	pub raw_index_start: usize,
	pub raw_length: usize,
}

impl Loc {
	pub fn line(&self) -> usize {
		self.line_start
	}
}

impl AddAssign for Loc {
	fn add_assign(&mut self, right: Loc) {
		std::assert_eq!(Rc::as_ptr(&self.scu), Rc::as_ptr(&right.scu));
		std::assert!(self.line_start <= right.line_start);
		std::assert!(self.raw_index_start <= right.raw_index_start);
		self.raw_length += (right.raw_index_start - self.raw_index_start) + right.raw_length;
	}
}

impl Add for Loc {
	type Output = Loc;
	fn add(mut self, right: Loc) -> Loc {
		self += right;
		self
	}
}

pub struct Located<T> {
	pub content: T,
	pub loc: Loc,
}

impl<T> Located<T> {
	pub fn rightmost_loc(mut self, loc: Loc) -> Located<T> {
		self.loc += loc;
		self
	}

	pub fn leftmost_loc(mut self, loc: Loc) -> Located<T> {
		self.loc = loc + self.loc;
		self
	}
}

impl<T> Located<T> {
	pub fn map<U>(self, func: impl FnOnce(T) -> U) -> Located<U> {
		Located {
			content: func(self.content),
			loc: self.loc,
		}
	}
}
