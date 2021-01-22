
#[derive(Debug)]
pub struct SourceCodeUnit {
	name: String,
	content: String,
	line_offsets: Vec<usize>,
}

impl SourceCodeUnit {
	pub fn from_filename(filename: &str) -> SourceCodeUnit {
		let src = std::fs::read_to_string(filename)
			.expect(&format!("source file `{}` couldn't be read", filename));
		SourceCodeUnit::from_str(&src, filename.to_string())
	}

	pub fn from_str(s: &str, name: String) -> SourceCodeUnit {
		let line_offsets_iter = s.bytes()
			.enumerate()
			.filter_map(|(i, ch)|
				if ch as char == '\n' {
					Some(i+1)
				} else {
					None 
				});
		let mut line_offsets: Vec<usize> = Some(0usize).into_iter()
			.chain(line_offsets_iter)
			.collect();
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


#[derive(Debug)]
pub enum TokenizingError {
	EofInComment {loc: Loc},
	EofInString {loc: Loc},
	EofInEscapeSequence {loc: Loc},
	UnexpectedCharacter {ch: char, loc: Loc},
	InvalidEscapeSequence {sequence: String, loc: Loc},
}

impl std::fmt::Display for TokenizingError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			TokenizingError::EofInComment {loc} =>
				write!(f, "end-of-file in comment started at line {}",
					loc.line_start),
			TokenizingError::EofInString {loc} =>
				write!(f, "end-of-file in string literal started at line {}",
					loc.line_start),
			TokenizingError::EofInEscapeSequence {loc} =>
				write!(f, "end-of-file in escape sequence at line {}",
					loc.line_start),
			TokenizingError::UnexpectedCharacter {ch, loc} =>
				write!(f, "unexpected character `{}` at line {}",
					ch, loc.line_start),
			TokenizingError::InvalidEscapeSequence {sequence, loc} =>
				write!(f, "invalid escape sequence `{}` at line {}",
					sequence, loc.line_start),
		}
	}
}


use std::rc::Rc;

#[derive(Debug)]
pub struct TokReadingHead {
	scu: Rc<SourceCodeUnit>,
	raw_index: usize,
	line: usize,
}

#[derive(Debug, Clone)]
pub struct Loc {
	scu: Rc<SourceCodeUnit>,
	line_start: usize,
	raw_index_start: usize,
	raw_length: usize,
}

impl Loc {
	pub fn line(&self) -> usize {
		self.line_start
	}
}

use std::ops::{Add, AddAssign};

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

impl TokReadingHead {
	pub fn from_scu(scu: Rc<SourceCodeUnit>) -> TokReadingHead {
		TokReadingHead {
			scu,
			raw_index: 0,
			line: 1,
		}
	}

	fn peek_cur_char(&self) -> Option<char> {
		self.scu.content[self.raw_index..].chars().next()
	}

	fn goto_next_char(&mut self) {
		if let Some(ch) = self.peek_cur_char() {
			self.raw_index += ch.len_utf8();
			match ch {
				'\n' => self.line += 1,
				_ => (),
			}
		}
	}

	fn cur_char_loc(&self) -> Loc {
		Loc {
			scu: Rc::clone(&self.scu),
			line_start: self.line,
			raw_index_start: self.raw_index,
			raw_length: match self.peek_cur_char() {
				Some(ch) => ch.len_utf8(),
				None => 0,
			},
		}
	}

	fn skip_ws(&mut self) -> Result<(), TokenizingError> {
		let mut comment: Option<Loc> = None;
		loop {
			match (self.peek_cur_char(), &comment) {
				(Some('#'), None) =>
					comment = Some(self.cur_char_loc()),
				(Some(ch), None) if !ch.is_ascii_whitespace() =>
					break,
				(Some('#'), Some(_)) =>
					comment = None,
				(None, Some(comment_loc)) =>
					return Err(TokenizingError::EofInComment {
						loc: comment_loc.clone()
					}),
				(None, None) =>
					break,
				_ => (),
			}
			self.goto_next_char();
		}
		Ok(())
	}
}

#[derive(Debug, Clone)]
pub enum Tok {
	Word(String),
	Integer(String),
	String(String),
	BinOp(String),
	Left(String),
	Right(String),
	ToLeft,
	ToLeftTilde,
	Void,
}

impl std::fmt::Display for Tok {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Tok::Word(s) => write!(f, "{}", s),
			Tok::Integer(s) => write!(f, "{}", s),
			Tok::String(s) => write!(f, "\"{}\"", s),
			Tok::BinOp(s) => write!(f, "{}", s),
			Tok::Left(s) => write!(f, "{}", s),
			Tok::Right(s) => write!(f, "{}", s),
			Tok::ToLeft => write!(f, "<"),
			Tok::ToLeftTilde => write!(f, "<~"),
			Tok::Void => write!(f, ""),
		}
	}
}

impl Tok {
	pub fn is_bin_op(&self) -> bool {
		match self {
			Tok::BinOp(_) => true,
			_ => false,
		}
	}
}

impl TokReadingHead {
	pub fn read_cur_tok(&mut self) -> Result<(Tok, Loc), TokenizingError> {
		self.skip_ws()?;
		match self.peek_cur_char() {
			Some(ch) if ch.is_ascii_alphabetic() => {
				let (word, loc) = self.read_cur_word();
				Ok((Tok::Word(word), loc))
			},
			Some(ch) if ch.is_ascii_digit() => {
				let (integer, loc) = self.read_cur_integer();
				Ok((Tok::Integer(integer), loc))
			},
			Some(ch) if ch == '\"' => {
				let (string, loc) = self.read_cur_string()?;
				Ok((Tok::String(string), loc))
			},
			Some(ch) if ch == '+' || ch == '-' || ch == '*' || ch == '/' => {
				self.goto_next_char();
				Ok((Tok::BinOp(ch.to_string()), self.cur_char_loc()))
			},
			Some(ch) if ch == '(' || ch == '[' || ch == '{' => {
				self.goto_next_char();
				Ok((Tok::Left(ch.to_string()), self.cur_char_loc()))
			},
			Some(ch) if ch == ')' || ch == ']' || ch == '}' => {
				self.goto_next_char();
				Ok((Tok::Right(ch.to_string()), self.cur_char_loc()))
			},
			Some(ch) if ch == '<' => {
				self.goto_next_char();
				match self.peek_cur_char() {
					Some('~') => {
						self.goto_next_char();
						Ok((Tok::ToLeftTilde, self.cur_char_loc()))
					},
					_ => Ok((Tok::ToLeft, self.cur_char_loc())),
				}
			},
			Some(ch) => Err(TokenizingError::UnexpectedCharacter {
				ch, loc: self.cur_char_loc(),
			}),
			None => Ok((Tok::Void, self.cur_char_loc())),
		}
	}

	fn read_cur_word(&mut self) -> (String, Loc) {
		let mut word_string = String::new();
		let mut loc = self.cur_char_loc();
		while let Some(ch) = self.peek_cur_char() {
			if !ch.is_ascii_alphabetic() {
				break;
			}
			word_string.push(ch);
			self.goto_next_char();
		}
		std::assert!(word_string.len() >= 1);
		loc.raw_length = word_string.bytes().len();
		(word_string, loc)
	}

	fn read_cur_integer(&mut self) -> (String, Loc) {
		let mut integer_string = String::new();
		let mut loc = self.cur_char_loc();
		while let Some(ch) = self.peek_cur_char() {
			if !ch.is_ascii_digit() {
				break;
			}
			integer_string.push(ch);
			self.goto_next_char();
		}
		std::assert!(integer_string.len() >= 1);
		loc.raw_length = integer_string.bytes().len();
		(integer_string, loc)
	}

	fn read_cur_string(&mut self) -> Result<(String, Loc), TokenizingError> {
		let mut string_string = String::new();
		let mut loc = self.cur_char_loc();
		std::assert_eq!(self.peek_cur_char(), Some('\"'));
		self.goto_next_char();
		loop {
			match self.peek_cur_char() {
				None => {
					loc.raw_length = string_string.bytes().len()+1;
					return Err(TokenizingError::EofInString {loc});
				},
				Some('\"') => {
					self.goto_next_char();
					break;
				},
				Some('\\') => {
					string_string += &self.read_cur_escape_sequence()?.0;
				},
				Some(ch) => {
					self.goto_next_char();
					string_string.push(ch);
				},
			}
		}
		loc.raw_length = string_string.bytes().len()+2;
		Ok((string_string, loc))
	}

	fn read_cur_escape_sequence(&mut self) -> Result<(String, Loc), TokenizingError> {
		let loc_beg = self.cur_char_loc();
		std::assert_eq!(self.peek_cur_char(), Some('\\'));
		self.goto_next_char();
		match self.peek_cur_char() {
			Some('\n') => {self.goto_next_char(); Ok(("".to_string(), loc_beg))},
			Some('\\') => {self.goto_next_char(); Ok(("\\".to_string(), loc_beg))},
			Some('\"') => {self.goto_next_char(); Ok(("\"".to_string(), loc_beg))},
			Some('n') => {self.goto_next_char(); Ok(("\n".to_string(), loc_beg))},
			Some('t') => {self.goto_next_char(); Ok(("\t".to_string(), loc_beg))},
			Some('e') => {self.goto_next_char(); Ok(("\x1b".to_string(), loc_beg))},
			Some('a') => {self.goto_next_char(); Ok(("\x07".to_string(), loc_beg))},
			Some('b') => {self.goto_next_char(); Ok(("\x08".to_string(), loc_beg))},
			Some('v') => {self.goto_next_char(); Ok(("\x0b".to_string(), loc_beg))},
			Some('f') => {self.goto_next_char(); Ok(("\x0c".to_string(), loc_beg))},
			Some('r') => {self.goto_next_char(); Ok(("\r".to_string(), loc_beg))},
			Some('x') => {
				/* TODO:
				 * put this in an other function 
				 * and use idiomatic rust comment style */
				self.goto_next_char();
				let ch1 = self.peek_cur_char();
				self.goto_next_char();
				let ch2 = self.peek_cur_char();
				let loc = loc_beg + self.cur_char_loc();
				self.goto_next_char();
				match (ch1, ch2) {
					(Some(ch1), Some(ch2)) => {
						let digit1 = ch1.to_digit(16);
						let digit2 = ch2.to_digit(16);
						if digit1.is_none() || digit2.is_none() {
							return Err(TokenizingError::InvalidEscapeSequence {
								sequence: format!("x{}{}", ch1, ch2), loc})
						} else {
							let value = digit1.unwrap() * 16 + digit2.unwrap();
							Ok(((value as u8 as char).to_string(), loc))
						}
					},
					(None, _) | (_, None) => Err(TokenizingError::EofInEscapeSequence {loc}),
				}
			},
			Some(ch) => Err(TokenizingError::InvalidEscapeSequence {
				sequence: ch.to_string(), loc: loc_beg}),
			None => Err(TokenizingError::EofInEscapeSequence {loc: loc_beg}),
		}
	}
}
