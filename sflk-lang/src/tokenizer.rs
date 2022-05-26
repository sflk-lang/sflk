use crate::scu::{Loc, SourceCodeUnit};
use std::rc::Rc;

#[derive(Debug)]
pub struct CharReadingHead {
	scu: Rc<SourceCodeUnit>,
	raw_index: usize,
	line: usize,
}

impl CharReadingHead {
	pub fn from_scu(scu: Rc<SourceCodeUnit>) -> CharReadingHead {
		CharReadingHead { scu, raw_index: 0, line: 1 }
	}
}

impl CharReadingHead {
	fn peek(&self) -> Option<char> {
		self.scu.content[self.raw_index..].chars().next()
	}

	fn disc(&mut self) {
		if let Some(ch) = self.peek() {
			self.raw_index += ch.len_utf8();
			if ch == '\n' {
				self.line += 1;
			}
		}
	}

	fn loc(&self) -> Loc {
		Loc {
			scu: Rc::clone(&self.scu),
			line_start: self.line,
			raw_index_start: self.raw_index,
			raw_length: match self.peek() {
				Some(ch) => ch.len_utf8(),
				None => 0,
			},
		}
	}

	fn skip_ws(&mut self) {
		loop {
			match self.peek() {
				Some(ch) if ch.is_ascii_whitespace() => self.disc(),
				_ => break,
			}
		}
	}
}

impl CharReadingHead {
	pub fn scu(&self) -> Rc<SourceCodeUnit> {
		Rc::clone(&self.scu)
	}
}

#[derive(Debug, Clone)]
pub enum Tok {
	Name {
		string: String,
		unstable_warning: bool,
	},
	Kw(Kw),
	Integer(String),
	String {
		content: String,
		no_end_quote_warning: bool,
		invalid_escape_sequence_errors: Vec<(EscapeSequenceError, usize)>,
		// The usize is the `\` character offset in literal
	},
	BinOp(BinOp),
	Left(Matched),
	Right(Matched),
	StmtBinOp(StmtBinOp),
	InvalidCharacter(char),
	Comment {
		content: String,
		delimitation_thickness: usize,
		no_end_hash_warning: bool,
	},
	Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EscapeSequenceError {
	InvalidFirstCharacter(char),
	InvalidDigitCharacter(char),
	UnexpectedEof,
	InvalidUnicodeCodePoint(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Kw {
	Np,
	Pr,
	Nl,
	Do,
	Dh,
	Fh,
	Ev,
	If,
	Th,
	El,
	Lp,
	Wh,
	Bd,
	Sp,
	Ri,
	Em,
	Rs,
}

#[derive(Debug, Clone)]
pub enum BinOp {
	Plus,
	Minus,
	Star,
	Slash,
	Comma,
	DoubleComma,
	Dot,
	ToRight,
}

#[derive(Debug, Clone)]
pub enum Matched {
	Paren,
	Curly,
	Bracket,
}

#[derive(Debug, Clone)]
pub enum StmtBinOp {
	ToLeft,
}

pub struct Tokenizer {}

impl Tokenizer {
	pub fn new() -> Tokenizer {
		Tokenizer {}
	}
}

impl Tokenizer {
	pub fn pop_tok(&mut self, crh: &mut CharReadingHead) -> (Tok, Loc) {
		crh.skip_ws();
		let loc = crh.loc();
		match crh.peek() {
			Some(ch) if ch.is_ascii_alphabetic() => {
				let (word_string, word_loc) = self.pop_word(crh);
				(self.word_to_tok(word_string), word_loc)
			},
			Some(ch) if ch.is_ascii_digit() => {
				let (integer_string, word_loc) = self.pop_integer(crh);
				(Tok::Integer(integer_string), word_loc)
			},
			Some('\"') => self.pop_string_tok(crh),
			Some('+') => {
				crh.disc();
				(Tok::BinOp(BinOp::Plus), loc)
			},
			Some('-') => {
				crh.disc();
				(Tok::BinOp(BinOp::Minus), loc)
			},
			Some('*') => {
				crh.disc();
				(Tok::BinOp(BinOp::Star), loc)
			},
			Some('/') => {
				crh.disc();
				(Tok::BinOp(BinOp::Slash), loc)
			},
			Some(',') => {
				crh.disc();
				let loc2 = crh.loc();
				if crh.peek() == Some(',') {
					crh.disc();
					(Tok::BinOp(BinOp::DoubleComma), loc + loc2)
				} else {
					(Tok::BinOp(BinOp::Comma), loc)
				}
			},
			Some('.') => {
				crh.disc();
				(Tok::BinOp(BinOp::Dot), loc)
			},
			Some('>') => {
				crh.disc();
				(Tok::BinOp(BinOp::ToRight), loc)
			},
			Some('(') => {
				crh.disc();
				(Tok::Left(Matched::Paren), loc)
			},
			Some('[') => {
				crh.disc();
				(Tok::Left(Matched::Bracket), loc)
			},
			Some('{') => {
				crh.disc();
				(Tok::Left(Matched::Curly), loc)
			},
			Some(')') => {
				crh.disc();
				(Tok::Right(Matched::Paren), loc)
			},
			Some(']') => {
				crh.disc();
				(Tok::Right(Matched::Bracket), loc)
			},
			Some('}') => {
				crh.disc();
				(Tok::Right(Matched::Curly), loc)
			},
			Some('<') => {
				crh.disc();
				(Tok::StmtBinOp(StmtBinOp::ToLeft), loc)
			},
			Some('#') => self.pop_comment_tok(crh),
			Some(ch) => {
				crh.disc();
				(Tok::InvalidCharacter(ch), loc)
			},
			None => (Tok::Eof, loc),
		}
	}

	fn pop_word(&mut self, crh: &mut CharReadingHead) -> (String, Loc) {
		let mut word_string = String::new();
		let mut loc = crh.loc();
		while let Some(ch) = crh.peek() {
			if !ch.is_ascii_alphabetic() {
				break;
			}
			word_string.push(ch);
			crh.disc();
		}
		std::assert!(!word_string.is_empty());
		loc.raw_length = word_string.bytes().len();
		(word_string, loc)
	}

	fn word_to_tok(&self, word: String) -> Tok {
		match &word[..] {
			"np" => Tok::Kw(Kw::Np),
			"pr" => Tok::Kw(Kw::Pr),
			"nl" => Tok::Kw(Kw::Nl),
			"do" => Tok::Kw(Kw::Do),
			"dh" => Tok::Kw(Kw::Dh),
			"fh" => Tok::Kw(Kw::Fh),
			"ev" => Tok::Kw(Kw::Ev),
			"if" => Tok::Kw(Kw::If),
			"th" => Tok::Kw(Kw::Th),
			"el" => Tok::Kw(Kw::El),
			"lp" => Tok::Kw(Kw::Lp),
			"wh" => Tok::Kw(Kw::Wh),
			"bd" => Tok::Kw(Kw::Bd),
			"sp" => Tok::Kw(Kw::Sp),
			"ri" => Tok::Kw(Kw::Ri),
			"em" => Tok::Kw(Kw::Em),
			"rs" => Tok::Kw(Kw::Rs),
			_ => {
				let len = word.len();
				Tok::Name { string: word, unstable_warning: len == 2 }
			},
		}
	}

	fn pop_integer(&mut self, crh: &mut CharReadingHead) -> (String, Loc) {
		let mut integer_string = String::new();
		let mut loc = crh.loc();
		while let Some(ch) = crh.peek() {
			if !ch.is_ascii_digit() {
				break;
			}
			integer_string.push(ch);
			crh.disc();
		}
		std::assert!(!integer_string.is_empty());
		loc.raw_length = integer_string.bytes().len();
		(integer_string, loc)
	}

	fn pop_string_tok(&mut self, crh: &mut CharReadingHead) -> (Tok, Loc) {
		let mut content = String::new();
		let mut no_end_quote_warning = false;
		let mut invalid_escape_sequence_errors: Vec<(EscapeSequenceError, usize)> = Vec::new();
		let mut offset = 0;
		let mut loc = crh.loc();
		std::assert_eq!(crh.peek(), Some('\"'));
		crh.disc();
		loop {
			match crh.peek() {
				None => {
					no_end_quote_warning = true;
					break;
				},
				Some('\"') => {
					loc += crh.loc();
					crh.disc();
					break;
				},
				Some('\\') => {
					let (escaped, len, escaped_loc) = self.pop_escaped(crh);
					loc += escaped_loc;
					match escaped {
						Ok(escaped_string) => {
							content += &escaped_string;
						},
						Err(error) => {
							content += "�";
							invalid_escape_sequence_errors.push((error, offset));
						},
					}
					offset += len;
				},
				Some(ch) => {
					loc += crh.loc();
					offset += 1;
					crh.disc();
					content.push(ch);
				},
			}
		}
		(
			Tok::String {
				content,
				no_end_quote_warning,
				invalid_escape_sequence_errors,
			},
			loc,
		)
	}

	fn pop_escaped(
		&mut self,
		crh: &mut CharReadingHead,
	) -> (Result<String, EscapeSequenceError>, usize, Loc) {
		let loc_beg = crh.loc();
		std::assert_eq!(crh.peek(), Some('\\'));
		crh.disc();
		match crh.peek() {
			Some('\n') => {
				let loc_end = crh.loc();
				crh.disc();
				(Ok("".to_string()), 2, loc_beg + loc_end)
			},
			Some('\\') => {
				let loc_end = crh.loc();
				crh.disc();
				(Ok("\\".to_string()), 2, loc_beg + loc_end)
			},
			Some('\"') => {
				let loc_end = crh.loc();
				crh.disc();
				(Ok("\"".to_string()), 2, loc_beg + loc_end)
			},
			Some('?') => {
				let loc_end = crh.loc();
				crh.disc();
				(Ok("�".to_string()), 2, loc_beg + loc_end)
			},
			Some('n') => {
				let loc_end = crh.loc();
				crh.disc();
				(Ok("\n".to_string()), 2, loc_beg + loc_end)
			},
			Some('t') => {
				let loc_end = crh.loc();
				crh.disc();
				(Ok("\t".to_string()), 2, loc_beg + loc_end)
			},
			Some('e') => {
				let loc_end = crh.loc();
				crh.disc();
				(Ok("\x1b".to_string()), 2, loc_beg + loc_end)
			},
			Some('a') => {
				let loc_end = crh.loc();
				crh.disc();
				(Ok("\x07".to_string()), 2, loc_beg + loc_end)
			},
			Some('b') => {
				let loc_end = crh.loc();
				crh.disc();
				(Ok("\x08".to_string()), 2, loc_beg + loc_end)
			},
			Some('v') => {
				let loc_end = crh.loc();
				crh.disc();
				(Ok("\x0b".to_string()), 2, loc_beg + loc_end)
			},
			Some('f') => {
				let loc_end = crh.loc();
				crh.disc();
				(Ok("\x0c".to_string()), 2, loc_beg + loc_end)
			},
			Some('r') => {
				let loc_end = crh.loc();
				crh.disc();
				(Ok("\r".to_string()), 2, loc_beg + loc_end)
			},
			Some('x') | Some('d') => {
				let (escaped, len, loc_end) = self.pop_hex_escaped(crh);
				(escaped, len + 1, loc_beg + loc_end)
			},
			Some(ch) => (
				Err(EscapeSequenceError::InvalidFirstCharacter(ch)),
				1,
				loc_beg,
			),
			None => (Err(EscapeSequenceError::UnexpectedEof), 1, loc_beg),
		}
	}

	fn pop_hex_escaped(
		&mut self,
		crh: &mut CharReadingHead,
	) -> (Result<String, EscapeSequenceError>, usize, Loc) {
		let mut loc = crh.loc();
		let mut len = 1;
		let base = {
			match crh.peek() {
				Some('x') => {
					crh.disc();
					16
				},
				Some('d') => {
					crh.disc();
					10
				},
				_ => unreachable!(),
			}
		};
		let mut character_code = 0;
		if crh.peek() == Some('(') {
			loc += crh.loc();
			len += 1;
			crh.disc();
			loop {
				if let Some(ch) = crh.peek() {
					match ch.to_digit(base) {
						Some(digit) => {
							loc += crh.loc();
							len += 1;
							crh.disc();
							character_code = character_code * base + digit;
						},
						None if ch == ')' => {
							loc += crh.loc();
							len += 1;
							crh.disc();
							break;
						},
						None => {
							return (
								Err(EscapeSequenceError::InvalidDigitCharacter(ch)),
								len,
								loc,
							)
						},
					}
				} else {
					return (Err(EscapeSequenceError::UnexpectedEof), len, loc);
				}
			}
		} else {
			for _ in 0..2 {
				if let Some(ch) = crh.peek() {
					match ch.to_digit(base) {
						Some(digit) => {
							loc += crh.loc();
							len += 1;
							crh.disc();
							character_code = character_code * base + digit;
						},
						None => {
							return (
								Err(EscapeSequenceError::InvalidDigitCharacter(ch)),
								len,
								loc,
							)
						},
					}
				} else {
					return (Err(EscapeSequenceError::UnexpectedEof), len, loc);
				}
			}
		}
		if let Some(ch) = std::char::from_u32(character_code) {
			(Ok(ch.to_string()), len, loc)
		} else {
			(
				Err(EscapeSequenceError::InvalidUnicodeCodePoint(character_code)),
				len,
				loc,
			)
		}
	}

	fn pop_comment_tok(&mut self, crh: &mut CharReadingHead) -> (Tok, Loc) {
		assert_eq!(crh.peek(), Some('#'));
		let mut loc = crh.loc();
		let mut delimitation_thickness = 0;
		while let Some('#') = crh.peek() {
			delimitation_thickness += 1;
			loc += crh.loc();
			crh.disc();
		}
		let delimitation_thickness = delimitation_thickness;
		let mut content = String::new();
		let mut no_end_hash_warning = false;
		loop {
			if let Some('#') = crh.peek() {
				let mut hashes_thickness = 0;
				while let Some('#') = crh.peek() {
					hashes_thickness += 1;
					loc += crh.loc();
					crh.disc();
				}
				if hashes_thickness == delimitation_thickness {
					break;
				} else {
					content.extend(std::iter::repeat('#').take(hashes_thickness));
				}
			} else if let Some(ch) = crh.peek() {
				content.push(ch);
				loc += crh.loc();
				crh.disc();
			} else {
				no_end_hash_warning = true;
				break;
			}
		}
		(
			Tok::Comment {
				content,
				delimitation_thickness,
				no_end_hash_warning,
			},
			loc,
		)
	}
}
