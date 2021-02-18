use crate::scu::{Loc, SourceCodeUnit};
use crate::utils::{escape_string, styles};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub enum TokenizingError {
	EofInComment { loc: Loc },
	EofInString { loc: Loc },
	EofInEscapeSequence { loc: Loc },
	UnexpectedCharacter { ch: char, loc: Loc },
	InvalidEscapeSequence { sequence: String, loc: Loc },
}

impl std::fmt::Display for TokenizingError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			TokenizingError::EofInComment { loc } => write!(
				f,
				"end-of-file in comment (started at line {})",
				loc.line_start
			),
			TokenizingError::EofInString { loc } => write!(
				f,
				"end-of-file in string literal (started at line {})",
				loc.line_start
			),
			TokenizingError::EofInEscapeSequence { loc } => write!(
				f,
				"end-of-file in escape sequence (at line {})",
				loc.line_start
			),
			TokenizingError::UnexpectedCharacter { ch, loc } => write!(
				f,
				"unexpected character `{}` (at line {})",
				ch, loc.line_start
			),
			TokenizingError::InvalidEscapeSequence { sequence, loc } => write!(
				f,
				"invalid escape sequence `{}` (at line {})",
				sequence, loc.line_start
			),
		}
	}
}

#[derive(Debug)]
pub struct TokReadingHead {
	scu: Rc<SourceCodeUnit>,
	raw_index: usize,
	line: usize,
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
				(Some('#'), None) => comment = Some(self.cur_char_loc()),
				(Some(ch), None) if !ch.is_ascii_whitespace() => break,
				(Some('#'), Some(_)) => comment = None,
				(None, Some(comment_loc)) => {
					return Err(TokenizingError::EofInComment {
						loc: comment_loc.clone(),
					})
				}
				(None, None) => break,
				_ => (),
			}
			self.goto_next_char();
		}
		Ok(())
	}
}

impl TokReadingHead {
	pub fn scu(&self) -> Rc<SourceCodeUnit> {
		Rc::clone(&self.scu)
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tok {
	Name(String),
	Keyword(Keyword),
	Integer(String),
	String(String),
	BinOp(BinOp),
	Left(Matched),
	Right(Matched),
	StmtBinOp(StmtBinOp),
	Void,
}

impl std::fmt::Display for Tok {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Tok::Name(s) => write!(f, "{}", s),
			Tok::Keyword(kw) => write!(f, "{}", kw),
			Tok::Integer(s) => write!(f, "{}", s),
			Tok::String(s) => write!(f, "\"{}\"", escape_string(s, &styles::UNDERLINE)),
			Tok::BinOp(op) => write!(f, "{}", op),
			Tok::Left(s) => write!(f, "{:#}", s),
			Tok::Right(s) => write!(f, "{}", s),
			Tok::StmtBinOp(op) => write!(f, "{}", op),
			Tok::Void => write!(f, ""),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
	Np,
	Pr,
	Nl,
	Do,
	Dh,
	Fh,
	Ev,
	Redo,
	End,
	Imp,
	Exp,
	If,
	El,
}

impl std::fmt::Display for Keyword {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Keyword::Np => write!(f, "{}", "np"),
			Keyword::Pr => write!(f, "{}", "pr"),
			Keyword::Nl => write!(f, "{}", "nl"),
			Keyword::Do => write!(f, "{}", "do"),
			Keyword::Dh => write!(f, "{}", "dh"),
			Keyword::Fh => write!(f, "{}", "fh"),
			Keyword::Ev => write!(f, "{}", "ev"),
			Keyword::Redo => write!(f, "{}", "redo"),
			Keyword::End => write!(f, "{}", "end"),
			Keyword::Imp => write!(f, "{}", "imp"),
			Keyword::Exp => write!(f, "{}", "exp"),
			Keyword::If => write!(f, "{}", "if"),
			Keyword::El => write!(f, "{}", "el"),
		}
	}
}

impl Tok {
	fn maybe_to_keyword(self) -> Tok {
		let keywords = {
			let mut keywords: HashMap<&str, Keyword> = HashMap::new();
			keywords.insert("np", Keyword::Np);
			keywords.insert("pr", Keyword::Pr);
			keywords.insert("nl", Keyword::Nl);
			keywords.insert("do", Keyword::Do);
			keywords.insert("dh", Keyword::Dh);
			keywords.insert("fh", Keyword::Fh);
			keywords.insert("ev", Keyword::Ev);
			keywords.insert("redo", Keyword::Redo);
			keywords.insert("end", Keyword::End);
			keywords.insert("imp", Keyword::Imp);
			keywords.insert("exp", Keyword::Exp);
			keywords.insert("if", Keyword::If);
			keywords.insert("el", Keyword::El);
			keywords
		};
		match &self {
			Tok::Name(s) => {
				if let Some(keyword) = keywords.get(s.as_str()) {
					Tok::Keyword(keyword.clone())
				} else {
					self
				}
			}
			_ => self,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOp {
	Plus,
	Minus,
	Star,
	Slash,
	ToRight,
}

impl std::fmt::Display for BinOp {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			BinOp::Plus => write!(f, "{}", "+"),
			BinOp::Minus => write!(f, "{}", "-"),
			BinOp::Star => write!(f, "{}", "*"),
			BinOp::Slash => write!(f, "{}", "/"),
			BinOp::ToRight => write!(f, "{}", ">"),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Matched {
	Paren,
	Bracket,
	Curly,
}

impl std::fmt::Display for Matched {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Matched::Paren => write!(f, "{}", if f.alternate() { "(" } else { ")" }),
			Matched::Bracket => write!(f, "{}", if f.alternate() { "[" } else { "]" }),
			Matched::Curly => write!(f, "{}", if f.alternate() { "{" } else { "}" }),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StmtBinOp {
	ToLeft,
	ToLeftTilde,
}

impl std::fmt::Display for StmtBinOp {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			StmtBinOp::ToLeft => write!(f, "{}", "<"),
			StmtBinOp::ToLeftTilde => write!(f, "{}", "<~"),
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
				Ok((Tok::Name(word).maybe_to_keyword(), loc))
			}
			Some(ch) if ch.is_ascii_digit() => {
				let (integer, loc) = self.read_cur_integer();
				Ok((Tok::Integer(integer), loc))
			}
			Some(ch) if ch == '\"' => {
				let (string, loc) = self.read_cur_string()?;
				Ok((Tok::String(string), loc))
			}
			Some(ch) if ch == '+' => {
				self.goto_next_char();
				Ok((Tok::BinOp(BinOp::Plus), self.cur_char_loc()))
			}
			Some(ch) if ch == '-' => {
				self.goto_next_char();
				Ok((Tok::BinOp(BinOp::Minus), self.cur_char_loc()))
			}
			Some(ch) if ch == '*' => {
				self.goto_next_char();
				Ok((Tok::BinOp(BinOp::Star), self.cur_char_loc()))
			}
			Some(ch) if ch == '/' => {
				self.goto_next_char();
				Ok((Tok::BinOp(BinOp::Slash), self.cur_char_loc()))
			}
			Some(ch) if ch == '>' => {
				self.goto_next_char();
				Ok((Tok::BinOp(BinOp::ToRight), self.cur_char_loc()))
			}
			Some(ch) if ch == '(' => {
				self.goto_next_char();
				Ok((Tok::Left(Matched::Paren), self.cur_char_loc()))
			}
			Some(ch) if ch == ')' => {
				self.goto_next_char();
				Ok((Tok::Right(Matched::Paren), self.cur_char_loc()))
			}
			Some(ch) if ch == '[' => {
				self.goto_next_char();
				Ok((Tok::Left(Matched::Bracket), self.cur_char_loc()))
			}
			Some(ch) if ch == ']' => {
				self.goto_next_char();
				Ok((Tok::Right(Matched::Bracket), self.cur_char_loc()))
			}
			Some(ch) if ch == '{' => {
				self.goto_next_char();
				Ok((Tok::Left(Matched::Curly), self.cur_char_loc()))
			}
			Some(ch) if ch == '}' => {
				self.goto_next_char();
				Ok((Tok::Right(Matched::Curly), self.cur_char_loc()))
			}
			Some(ch) if ch == '<' => {
				self.goto_next_char();
				match self.peek_cur_char() {
					Some('~') => {
						self.goto_next_char();
						Ok((Tok::StmtBinOp(StmtBinOp::ToLeftTilde), self.cur_char_loc()))
					}
					_ => Ok((Tok::StmtBinOp(StmtBinOp::ToLeft), self.cur_char_loc())),
				}
			}
			Some(ch) => Err(TokenizingError::UnexpectedCharacter {
				ch,
				loc: self.cur_char_loc(),
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
					loc.raw_length = string_string.bytes().len() + 1;
					return Err(TokenizingError::EofInString { loc });
				}
				Some('\"') => {
					self.goto_next_char();
					break;
				}
				Some('\\') => {
					string_string += &self.read_cur_escape_sequence()?.0;
				}
				Some(ch) => {
					self.goto_next_char();
					string_string.push(ch);
				}
			}
		}
		loc.raw_length = string_string.bytes().len() + 2;
		Ok((string_string, loc))
	}

	fn read_cur_escape_sequence(&mut self) -> Result<(String, Loc), TokenizingError> {
		let loc_beg = self.cur_char_loc();
		std::assert_eq!(self.peek_cur_char(), Some('\\'));
		self.goto_next_char();
		match self.peek_cur_char() {
			Some('\n') => {
				self.goto_next_char();
				Ok(("".to_string(), loc_beg))
			}
			Some('\\') => {
				self.goto_next_char();
				Ok(("\\".to_string(), loc_beg))
			}
			Some('\"') => {
				self.goto_next_char();
				Ok(("\"".to_string(), loc_beg))
			}
			Some('n') => {
				self.goto_next_char();
				Ok(("\n".to_string(), loc_beg))
			}
			Some('t') => {
				self.goto_next_char();
				Ok(("\t".to_string(), loc_beg))
			}
			Some('e') => {
				self.goto_next_char();
				Ok(("\x1b".to_string(), loc_beg))
			}
			Some('a') => {
				self.goto_next_char();
				Ok(("\x07".to_string(), loc_beg))
			}
			Some('b') => {
				self.goto_next_char();
				Ok(("\x08".to_string(), loc_beg))
			}
			Some('v') => {
				self.goto_next_char();
				Ok(("\x0b".to_string(), loc_beg))
			}
			Some('f') => {
				self.goto_next_char();
				Ok(("\x0c".to_string(), loc_beg))
			}
			Some('r') => {
				self.goto_next_char();
				Ok(("\r".to_string(), loc_beg))
			}
			Some('x') => {
				self.goto_next_char();
				let (ch, loc) = self.read_cur_hex_escape_sequence()?;
				Ok((ch.to_string(), loc))
			}
			Some(ch) => Err(TokenizingError::InvalidEscapeSequence {
				sequence: ch.to_string(),
				loc: loc_beg,
			}),
			None => Err(TokenizingError::EofInEscapeSequence { loc: loc_beg }),
		}
	}

	fn read_cur_hex_escape_sequence(&mut self) -> Result<(char, Loc), TokenizingError> {
		let loc_beg = self.cur_char_loc();
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
						sequence: format!("x{}{}", ch1, ch2),
						loc,
					});
				} else {
					let value = digit1.unwrap() * 16 + digit2.unwrap();
					Ok((value as u8 as char, loc))
				}
			}
			(None, _) | (_, None) => Err(TokenizingError::EofInEscapeSequence { loc }),
		}
	}
}
