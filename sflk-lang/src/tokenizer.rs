use crate::{
	scu::{Loc, SourceCodeUnit},
	utils::{escape_string, styles},
};
use std::{collections::VecDeque, convert::TryFrom, fmt, rc::Rc};

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

#[derive(PartialEq, Eq, Hash)]
pub enum SimpleTok {
	Kw(Kw),
	Op(Op),
}

impl TryFrom<&Tok> for SimpleTok {
	type Error = ();

	fn try_from(tok: &Tok) -> Result<SimpleTok, Self::Error> {
		match tok {
			Tok::Kw(kw) => Ok(SimpleTok::Kw(*kw)),
			Tok::Op(op) => Ok(SimpleTok::Op(*op)),
			_ => Err(()),
		}
	}
}

#[derive(Debug, Clone)]
pub enum Tok {
	Kw(Kw),
	Op(Op),
	Left(Matched),
	Right(Matched),
	Name {
		string: String,
		unstable_warning: bool,
	},
	Integer(String),
	String {
		content: String,
		no_end_quote_warning: bool,
		invalid_escape_sequence_errors: Vec<(EscapeSequenceError, usize)>,
		// The usize is the `\` character index in the literal.
	},
	InvalidCharacter(char),
	CommentBlock {
		content: String,
		delimitation_thickness: usize,
		no_end_hash_warning: bool,
	},
	CommentLine {
		content: String,
	},
	Eof,
}

#[derive(Debug, Clone)]
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
	Ao,
	Ri,
	Em,
	Rs,
	Fi,
	In,
	Ix,
	Cx,
	Cy,
	Wi,
	Od,
	Os,
	Ln,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Op {
	Plus,
	Minus,
	Star,
	Slash,
	Comma,
	DoubleComma,
	Dot,
	ToRight,
	ToLeft,
	Bang,
}

#[derive(Debug, Clone)]
pub enum Matched {
	Paren,
	Curly,
	Bracket,
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
				(Tok::Op(Op::Plus), loc)
			},
			Some('-') => {
				crh.disc();
				(Tok::Op(Op::Minus), loc)
			},
			Some('*') => {
				crh.disc();
				(Tok::Op(Op::Star), loc)
			},
			Some('/') => {
				crh.disc();
				(Tok::Op(Op::Slash), loc)
			},
			Some(',') => {
				crh.disc();
				let loc2 = crh.loc();
				if crh.peek() == Some(',') {
					crh.disc();
					(Tok::Op(Op::DoubleComma), loc + loc2)
				} else {
					(Tok::Op(Op::Comma), loc)
				}
			},
			Some('.') => {
				crh.disc();
				(Tok::Op(Op::Dot), loc)
			},
			Some('>') => {
				crh.disc();
				(Tok::Op(Op::ToRight), loc)
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
				(Tok::Op(Op::ToLeft), loc)
			},
			Some('!') => {
				crh.disc();
				(Tok::Op(Op::Bang), loc)
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
			"ao" => Tok::Kw(Kw::Ao),
			"ri" => Tok::Kw(Kw::Ri),
			"em" => Tok::Kw(Kw::Em),
			"rs" => Tok::Kw(Kw::Rs),
			"fi" => Tok::Kw(Kw::Fi),
			"in" => Tok::Kw(Kw::In),
			"ix" => Tok::Kw(Kw::Ix),
			"cx" => Tok::Kw(Kw::Cx),
			"cy" => Tok::Kw(Kw::Cy),
			"wi" => Tok::Kw(Kw::Wi),
			"od" => Tok::Kw(Kw::Od),
			"os" => Tok::Kw(Kw::Os),
			"ln" => Tok::Kw(Kw::Ln),
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
		let mut comment_line = false;
		while let Some('#') = crh.peek() {
			delimitation_thickness += 1;
			loc += crh.loc();
			crh.disc();
			if crh.peek() == Some('!') && delimitation_thickness == 1 {
				comment_line = true;
				break;
			}
		}
		let delimitation_thickness = delimitation_thickness;
		let comment_line = comment_line;
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
				if !comment_line && hashes_thickness == delimitation_thickness {
					break;
				} else {
					content.extend(std::iter::repeat('#').take(hashes_thickness));
				}
			} else if let Some(ch) = crh.peek() {
				content.push(ch);
				loc += crh.loc();
				crh.disc();
				if comment_line && ch == '\n' {
					break;
				}
			} else {
				no_end_hash_warning = true;
				break;
			}
		}
		if comment_line {
			(Tok::CommentLine { content }, loc)
		} else {
			(
				Tok::CommentBlock {
					content,
					delimitation_thickness,
					no_end_hash_warning,
				},
				loc,
			)
		}
	}
}

pub struct TokBuffer {
	crh: CharReadingHead,
	tokenizer: Tokenizer,
	toks_ahead: VecDeque<(Tok, Loc)>,
}

impl TokBuffer {
	pub fn from(crh: CharReadingHead) -> TokBuffer {
		TokBuffer {
			crh,
			tokenizer: Tokenizer::new(),
			toks_ahead: VecDeque::new(),
		}
	}

	pub fn scu(&self) -> Rc<SourceCodeUnit> {
		self.crh.scu()
	}

	fn tokenizer_pop_tok_no_comments(&mut self) -> (Tok, Loc) {
		loop {
			let (tok, loc) = self.tokenizer.pop_tok(&mut self.crh);
			if !matches!(tok, Tok::CommentBlock { .. } | Tok::CommentLine { .. }) {
				break (tok, loc);
			}
		}
	}

	pub fn prepare_max_index(&mut self, n: usize) {
		if self.toks_ahead.len() < n + 1 {
			self.toks_ahead.reserve(n - self.toks_ahead.len());
		}
		while self.toks_ahead.len() < n + 1 {
			let (tok, loc) = self.tokenizer_pop_tok_no_comments();
			self.toks_ahead.push_back((tok, loc));
		}
	}

	pub fn peek(&mut self, n: usize) -> &(Tok, Loc) {
		self.prepare_max_index(n);
		&self.toks_ahead[n]
	}

	pub fn pop(&mut self) -> (Tok, Loc) {
		self.peek(0);
		let tok_loc_opt = self.toks_ahead.pop_front();
		if let Some(tok_loc) = tok_loc_opt {
			tok_loc
		} else {
			panic!("bug: no token to pop")
		}
	}

	pub fn display_all(mut self, line_numbers: bool) {
		let mut last_line = 0;
		loop {
			let (tok, loc) = self.tokenizer.pop_tok(&mut self.crh);
			if line_numbers && last_line < loc.line_start {
				println!("Line {}:", loc.line_start);
				last_line = loc.line_start;
			}
			if line_numbers {
				print!("\t");
			}
			match tok {
				Tok::InvalidCharacter(_) => println!("\x1b[31m{}\x1b[39m", tok),
				_ => println!("{}", tok),
			}
			if matches!(tok, Tok::Eof) {
				break;
			}
		}
	}
}

impl fmt::Display for Tok {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Tok::Op(Op::Plus) => write!(f, "operator +"),
			Tok::Op(Op::Minus) => write!(f, "operator -"),
			Tok::Op(Op::Star) => write!(f, "operator *"),
			Tok::Op(Op::Slash) => write!(f, "operator /"),
			Tok::Op(Op::ToRight) => write!(f, "operator >"),
			Tok::Op(Op::Comma) => write!(f, "operator ,"),
			Tok::Op(Op::DoubleComma) => write!(f, "operator ,,"),
			Tok::Op(Op::Dot) => write!(f, "operator ."),
			Tok::Op(Op::ToLeft) => write!(f, "operator <"),
			Tok::Op(Op::Bang) => write!(f, "operator !"),
			Tok::Left(Matched::Paren) => write!(f, "left parenthesis"),
			Tok::Left(Matched::Curly) => write!(f, "left curly bracket"),
			Tok::Left(Matched::Bracket) => write!(f, "left bracket"),
			Tok::Right(Matched::Paren) => write!(f, "right parenthesis"),
			Tok::Right(Matched::Curly) => write!(f, "right curly bracket"),
			Tok::Right(Matched::Bracket) => write!(f, "right bracket"),
			Tok::CommentBlock { .. } => write!(f, "comment block"),
			Tok::CommentLine { .. } => write!(f, "comment line"),
			Tok::Kw(kw) => write!(f, "keyword {}", kw),
			Tok::Integer(string) => write!(f, "integer {}", string),
			Tok::Name { string, .. } => write!(f, "name {}", string),
			Tok::String { content, .. } => {
				write!(f, "string \"{}\"", escape_string(content, &styles::NORMAL))
			},
			Tok::InvalidCharacter(c) => write!(f, "invalid character {}", c),
			Tok::Eof => write!(f, "end-of-file"),
		}
	}
}

impl fmt::Display for Kw {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Kw::Np => write!(f, "np"),
			Kw::Pr => write!(f, "pr"),
			Kw::Nl => write!(f, "nl"),
			Kw::Do => write!(f, "do"),
			Kw::Dh => write!(f, "dh"),
			Kw::Fh => write!(f, "fh"),
			Kw::Ev => write!(f, "ev"),
			Kw::If => write!(f, "if"),
			Kw::Th => write!(f, "th"),
			Kw::El => write!(f, "el"),
			Kw::Lp => write!(f, "lp"),
			Kw::Wh => write!(f, "wh"),
			Kw::Bd => write!(f, "bd"),
			Kw::Sp => write!(f, "sp"),
			Kw::Ao => write!(f, "ao"),
			Kw::Ri => write!(f, "ri"),
			Kw::Em => write!(f, "em"),
			Kw::Rs => write!(f, "rs"),
			Kw::Fi => write!(f, "fi"),
			Kw::In => write!(f, "in"),
			Kw::Ix => write!(f, "ix"),
			Kw::Cx => write!(f, "cx"),
			Kw::Cy => write!(f, "cy"),
			Kw::Wi => write!(f, "wi"),
			Kw::Od => write!(f, "od"),
			Kw::Os => write!(f, "os"),
			Kw::Ln => write!(f, "ln"),
		}
	}
}
