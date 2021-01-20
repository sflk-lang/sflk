
use std::char::ToLowercase;

use tokenizer::*;

#[derive(Debug)]
pub enum ParsingError {
	TokenizingError(TokenizingError),
	UnexpectedToken {tok: Tok, loc: Loc},
}

impl From<TokenizingError> for ParsingError {
	fn from(tokenizer_error: TokenizingError) -> ParsingError {
		ParsingError::TokenizingError(tokenizer_error)
	}
}

impl std::fmt::Display for ParsingError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			ParsingError::TokenizingError(parsing_error) => write!(f, "{}", parsing_error),
			ParsingError::UnexpectedToken {tok, loc} =>
					write!(f, "unexpected token `{}` at line {}",
						tok, loc.line()),
		}
	}
}

pub struct ProgReadingHead {
	trh: TokReadingHead,
	tok_buffer: Vec<(Tok, Loc)>,
}

impl From<TokReadingHead> for ProgReadingHead {
	fn from(trh: TokReadingHead) -> ProgReadingHead {
		ProgReadingHead {
			trh,
			tok_buffer: Vec::new(),
		}
	}
}

impl ProgReadingHead {
	fn read_tok_from_trh(&mut self) -> Result<(Tok, Loc), TokenizingError> {
		self.trh.read_cur_tok()
	}

	pub fn peek_tok(&mut self) -> Result<&(Tok, Loc), ParsingError> {
		if self.tok_buffer.is_empty() {
			let tok_loc = self.read_tok_from_trh()?;
			self.tok_buffer.push(tok_loc);
		}
		Ok(self.tok_buffer.last().unwrap())
	}

	fn disc_tok(&mut self) -> Result<(), ParsingError> {
		match self.tok_buffer.pop() {
			Some(_) => (),
			None => {
				let tok = self.read_tok_from_trh()?;
				if cfg!(debug_assertions) {
					println!("debug warning: \
						token {:?} discaded without having been peeked",
						tok);
				}
			},
		}
		Ok(())
	}

	fn pop_tok(&mut self) -> Result<(Tok, Loc), ParsingError> {
		if let Some(tok_loc) = self.tok_buffer.pop() {
			Ok(tok_loc)
		} else {
			Ok(self.read_tok_from_trh()?)
		}
	}
}

use crate::{machine::*, tokenizer};

pub enum BlockEnd {
	Void,
	Curly,
}

pub enum ExprEnd {
	Nothing,
	Paren,
}

impl ProgReadingHead {
	pub fn parse_prog(&mut self) -> Result<(Prog, Loc), ParsingError> {
		self.parse_block(BlockEnd::Void)
	}

	pub fn parse_block(&mut self, end: BlockEnd) -> Result<(Block, Loc), ParsingError> {
		let mut stmts: Vec<Stmt> = Vec::new();
		let mut loc = self.peek_tok()?.1.to_owned();
		while !self.peek_tok()?.0.is_void() {
			let (stmt, stmt_loc) = self.parse_stmt()?;
			stmts.push(stmt);
			loc += stmt_loc;
		}
		let block = Block::new(stmts);
		match end {
			BlockEnd::Void => match self.pop_tok()? {
				(Tok::Void, _) => Ok((block, loc)),
				(tok, end_loc) => Err(ParsingError::UnexpectedToken {tok, loc: end_loc}),
			},
			BlockEnd::Curly => match self.pop_tok()? {
				(Tok::Right(s), _) if s == "}" => Ok((block, loc)),
				(tok, end_loc) => Err(ParsingError::UnexpectedToken {tok, loc: end_loc}),
			},
		}
	}

	pub fn parse_stmt(&mut self) -> Result<(Stmt, Loc), ParsingError> {
		let (tok, loc) = self.pop_tok()?;
		match tok {
			Tok::Word(s) if s == "pr" => {
				let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
				Ok((Stmt::Print {expr}, loc + expr_loc))
			},
			Tok::Word(left_word) => match self.peek_tok()? {
				(Tok::ToLeft, _) => {
					self.disc_tok()?;
					let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
					Ok((Stmt::Assign {varname: left_word, expr}, loc + expr_loc))
				},
				(tok, loc) => Err(ParsingError::UnexpectedToken {
					tok: tok.clone(), loc: loc.clone()
				}),
			}
			tok => Err(ParsingError::UnexpectedToken {tok, loc}),
		}
	}

	fn parse_expr_left(&mut self) -> Result<(Expr, Loc), ParsingError> {
		let (tok, loc) = self.pop_tok()?;
		match tok {
			Tok::Word(word) => Ok((Expr::Var {varname: word}, loc)),
			Tok::Integer(integer) => Ok((Expr::Const {val:
				Obj::Integer(str::parse(&integer).expect("integer parsing error"))}, loc)),
			Tok::String(string) => Ok((Expr::Const {val:
				Obj::String(string.clone())}, loc)),
			Tok::Left(left) if left == "(" => self.parse_expr(ExprEnd::Paren),
			tok => Err(ParsingError::UnexpectedToken {tok, loc}),
		}
	}

	pub fn parse_expr(&mut self, end: ExprEnd) -> Result<(Expr, Loc), ParsingError> {
		let (mut expr, mut loc) = self.parse_expr_left()?;
		while self.peek_tok()?.0.is_bin_op() {
			let (op, _) = self.pop_tok()?;
			match op {
				Tok::BinOp(op_string) => {
					let (right_expr, right_loc) = self.parse_expr_left()?;
					expr = Expr::BinOp {
						op: match &op_string[..] {
							"+" => Op::Plus,
							"-" => Op::Minus,
							_ => panic!("operator bad"),
						},
						left: Box::new(expr),
						right: Box::new(right_expr),
					};
					loc += right_loc;
				},
				_ => unreachable!(),
			}
		}
		match end {
			ExprEnd::Nothing => Ok((expr, loc)),
			ExprEnd::Paren => match self.pop_tok()? {
				(Tok::Right(s), _) if s == ")" => Ok((expr, loc)),
				(tok, loc) => Err(ParsingError::UnexpectedToken {loc, tok}),
			}
		}
	}
}
