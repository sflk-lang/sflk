
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
use std::rc::Rc;

pub enum BlockEnd {
	Void,
	Curly,
	Paren,
}

impl Tok {
	fn is_block_end(&self, end: &BlockEnd) -> bool {
		match (end, self) {
			(BlockEnd::Void, Tok::Void) => true,
			(BlockEnd::Curly, Tok::Right(s)) if s == "}" => true,
			(BlockEnd::Paren, Tok::Right(s)) if s == ")" => true,
			_ => false,
		}
	}
}

impl From<Tok> for Op {
	fn from(tok: Tok) -> Op {
		match tok {
			Tok::BinOp(op_string) => match &op_string[..] {
				"+" => Op::Plus,
				"-" => Op::Minus,
				"*" => Op::Star,
				"/" => Op::Slash,
				">" => Op::ToRight,
				_ => panic!("operator bad"),
			},
			_ => panic!("not even operator"),
		}
	}
}

pub enum ExprEnd {
	Nothing,
	Paren,
}

impl ProgReadingHead {
	pub fn parse_prog(&mut self) -> Result<(Prog, Loc), ParsingError> {
		self.parse_block(BlockEnd::Void)
	}

	fn parse_stmts(&mut self, end: BlockEnd) -> Result<(Vec<Stmt>, Loc), ParsingError> {
		let mut stmts: Vec<Stmt> = Vec::new();
		let mut loc = self.peek_tok()?.1.to_owned();
		while !self.peek_tok()?.0.is_block_end(&end) {
			let (stmt, stmt_loc) = self.parse_stmt()?;
			stmts.push(stmt);
			loc += stmt_loc;
		}
		self.disc_tok()?; // Discard the end tok already peeked in the loop condition
		Ok((stmts, loc))
	}

	pub fn parse_block(&mut self, end: BlockEnd) -> Result<(Block, Loc), ParsingError> {
		let (stmts, loc) = self.parse_stmts(end)?;
		let block = Block::new(stmts);
		Ok((block, loc))
	}

	pub fn parse_stmt(&mut self) -> Result<(Stmt, Loc), ParsingError> {
		let (tok, loc) = self.pop_tok()?;
		match tok {
			Tok::Word(s) if s == "nop" => {
				Ok((Stmt::Nop, loc))
			},
			Tok::Word(s) if s == "pr" => {
				let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
				Ok((Stmt::Print {expr}, loc + expr_loc))
			},
			Tok::Word(s) if s == "nl" => {
				Ok((Stmt::PrintNewline, loc))
			},
			Tok::Word(s) if s == "do" => {
				let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
				Ok((Stmt::Do {expr}, loc + expr_loc))
			},
			Tok::Word(s) if s == "ev" => {
				let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
				Ok((Stmt::Ev {expr}, loc + expr_loc))
			}
			Tok::Word(s) if s == "imp" => {
				let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
				Ok((Stmt::Imp {expr}, loc + expr_loc))
			},
			Tok::Word(s) if s == "exp" => {
				let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
				Ok((Stmt::Exp {expr}, loc + expr_loc))
			},
			Tok::Word(s) if s == "redo" => {
				let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
				Ok((Stmt::Redo {expr}, loc + expr_loc))
			},
			Tok::Word(s) if s == "end" => {
				let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
				Ok((Stmt::End {expr}, loc + expr_loc))
			},
			Tok::Word(s) if s == "if" => {
				let (cond_expr, cond_loc) = self.parse_expr(ExprEnd::Nothing)?;
				let (stmt, stmt_loc) = self.parse_stmt()?;
				Ok((Stmt::If {cond_expr, stmt: Box::new(stmt)}, loc + cond_loc + stmt_loc))
			},
			Tok::Word(left_word) => match self.peek_tok()? {
				(Tok::ToLeft, _) => {
					self.disc_tok()?;
					let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
					Ok((Stmt::Assign {varname: left_word, expr}, loc + expr_loc))
				},
				(Tok::ToLeftTilde, _) => {
					self.disc_tok()?;
					let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
					Ok((Stmt::AssignIfFree {varname: left_word, expr}, loc + expr_loc))
				},
				(tok, loc) => Err(ParsingError::UnexpectedToken {
					tok: tok.clone(), loc: loc.clone()
				}),
			},
			Tok::Left(s) if s == "(" => {
				let (stmts, loc) = self.parse_stmts(BlockEnd::Paren)?;
				Ok((Stmt::Group {stmts}, loc))
			},
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
			Tok::Left(left) if left == "{" => {
				let (block, block_loc) = self.parse_block(BlockEnd::Curly)?;
				Ok((Expr::Const {val: Obj::Block(Rc::new(block))}, block_loc))
			},
			tok => Err(ParsingError::UnexpectedToken {tok, loc}),
		}
	}

	fn parse_chop(&mut self) -> Result<(ChOp, Loc), ParsingError> {
		std::assert!(self.peek_tok()?.0.is_bin_op());
		let (op_tok, loc) = self.pop_tok()?;
		let (expr, right_loc) = self.parse_expr_left()?;
		Ok((ChOp {op: Op::from(op_tok), expr}, loc + right_loc))
	}

	pub fn parse_expr(&mut self, end: ExprEnd) -> Result<(Expr, Loc), ParsingError> {
		// TODO:
		// get rid of this dead code in the comment
		// it is unreachable because it is in a comment
		// which is ignored by the compiler
		// poor code
		/*
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
							"*" => Op::Star,
							"/" => Op::Slash,
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
		*/
		let (init_expr, mut loc) = self.parse_expr_left()?;
		let mut chops: Vec<ChOp> = Vec::new();
		while self.peek_tok()?.0.is_bin_op() {
			let (chop, chop_loc) = self.parse_chop()?;
			chops.push(chop);
			loc += chop_loc;
		}
		let expr = if chops.is_empty() {
			init_expr
		} else {Expr::Chain {
			init_expr: Box::new(init_expr),
			chops,
		}};
		match end {
			ExprEnd::Nothing => Ok((expr, loc)),
			ExprEnd::Paren => match self.pop_tok()? {
				(Tok::Right(s), _) if s == ")" => Ok((expr, loc)),
				(tok, loc) => Err(ParsingError::UnexpectedToken {loc, tok}),
			}
		}
	}
}
