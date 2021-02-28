/*
use crate::object::Obj;
use crate::program::{Block, ChOp, Expr, Op, Prog, Stmt};
use crate::scu::{Loc, Located};
use crate::tokenizer::{BinOp, Keyword, Matched, StmtBinOp, Tok, TokReadingHead, TokenizingError};
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum ParsingError {
	TokenizingError(TokenizingError),
	UnexpectedToken {
		tok: Tok,
		loc: Loc,
		for_what: UnexpectedForWhat,
	},
}

impl From<TokenizingError> for ParsingError {
	fn from(tokenizer_error: TokenizingError) -> ParsingError {
		ParsingError::TokenizingError(tokenizer_error)
	}
}

impl Display for ParsingError {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		match self {
			ParsingError::TokenizingError(parsing_error) => write!(f, "{}", parsing_error),
			ParsingError::UnexpectedToken { tok, loc, for_what } => write!(
				f,
				"unexpected token `{}` {} (at line {})",
				tok,
				for_what,
				loc.line()
			),
		}
	}
}

// TODO:
// Find better names
#[derive(Debug)]
pub enum UnexpectedForWhat {
	ToStartAStatement,
	ToFollowAVariableNameAtTheStartOfAStatement,
	ToStartAnExpression,
	ToEndAnExpression(ExprEnd),
}

impl Display for UnexpectedForWhat {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		match self {
			UnexpectedForWhat::ToStartAStatement => write!(f, "to start a statement"),
			UnexpectedForWhat::ToFollowAVariableNameAtTheStartOfAStatement => {
				write!(f, "to follow a variable name at the start of a statement")
			}
			UnexpectedForWhat::ToStartAnExpression => write!(f, "to start an expression"),
			UnexpectedForWhat::ToEndAnExpression(expr_end) => match expr_end {
				ExprEnd::Nothing => unreachable!(),
				ExprEnd::Paren => write!(f, "to end an expression while `)` was expected"),
			},
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

	fn peek_tok(&mut self) -> Result<&(Tok, Loc), ParsingError> {
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
					println!(
						"debug warning: \
						token {:?} discaded without having been peeked",
						tok
					);
				}
			}
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

enum BlockEnd {
	Eof,
	Curly,
}

impl Tok {
	fn is_block_end(&self, end: &BlockEnd) -> bool {
		match (end, self) {
			(BlockEnd::Eof, Tok::Eof) => true,
			(BlockEnd::Curly, Tok::Right(Matched::Curly)) => true,
			_ => false,
		}
	}
}

#[derive(Debug)]
pub enum ExprEnd {
	Nothing,
	Paren,
}

impl ProgReadingHead {
	fn assert_expr_end(&mut self, end: ExprEnd) -> Result<Option<Loc>, ParsingError> {
		match end {
			ExprEnd::Nothing => Ok(None),
			ExprEnd::Paren => match self.pop_tok()? {
				(Tok::Right(Matched::Paren), loc) => Ok(Some(loc)),
				(tok, loc) => Err(ParsingError::UnexpectedToken {
					loc,
					tok,
					for_what: UnexpectedForWhat::ToEndAnExpression(end),
				}),
			},
		}
	}
}

impl From<Tok> for Op {
	fn from(tok: Tok) -> Op {
		match tok {
			Tok::BinOp(binop) => match binop {
				BinOp::Plus => Op::Plus,
				BinOp::Minus => Op::Minus,
				BinOp::Star => Op::Star,
				BinOp::Slash => Op::Slash,
				BinOp::ToRight => Op::ToRight,
			},
			_ => panic!("not even operator"),
		}
	}
}

impl ProgReadingHead {
	pub fn parse_prog(&mut self) -> Result<Located<Prog>, ParsingError> {
		self.parse_block(BlockEnd::Eof)
	}

	fn parse_stmts(&mut self, end: BlockEnd) -> Result<(Vec<Stmt>, Loc), ParsingError> {
		let mut stmts: Vec<Stmt> = Vec::new();
		let mut loc = self.peek_tok()?.1.to_owned();
		while !self.peek_tok()?.0.is_block_end(&end) {
			let Located {
				content: stmt,
				loc: stmt_loc,
			} = self.parse_stmt()?;
			stmts.push(stmt);
			loc += stmt_loc;
		}
		let (_, rightmost_loc) = self.pop_tok()?; // Peeked in the loop condition
		Ok((stmts, loc + rightmost_loc))
	}

	fn parse_block(&mut self, end: BlockEnd) -> Result<Located<Block>, ParsingError> {
		let (stmts, loc) = self.parse_stmts(end)?;
		Ok(Located {
			content: Block::new(stmts),
			loc,
		})
	}

	fn parse_stmt(&mut self) -> Result<Located<Stmt>, ParsingError> {
		let (tok, leftmost_loc) = self.pop_tok()?;
		match tok {
			Tok::Keyword(Keyword::Np) => Ok(Located {
				content: Stmt::Nop,
				loc: leftmost_loc,
			}),
			Tok::Keyword(Keyword::Pr) => Ok(self
				.parse_expr(ExprEnd::Nothing)?
				.map(|expr| Stmt::Print { expr })
				.leftmost_loc(leftmost_loc)),
			Tok::Keyword(Keyword::Nl) => Ok(Located {
				content: Stmt::PrintNewline,
				loc: leftmost_loc,
			}),
			Tok::Keyword(Keyword::Do) => Ok(self
				.parse_expr(ExprEnd::Nothing)?
				.map(|expr| Stmt::Do { expr })
				.leftmost_loc(leftmost_loc)),
			Tok::Keyword(Keyword::Dh) => Ok(self
				.parse_expr(ExprEnd::Nothing)?
				.map(|expr| Stmt::DoHere { expr })
				.leftmost_loc(leftmost_loc)),
			Tok::Keyword(Keyword::Fh) => Ok(self
				.parse_expr(ExprEnd::Nothing)?
				.map(|expr| Stmt::FileDoHere { expr })
				.leftmost_loc(leftmost_loc)),
			Tok::Keyword(Keyword::Ev) => Ok(self
				.parse_expr(ExprEnd::Nothing)?
				.map(|expr| Stmt::Evaluate { expr })
				.leftmost_loc(leftmost_loc)),
			Tok::Keyword(Keyword::Imp) => Ok(self // Will likely disapear or change
				.parse_expr(ExprEnd::Nothing)?
				.map(|expr| Stmt::Imp { expr })
				.leftmost_loc(leftmost_loc)),
			Tok::Keyword(Keyword::Exp) => Ok(self // Will likely disapear or change
				.parse_expr(ExprEnd::Nothing)?
				.map(|expr| Stmt::Exp { expr })
				.leftmost_loc(leftmost_loc)),
			Tok::Keyword(Keyword::Redo) => Ok(self // Will likely disapear or change
				.parse_expr(ExprEnd::Nothing)?
				.map(|expr| Stmt::Redo { expr })
				.leftmost_loc(leftmost_loc)),
			Tok::Keyword(Keyword::End) => Ok(self // Will likely disapear or change
				.parse_expr(ExprEnd::Nothing)?
				.map(|expr| Stmt::End { expr })
				.leftmost_loc(leftmost_loc)),
			Tok::Keyword(Keyword::If) => {
				let Located {
					content: cond_expr,
					loc: cond_loc,
				} = self.parse_expr(ExprEnd::Nothing)?;
				let Located {
					content: if_stmt,
					loc: if_stmt_loc,
				} = self.parse_stmt()?;
				if self.peek_tok()?.0 == Tok::Keyword(Keyword::El) {
					self.disc_tok()?;
					let Located {
						content: el_stmt,
						loc: el_stmt_loc,
					} = self.parse_stmt()?;
					Ok(Located {
						content: Stmt::If {
							cond_expr,
							if_stmt: Box::new(if_stmt),
							el_stmt: Some(Box::new(el_stmt)),
						},
						loc: leftmost_loc + cond_loc + if_stmt_loc + el_stmt_loc,
					})
				} else {
					Ok(Located {
						content: Stmt::If {
							cond_expr,
							if_stmt: Box::new(if_stmt),
							el_stmt: None,
						},
						loc: leftmost_loc + cond_loc + if_stmt_loc,
					})
				}
			}
			Tok::Name(name) => match self.peek_tok()? {
				(Tok::StmtBinOp(StmtBinOp::ToLeft), _) => {
					self.disc_tok()?;
					let Located {
						content: expr,
						loc: expr_loc,
					} = self.parse_expr(ExprEnd::Nothing)?;
					Ok(Located {
						content: Stmt::Assign {
							varname: name,
							expr,
						},
						loc: leftmost_loc + expr_loc,
					})
				}
				(Tok::StmtBinOp(StmtBinOp::ToLeftTilde), _) => {
					self.disc_tok()?;
					let Located {
						content: expr,
						loc: expr_loc,
					} = self.parse_expr(ExprEnd::Nothing)?;
					Ok(Located {
						content: Stmt::AssignIfFree {
							varname: name,
							expr,
						},
						loc: leftmost_loc + expr_loc,
					})
				}
				(tok, loc) => Err(ParsingError::UnexpectedToken {
					tok: tok.clone(),
					loc: loc.clone(),
					for_what: UnexpectedForWhat::ToFollowAVariableNameAtTheStartOfAStatement,
				}),
			},
			tok => Err(ParsingError::UnexpectedToken {
				tok,
				loc: leftmost_loc,
				for_what: UnexpectedForWhat::ToStartAStatement,
			}),
		}
	}

	fn parse_expr_left(&mut self) -> Result<Located<Expr>, ParsingError> {
		let (tok, leftmost_loc) = self.pop_tok()?;
		match tok {
			Tok::Name(name) => Ok(Located {
				content: Expr::Var { varname: name },
				loc: leftmost_loc,
			}),
			Tok::Integer(integer) => Ok(Located {
				content: Expr::Const {
					val: Obj::Integer(str::parse(&integer).expect("integer parsing error")),
				},
				loc: leftmost_loc,
			}),
			Tok::String(string) => Ok(Located {
				content: Expr::Const {
					val: Obj::String(string.clone()),
				},
				loc: leftmost_loc,
			}),
			Tok::Left(Matched::Paren) => {
				Ok(self.parse_expr(ExprEnd::Paren)?.leftmost_loc(leftmost_loc))
			}
			Tok::Left(Matched::Curly) => Ok(self
				.parse_block(BlockEnd::Curly)?
				.map(|block| Expr::Const {
					val: Obj::Block(block),
				})
				.leftmost_loc(leftmost_loc)),
			tok => Err(ParsingError::UnexpectedToken {
				tok,
				loc: leftmost_loc,
				for_what: UnexpectedForWhat::ToStartAnExpression,
			}),
		}
	}

	fn parse_chop(&mut self) -> Result<Located<ChOp>, ParsingError> {
		std::assert!(self.peek_tok()?.0.is_bin_op());
		let (op_tok, op_loc) = self.pop_tok()?;
		let op = Op::from(op_tok);
		Ok(self
			.parse_expr_left()?
			.map(|expr| ChOp { op, expr })
			.leftmost_loc(op_loc))
	}

	fn parse_expr(&mut self, end: ExprEnd) -> Result<Located<Expr>, ParsingError> {
		let Located {
			content: init_expr,
			mut loc,
		} = self.parse_expr_left()?;
		let mut chops: Vec<ChOp> = Vec::new();
		while self.peek_tok()?.0.is_bin_op() {
			let Located {
				content: chop,
				loc: chop_loc,
			} = self.parse_chop()?;
			chops.push(chop);
			loc += chop_loc;
		}
		let expr = if chops.is_empty() {
			init_expr
		} else {
			Expr::Chain {
				init_expr: Box::new(init_expr),
				chops,
			}
		};
		match self.assert_expr_end(end)? {
			Some(rightmost_loc) => loc += rightmost_loc,
			None => (),
		}
		Ok(Located { content: expr, loc })
	}
}
*/
