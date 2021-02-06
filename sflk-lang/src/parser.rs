
use std::fmt::{Display, Formatter};
use crate::tokenizer::{
	TokReadingHead,
	Tok, Keyword, BinOp, Matched, StmtBinOp,
	Loc, TokenizingError
};
use crate::program::{Prog, Block, Stmt, Expr, ChOp, Op};
use crate::object::Obj;


#[derive(Debug)]
pub enum ParsingError {
	TokenizingError(TokenizingError),
	UnexpectedToken {tok: Tok, loc: Loc, for_what: UnexpectedForWhat},
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
			ParsingError::UnexpectedToken {tok, loc, for_what} =>
				write!(f, "unexpected token `{}` {} (at line {})",
					tok, for_what, loc.line()),
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
			UnexpectedForWhat::ToStartAStatement =>
				write!(f, "to start a statement"),
			UnexpectedForWhat::ToFollowAVariableNameAtTheStartOfAStatement =>
				write!(f, "to follow a variable name at the start of a statement"),
			UnexpectedForWhat::ToStartAnExpression =>
				write!(f, "to start an expression"),
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


enum BlockEnd {
	Void,
	Curly,
}

impl Tok {
	fn is_block_end(&self, end: &BlockEnd) -> bool {
		match (end, self) {
			(BlockEnd::Void, Tok::Void) => true,
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
	fn assert_expr_end(&mut self, end: ExprEnd) -> Result<(), ParsingError> {
		match end {
			ExprEnd::Nothing => Ok(()),
			ExprEnd::Paren => match self.pop_tok()? {
				(Tok::Right(Matched::Paren), _) => Ok(()),
				(tok, loc) => Err(ParsingError::UnexpectedToken {loc, tok,
					for_what: UnexpectedForWhat::ToEndAnExpression(end)}),
			}
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

	fn parse_block(&mut self, end: BlockEnd) -> Result<(Block, Loc), ParsingError> {
		let (stmts, loc) = self.parse_stmts(end)?;
		let block = Block::new(stmts);
		Ok((block, loc))
	}

	fn parse_stmt(&mut self) -> Result<(Stmt, Loc), ParsingError> {
		let (tok, loc) = self.pop_tok()?;
		match tok {
			Tok::Keyword(Keyword::Np) => {
				Ok((Stmt::Np, loc))
			},
			Tok::Keyword(Keyword::Pr) => {
				let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
				Ok((Stmt::Print {expr}, loc + expr_loc))
			},
			Tok::Keyword(Keyword::Nl) => {
				Ok((Stmt::PrintNewline, loc))
			},
			Tok::Keyword(Keyword::Do) => {
				let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
				Ok((Stmt::Do {expr}, loc + expr_loc))
			},
			Tok::Keyword(Keyword::Dh) => {
				let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
				Ok((Stmt::DoHere {expr}, loc + expr_loc))
			},
			Tok::Keyword(Keyword::Fh) => {
				let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
				Ok((Stmt::FileDoHere {expr}, loc + expr_loc))
			},
			Tok::Keyword(Keyword::Ev) => {
				let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
				Ok((Stmt::Ev {expr}, loc + expr_loc))
			}
			Tok::Keyword(Keyword::Imp) => { // will likely disapear or change
				let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
				Ok((Stmt::Imp {expr}, loc + expr_loc))
			},
			Tok::Keyword(Keyword::Exp) => { // will likely disapear or change
				let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
				Ok((Stmt::Exp {expr}, loc + expr_loc))
			},
			Tok::Keyword(Keyword::Redo) => { // will likely disapear or change
				let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
				Ok((Stmt::Redo {expr}, loc + expr_loc))
			},
			Tok::Keyword(Keyword::End) => { // will likely disapear or change
				let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
				Ok((Stmt::End {expr}, loc + expr_loc))
			},
			Tok::Keyword(Keyword::If) => {
				let (cond_expr, cond_loc) = self.parse_expr(ExprEnd::Nothing)?;
				let (if_stmt, if_stmt_loc) = self.parse_stmt()?;
				if self.peek_tok()?.0 == Tok::Keyword(Keyword::El) {
					self.disc_tok()?;
					let (el_stmt, el_stmt_loc) = self.parse_stmt()?;
					Ok((
						Stmt::If {
							cond_expr,
							if_stmt: Box::new(if_stmt),
							el_stmt: Some(Box::new(el_stmt))
						},
						loc + cond_loc + if_stmt_loc + el_stmt_loc))
				} else {
					Ok((
						Stmt::If {
							cond_expr,
							if_stmt: Box::new(if_stmt),
							el_stmt: None
						},
						loc + cond_loc + if_stmt_loc))
				}
			},
			Tok::Word(word) => match self.peek_tok()? {
				(Tok::StmtBinOp(StmtBinOp::ToLeft), _) => {
					self.disc_tok()?;
					let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
					Ok((Stmt::Assign {varname: word, expr}, loc + expr_loc))
				},
				(Tok::StmtBinOp(StmtBinOp::ToLeftTilde), _) => {
					self.disc_tok()?;
					let (expr, expr_loc) = self.parse_expr(ExprEnd::Nothing)?;
					Ok((Stmt::AssignIfFree {varname: word, expr}, loc + expr_loc))
				},
				(tok, loc) => Err(ParsingError::UnexpectedToken {
					tok: tok.clone(), loc: loc.clone(),
					for_what: UnexpectedForWhat::ToFollowAVariableNameAtTheStartOfAStatement
				}),
			},
			tok => Err(ParsingError::UnexpectedToken {tok, loc,
				for_what: UnexpectedForWhat::ToStartAStatement}),
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
			Tok::Left(Matched::Paren) => self.parse_expr(ExprEnd::Paren),
			Tok::Left(Matched::Curly) => {
				let (block, block_loc) = self.parse_block(BlockEnd::Curly)?;
				Ok((Expr::Const {val: Obj::Block(block)}, block_loc))
			},
			tok => Err(ParsingError::UnexpectedToken {tok, loc,
				for_what: UnexpectedForWhat::ToStartAnExpression}),
		}
	}

	fn parse_chop(&mut self) -> Result<(ChOp, Loc), ParsingError> {
		std::assert!(self.peek_tok()?.0.is_bin_op());
		let (op_tok, loc) = self.pop_tok()?;
		let (expr, right_loc) = self.parse_expr_left()?;
		Ok((ChOp {op: Op::from(op_tok), expr}, loc + right_loc))
	}

	fn parse_expr(&mut self, end: ExprEnd) -> Result<(Expr, Loc), ParsingError> {
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
		self.assert_expr_end(end)?;
		Ok((expr, loc))
	}
}
