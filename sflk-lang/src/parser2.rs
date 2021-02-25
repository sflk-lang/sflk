// TODO:
// Remove the * from all uses
use crate::ast::*;
use crate::parser::*;
//use crate::program::*;
use crate::scu::*;
use crate::tokenizer::*;
use std::{collections::VecDeque, rc::Rc};

// TODO:
//
// 1.
// Make a struct exactly like
// pub struct ProgReadingHead {
// 	trh: TokReadingHead,
// 	tok_buffer: Vec<(Tok, Loc)>,
// }
// but named TokStream
//
// 2.
// Make the Parser store the generated warnings
// (Note that errors are still returned by the Result returned objects)

pub struct ParsingWarning {
	// TODO
}

pub struct TokForwardRh {
	trh: TokReadingHead,
	toks_ahead: VecDeque<(Tok, Loc)>,
}

impl TokForwardRh {
	pub fn from(trh: TokReadingHead) -> TokForwardRh {
		TokForwardRh {
			trh,
			toks_ahead: VecDeque::new(),
		}
	}

	fn peek_prepare_max_index(&mut self, n: usize) -> Result<(), ParsingError> {
		if self.toks_ahead.len() < n + 1 {
			self.toks_ahead.reserve(n - self.toks_ahead.len());
		}
		while self.toks_ahead.len() < n + 1 {
			self.toks_ahead.push_back(self.trh.read_cur_tok()?);
		}
		Ok(())
	}

	fn peek(&mut self, n: usize) -> Result<&(Tok, Loc), ParsingError> {
		self.peek_prepare_max_index(n)?;
		Ok(&self.toks_ahead[n])
	}

	fn peek_prepared(&self) -> &VecDeque<(Tok, Loc)> {
		&self.toks_ahead
	}

	fn pop(&mut self) -> Result<(Tok, Loc), ParsingError> {
		self.peek(0)?;
		let tok_loc_opt = self.toks_ahead.pop_front();
		if let Some(tok_loc) = tok_loc_opt {
			Ok(tok_loc)
		} else {
			panic!("bug: no token to pop")
		}
	}

	fn discard_peeked(&mut self) {
		if self.toks_ahead.pop_front().is_none() {
			panic!("bug: token discarded but not peeked before")
		}
	}
}

impl TokForwardRh {
	fn scu(&self) -> Rc<SourceCodeUnit> {
		self.trh.scu()
	}
}

pub struct Parser {}

impl Parser {
	pub fn new() -> Parser {
		Parser {}
	}
}

impl Parser {
	pub fn parse_program(&mut self, tfr: &mut TokForwardRh) -> Result<Node<Program>, ParsingError> {
		let stmts = self.parse_stmts(tfr)?;
		Ok(Node::from(Program { stmts }, Loc::total_of(tfr.scu())))
	}

	fn parse_stmts(&mut self, tfr: &mut TokForwardRh) -> Result<Vec<Node<Stmt>>, ParsingError> {
		let mut stmts: Vec<Node<Stmt>> = Vec::new();
		while let Some(stmt) = self.maybe_parse_stmt(tfr)? {
			stmts.push(stmt);
		}
		Ok(stmts)
	}

	fn parse_stmt(&mut self, tfr: &mut TokForwardRh) -> Result<Node<Stmt>, ParsingError> {
		Ok(self.maybe_parse_stmt(tfr)?.unwrap())
	}

	fn maybe_parse_stmt(
		&mut self,
		tfr: &mut TokForwardRh,
	) -> Result<Option<Node<Stmt>>, ParsingError> {
		let (first_tok, first_loc) = tfr.peek(0)?;
		if let Tok::Keyword(kw) = first_tok {
			match kw {
				Keyword::Np => {
					let kw_loc = first_loc.clone();
					tfr.discard_peeked();
					Ok(Some(Node::from(Stmt::Nop, kw_loc)))
				}
				Keyword::Pr => {
					let kw_loc = first_loc.clone();
					tfr.discard_peeked();
					let expr_node = self.parse_expr(tfr)?;
					let full_loc = &kw_loc + expr_node.loc();
					Ok(Some(Node::from(Stmt::Print { expr: expr_node }, full_loc)))
				}
				Keyword::Nl => {
					let kw_loc = first_loc.clone();
					tfr.discard_peeked();
					Ok(Some(Node::from(Stmt::Newline, kw_loc)))
				}
				Keyword::Ev => {
					let kw_loc = first_loc.clone();
					tfr.discard_peeked();
					let expr_node = self.parse_expr(tfr)?;
					let full_loc = &kw_loc + expr_node.loc();
					Ok(Some(Node::from(
						Stmt::Evaluate { expr: expr_node },
						full_loc,
					)))
				}
				Keyword::Do => {
					let kw_loc = first_loc.clone();
					tfr.discard_peeked();
					let expr_node = self.parse_expr(tfr)?;
					let full_loc = &kw_loc + expr_node.loc();
					Ok(Some(Node::from(Stmt::Do { expr: expr_node }, full_loc)))
				}
				Keyword::Dh => {
					let kw_loc = first_loc.clone();
					tfr.discard_peeked();
					let expr_node = self.parse_expr(tfr)?;
					let full_loc = &kw_loc + expr_node.loc();
					Ok(Some(Node::from(Stmt::DoHere { expr: expr_node }, full_loc)))
				}
				Keyword::Fh => {
					let kw_loc = first_loc.clone();
					tfr.discard_peeked();
					let expr_node = self.parse_expr(tfr)?;
					let full_loc = &kw_loc + expr_node.loc();
					Ok(Some(Node::from(
						Stmt::DoFileHere { expr: expr_node },
						full_loc,
					)))
				}
				Keyword::If => {
					let kw_loc = first_loc.clone();
					tfr.discard_peeked();
					let cond_expr_node = self.parse_expr(tfr)?;
					let th_stmt_node = self.maybe_parse_stmt_extension_stmt(tfr, Keyword::Th)?;
					let el_stmt_node = self.maybe_parse_stmt_extension_stmt(tfr, Keyword::El)?;
					let mut full_loc = kw_loc;
					if let Some(stmt_node) = &th_stmt_node {
						full_loc += stmt_node.loc();
					}
					if let Some(stmt_node) = &el_stmt_node {
						full_loc += stmt_node.loc();
					}
					Ok(Some(Node::from(
						Stmt::If {
							cond_expr: cond_expr_node,
							th_stmt: th_stmt_node.map(Box::new),
							el_stmt: el_stmt_node.map(Box::new),
						},
						full_loc,
					)))
				}
				_ => todo!(),
			}
		} else if let Some(stmt) = self.maybe_parse_assign_stmt(tfr)? {
			Ok(Some(stmt))
		} else {
			Ok(None)
		}
	}

	fn maybe_parse_stmt_extension_stmt(
		&mut self,
		tfr: &mut TokForwardRh,
		kw: Keyword,
	) -> Result<Option<Node<Stmt>>, ParsingError> {
		let (tok, _) = tfr.peek(0)?;
		if *tok == Tok::Keyword(kw) {
			tfr.discard_peeked();
			Ok(Some(self.parse_stmt(tfr)?))
		} else {
			Ok(None)
		}
	}

	fn maybe_parse_assign_stmt(
		&mut self,
		tfr: &mut TokForwardRh,
	) -> Result<Option<Node<Stmt>>, ParsingError> {
		// TODO:
		// Make this beautiful
		tfr.peek_prepare_max_index(1)?;
		match (&tfr.peek_prepared()[0], &tfr.peek_prepared()[1]) {
			((Tok::Name(name), name_loc), (Tok::StmtBinOp(StmtBinOp::ToLeft), _)) => {
				let target_node =
					Node::from(TargetExpr::VariableName(name.clone()), name_loc.clone());
				tfr.discard_peeked();
				tfr.discard_peeked();
				let expr_node = self.parse_expr(tfr)?;
				let total_loc = target_node.loc() + expr_node.loc();
				Ok(Some(Node::from(
					Stmt::Assign {
						target: target_node,
						expr: expr_node,
					},
					total_loc,
				)))
			}
			_ => Ok(None),
		}
	}

	fn parse_expr(&mut self, tfr: &mut TokForwardRh) -> Result<Node<Expr>, ParsingError> {
		let expr_node = self.parse_expr_beg(tfr)?;
		let mut chops: Vec<Node<Chop>> = Vec::new();
		while let Some(chop_node) = self.maybe_parse_chop(tfr)? {
			chops.push(chop_node);
		}
		if chops.is_empty() {
			Ok(expr_node)
		} else {
			let loc = expr_node.loc() + chops.last().unwrap().loc();
			Ok(Node::from(
				Expr::Chain {
					init: Box::new(expr_node),
					chops,
				},
				loc,
			))
		}
	}

	fn parse_expr_beg(&mut self, tfr: &mut TokForwardRh) -> Result<Node<Expr>, ParsingError> {
		let (tok, left_loc) = tfr.pop()?;
		match tok {
			Tok::Name(name) => Ok(Node::from(Expr::VariableName(name), left_loc)),
			Tok::Integer(integer) => Ok(Node::from(Expr::IntegerLiteral(integer), left_loc)),
			Tok::String(string) => Ok(Node::from(Expr::StringLiteral(string), left_loc)),
			Tok::Left(Matched::Curly) => {
				let stmts = self.parse_stmts(tfr)?;
				let (right_tok, right_loc) = tfr.pop()?;
				match right_tok {
					Tok::Right(Matched::Curly) => {
						Ok(Node::from(Expr::BlockLiteral(stmts), left_loc + right_loc))
					}
					_ => panic!("TODO: generate an error here"),
				}
			}
			Tok::Left(Matched::Paren) => {
				let expr_node = self.parse_expr(tfr)?;
				let (right_tok, right_loc) = tfr.pop()?;
				match right_tok {
					Tok::Right(Matched::Paren) => {
						Ok(Node::from(expr_node.unwrap(), left_loc + right_loc))
					}
					_ => panic!("TODO: generate an error here"),
				}
			}
			_ => todo!(),
		}
	}

	fn maybe_parse_chop(
		&mut self,
		tfr: &mut TokForwardRh,
	) -> Result<Option<Node<Chop>>, ParsingError> {
		let (op_tok, op_loc) = tfr.peek(0)?.clone();
		if let Tok::BinOp(op) = op_tok {
			tfr.discard_peeked();
			let expr_node = self.parse_expr_beg(tfr)?;
			let full_loc = &op_loc + expr_node.loc();
			match op {
				BinOp::Plus => Ok(Some(Node::from(Chop::Plus(expr_node), full_loc))),
				BinOp::Minus => Ok(Some(Node::from(Chop::Minus(expr_node), full_loc))),
				BinOp::Star => Ok(Some(Node::from(Chop::Star(expr_node), full_loc))),
				BinOp::Slash => Ok(Some(Node::from(Chop::Slash(expr_node), full_loc))),
				BinOp::ToRight => Ok(Some(Node::from(Chop::ToRight(expr_node), full_loc))),
			}
		} else {
			Ok(None)
		}
	}
}
