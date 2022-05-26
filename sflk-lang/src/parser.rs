use crate::ast::{Chop, Comment, Expr, Node, Program, Stmt, TargetExpr};
use crate::scu::{Loc, SourceCodeUnit};
use crate::tokenizer::{BinOp, CharReadingHead, Kw, Matched, StmtBinOp, Tok, Tokenizer};
use std::collections::HashMap;
use std::{collections::VecDeque, rc::Rc};

pub struct ParsingWarning {
	// TODO
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

	fn tokenizer_pop_tok_no_comments(&mut self) -> (Tok, Loc) {
		loop {
			let (tok, loc) = self.tokenizer.pop_tok(&mut self.crh);
			if !matches!(tok, Tok::Comment { .. }) {
				break (tok, loc);
			}
		}
	}

	fn prepare_max_index(&mut self, n: usize) {
		if self.toks_ahead.len() < n + 1 {
			self.toks_ahead.reserve(n - self.toks_ahead.len());
		}
		while self.toks_ahead.len() < n + 1 {
			let (tok, loc) = self.tokenizer_pop_tok_no_comments();
			self.toks_ahead.push_back((tok, loc));
		}
	}

	fn prepare_all(&mut self) {
		loop {
			let (tok, loc) = self.tokenizer_pop_tok_no_comments();
			self.toks_ahead.push_back((tok, loc));
			if matches!(self.toks_ahead.back().map(|t| &t.0), Some(Tok::Eof)) {
				break;
			}
		}
	}

	fn peek(&mut self, n: usize) -> &(Tok, Loc) {
		self.prepare_max_index(n);
		&self.toks_ahead[n]
	}

	fn prepared(&self) -> &VecDeque<(Tok, Loc)> {
		&self.toks_ahead
	}

	fn pop(&mut self) -> (Tok, Loc) {
		self.peek(0);
		let tok_loc_opt = self.toks_ahead.pop_front();
		if let Some(tok_loc) = tok_loc_opt {
			tok_loc
		} else {
			panic!("bug: no token to pop")
		}
	}

	fn disc(&mut self) {
		if self.toks_ahead.pop_front().is_none() {
			panic!("bug: token discarded but not peeked before")
		}
	}
}

impl TokBuffer {
	fn scu(&self) -> Rc<SourceCodeUnit> {
		self.crh.scu()
	}
}

pub struct Parser {}

impl Parser {
	pub fn new() -> Parser {
		Parser {}
	}
}

enum ExtType {
	Stmt,
	Expr,
}

struct StmtExtDescr {
	content_type: ExtType,
	optional: bool,
	/// Can this extention be present multiple times in the same statement?
	can_stack: bool,
}

type StmtExtPackDescr = HashMap<Kw, StmtExtDescr>;

enum SingleContent {
	Stmt(Node<Stmt>),
	Expr(Node<Expr>),
}

type StmtExtPackContent = HashMap<Kw, Vec<SingleContent>>;

impl Parser {
	pub fn parse_program(&mut self, tb: &mut TokBuffer) -> Node<Program> {
		let stmts = self.parse_all_as_stmts(tb);
		Node::from(Program { stmts }, Loc::total_of(tb.scu()))
	}

	fn parse_all_as_stmts(&mut self, tb: &mut TokBuffer) -> Vec<Node<Stmt>> {
		let mut stmts: Vec<Node<Stmt>> = Vec::new();
		loop {
			let comments = self.parse_comments(tb);
			if matches!(tb.peek(0).0, Tok::Eof) {
				// TODO:
				// find a non-dirty way to put the last comments into the
				// internal_comments field of the Node containing this statement vector
				break;
			}
			let mut stmt_node = self.parse_stmt(tb);
			stmt_node.add_left_comments(comments);
			stmts.push(stmt_node);
		}
		stmts
	}

	// TODO:
	// Remove this function
	// and instead add an end parameter to parse_all_as_stmts
	// so that it can stop at either EOF or '}'
	fn parse_stmts(&mut self, tb: &mut TokBuffer) -> Vec<Node<Stmt>> {
		let mut stmts: Vec<Node<Stmt>> = Vec::new();
		while let Some(stmt) = self.maybe_parse_stmt(tb) {
			stmts.push(stmt);
		}
		stmts
	}

	fn parse_stmt(&mut self, tb: &mut TokBuffer) -> Node<Stmt> {
		let left_comments = self.parse_comments(tb);
		let mut stmt_node = if let Some(stmt_node) = self.maybe_parse_stmt(tb) {
			stmt_node
		} else {
			let (_tok, loc) = tb.pop();
			Node::from(Stmt::Invalid, loc)
		};
		stmt_node.add_left_comments(left_comments);
		stmt_node
	}

	fn maybe_parse_stmt(&mut self, tb: &mut TokBuffer) -> Option<Node<Stmt>> {
		let (first_tok, first_loc) = tb.peek(0);
		if let Tok::Kw(kw) = first_tok {
			match kw {
				Kw::Np => {
					let kw_loc = first_loc.clone();
					tb.disc();
					Some(Node::from(Stmt::Nop, kw_loc))
				},
				Kw::Pr => {
					let kw_loc = first_loc.clone();
					tb.disc();
					let expr_node = self.parse_expr(tb);
					let full_loc = &kw_loc + expr_node.loc();
					Some(Node::from(Stmt::Print { expr: expr_node }, full_loc))
				},
				Kw::Nl => {
					let kw_loc = first_loc.clone();
					tb.disc();
					Some(Node::from(Stmt::Newline, kw_loc))
				},
				Kw::Ev => {
					let kw_loc = first_loc.clone();
					tb.disc();
					let expr_node = self.parse_expr(tb);
					let full_loc = &kw_loc + expr_node.loc();
					Some(Node::from(Stmt::Evaluate { expr: expr_node }, full_loc))
				},
				Kw::Do => {
					let kw_loc = first_loc.clone();
					tb.disc();
					let expr_node = self.parse_expr(tb);
					let full_loc = &kw_loc + expr_node.loc();
					Some(Node::from(Stmt::Do { expr: expr_node }, full_loc))
				},
				Kw::Dh => {
					let kw_loc = first_loc.clone();
					tb.disc();
					let expr_node = self.parse_expr(tb);
					let full_loc = &kw_loc + expr_node.loc();
					Some(Node::from(Stmt::DoHere { expr: expr_node }, full_loc))
				},
				Kw::Fh => {
					let kw_loc = first_loc.clone();
					tb.disc();
					let expr_node = self.parse_expr(tb);
					let full_loc = &kw_loc + expr_node.loc();
					Some(Node::from(Stmt::DoFileHere { expr: expr_node }, full_loc))
				},
				Kw::If => {
					let kw_loc = first_loc.clone();
					tb.disc();
					let cond_expr_node = self.parse_expr(tb);
					let mut descr_pack = HashMap::new();
					descr_pack.insert(
						Kw::Th,
						StmtExtDescr {
							content_type: ExtType::Stmt,
							optional: true,
							can_stack: true,
						},
					);
					descr_pack.insert(
						Kw::El,
						StmtExtDescr {
							content_type: ExtType::Stmt,
							optional: true,
							can_stack: true,
						},
					);
					let mut content_pack = self.parse_stmt_extensions(tb, &descr_pack);
					let th_stmts: Vec<_> = content_pack
						.remove(&Kw::Th)
						.unwrap()
						.into_iter()
						.map(|single| match single {
							SingleContent::Stmt(stmt) => stmt,
							_ => panic!("bug"),
						})
						.collect();
					let el_stmts: Vec<_> = content_pack
						.remove(&Kw::El)
						.unwrap()
						.into_iter()
						.map(|single| match single {
							SingleContent::Stmt(stmt) => stmt,
							_ => panic!("bug"),
						})
						.collect();
					let mut full_loc = kw_loc;
					if let Some(stmt_node) = th_stmts.last() {
						full_loc += stmt_node.loc();
					}
					if let Some(stmt_node) = el_stmts.last() {
						full_loc += stmt_node.loc();
					}
					Some(Node::from(
						Stmt::If { cond_expr: cond_expr_node, th_stmts, el_stmts },
						full_loc,
					))
				},
				Kw::Lp => {
					let kw_loc = first_loc.clone();
					tb.disc();
					let sh_expr_node = self.maybe_parse_stmt_extension_expr(tb, Kw::Wh);
					let bd_stmt_node = self.maybe_parse_stmt_extension_stmt(tb, Kw::Bd);
					let sp_stmt_node = self.maybe_parse_stmt_extension_stmt(tb, Kw::Sp);
					let mut full_loc = kw_loc;
					if let Some(expr_node) = &sh_expr_node {
						full_loc += expr_node.loc();
					}
					if let Some(stmt_node) = &bd_stmt_node {
						full_loc += stmt_node.loc();
					}
					if let Some(stmt_node) = &sp_stmt_node {
						full_loc += stmt_node.loc();
					}
					Some(Node::from(
						Stmt::Loop {
							wh_expr: sh_expr_node,
							bd_stmt: bd_stmt_node.map(Box::new),
							sp_stmt: sp_stmt_node.map(Box::new),
						},
						full_loc,
					))
				},
				Kw::Ri => {
					let kw_loc = first_loc.clone();
					tb.disc();
					let expr_node = self.parse_expr(tb);
					let full_loc = &kw_loc + expr_node.loc();
					Some(Node::from(
						Stmt::RegisterInterceptor { expr: expr_node },
						full_loc,
					))
				},
				Kw::Em => {
					let kw_loc = first_loc.clone();
					tb.disc();
					let expr_node = self.parse_expr(tb);
					let (tok, _) = tb.peek(0);
					let target_node = match tok {
						Tok::Kw(tok_kw) if *tok_kw == Kw::Rs => {
							tb.disc();
							let (name_tok, loc) = tb.pop();
							match name_tok {
								Tok::Name { string, .. } => {
									Some(Node::from(TargetExpr::VariableName(string), loc))
								},
								_ => Some(Node::from(TargetExpr::Invalid, loc)),
							}
						},
						_ => None,
					};
					let mut full_loc = &kw_loc + expr_node.loc();
					if let Some(node) = &target_node {
						full_loc += node.loc();
					}
					Some(Node::from(
						Stmt::Emit { expr: expr_node, target: target_node },
						full_loc,
					))
				},
				_ => {
					let kw_loc = first_loc.clone();
					tb.disc();
					Some(Node::from(Stmt::Invalid, kw_loc)) // TODO: do
				},
			}
		} else {
			self.maybe_parse_assign_stmt(tb)
		}
	}

	fn parse_stmt_extensions(
		&mut self,
		tb: &mut TokBuffer,
		descr_pack: &StmtExtPackDescr,
	) -> StmtExtPackContent {
		let mut content_pack = HashMap::new();
		for kw in descr_pack.keys() {
			content_pack.insert(*kw, Vec::new());
		}
		loop {
			let (tok, _) = tb.peek(0);
			match tok {
				Tok::Kw(kw) => {
					if let Some(descr) = descr_pack.get(kw) {
						if !descr.can_stack && !content_pack.get(kw).unwrap().is_empty() {
							panic!("no");
						}
						let kw = *kw;
						tb.disc();
						match descr.content_type {
							ExtType::Stmt => content_pack
								.get_mut(&kw)
								.unwrap()
								.push(SingleContent::Stmt(self.parse_stmt(tb))),
							ExtType::Expr => content_pack
								.get_mut(&kw)
								.unwrap()
								.push(SingleContent::Expr(self.parse_expr(tb))),
						}
					} else {
						break;
					}
				},
				_ => break,
			}
		}
		for (kw, descr) in descr_pack {
			if !descr.optional && content_pack.get(kw).unwrap().is_empty() {
				panic!("no");
			}
		}
		content_pack
	}

	fn maybe_parse_stmt_extension_expr(
		&mut self,
		tb: &mut TokBuffer,
		kw: Kw,
	) -> Option<Node<Expr>> {
		let (tok, _) = tb.peek(0);
		match tok {
			Tok::Kw(tok_kw) if *tok_kw == kw => {
				tb.disc();
				Some(self.parse_expr(tb))
			},
			_ => None,
		}
	}

	fn maybe_parse_stmt_extension_stmt(
		&mut self,
		tb: &mut TokBuffer,
		kw: Kw,
	) -> Option<Node<Stmt>> {
		let (tok, _) = tb.peek(0);
		match tok {
			Tok::Kw(tok_kw) if *tok_kw == kw => {
				tb.disc();
				Some(self.parse_stmt(tb))
			},
			_ => None,
		}
	}

	fn maybe_parse_assign_stmt(&mut self, tb: &mut TokBuffer) -> Option<Node<Stmt>> {
		// TODO:
		// Make this beautiful
		tb.prepare_max_index(1);
		let prepared = tb.prepared();
		match (&prepared[0], &prepared[1]) {
			((Tok::Name { string, .. }, name_loc), (Tok::StmtBinOp(StmtBinOp::ToLeft), _)) => {
				let target_node =
					Node::from(TargetExpr::VariableName(string.clone()), name_loc.clone());
				tb.disc();
				tb.disc();
				let expr_node = self.parse_expr(tb);
				let total_loc = target_node.loc() + expr_node.loc();
				Some(Node::from(
					Stmt::Assign { target: target_node, expr: expr_node },
					total_loc,
				))
			},
			_ => None,
		}
	}

	fn parse_expr(&mut self, tb: &mut TokBuffer) -> Node<Expr> {
		let expr_node = self.parse_expr_beg(tb);
		let mut chops: Vec<Node<Chop>> = Vec::new();
		while let Some(chop_node) = self.maybe_parse_chop(tb) {
			chops.push(chop_node);
		}
		if chops.is_empty() {
			expr_node
		} else {
			let loc = expr_node.loc() + chops.last().unwrap().loc();
			Node::from(Expr::Chain { init: Box::new(expr_node), chops }, loc)
		}
	}

	fn parse_expr_beg(&mut self, tb: &mut TokBuffer) -> Node<Expr> {
		let (tok, left_loc) = tb.pop();
		match tok {
			Tok::Name { string, .. } => Node::from(Expr::VariableName(string), left_loc),
			Tok::Integer(integer) => Node::from(Expr::IntegerLiteral(integer), left_loc),
			Tok::String { content, .. } => Node::from(Expr::StringLiteral(content), left_loc),
			Tok::Left(Matched::Curly) => {
				let stmts = self.parse_stmts(tb);
				let (right_tok, right_loc) = tb.pop();
				match right_tok {
					Tok::Right(Matched::Curly) => {
						Node::from(Expr::BlockLiteral(stmts), left_loc + right_loc)
					},
					_ => panic!("TODO: generate an error here"),
				}
			},
			Tok::Left(Matched::Paren) => {
				if matches!(tb.peek(0).0, Tok::Right(Matched::Paren)) {
					let (_, right_loc) = tb.pop();
					Node::from(Expr::NothingLiteral, left_loc + right_loc)
				} else {
					let expr_node = self.parse_expr(tb);
					let (right_tok, right_loc) = tb.pop();
					match right_tok {
						Tok::Right(Matched::Paren) => {
							Node::from(expr_node.unwrap(), left_loc + right_loc)
						},
						_ => panic!("TODO: generate an error here"),
					}
				}
			},
			_ => Node::from(Expr::Invalid, left_loc), // TODO: do!
		}
	}

	fn maybe_parse_chop(&mut self, tb: &mut TokBuffer) -> Option<Node<Chop>> {
		let (op_tok, op_loc) = tb.peek(0).clone();
		if let Tok::BinOp(op) = op_tok {
			tb.disc();
			let expr_node = self.parse_expr_beg(tb);
			let full_loc = &op_loc + expr_node.loc();
			match op {
				BinOp::Plus => Some(Node::from(Chop::Plus(expr_node), full_loc)),
				BinOp::Minus => Some(Node::from(Chop::Minus(expr_node), full_loc)),
				BinOp::Star => Some(Node::from(Chop::Star(expr_node), full_loc)),
				BinOp::Slash => Some(Node::from(Chop::Slash(expr_node), full_loc)),
				BinOp::Comma => Some(Node::from(Chop::Comma(expr_node), full_loc)),
				BinOp::DoubleComma => Some(Node::from(Chop::DoubleComma(expr_node), full_loc)),
				BinOp::Dot => Some(Node::from(Chop::Dot(expr_node), full_loc)),
				BinOp::ToRight => Some(Node::from(Chop::ToRight(expr_node), full_loc)),
			}
		} else {
			None
		}
	}

	fn parse_comments(&mut self, tb: &mut TokBuffer) -> Vec<Node<Comment>> {
		let mut comments: Vec<Node<Comment>> = Vec::new();
		while let Some(comment) = self.maybe_parse_comment(tb) {
			comments.push(comment);
		}
		comments
	}

	fn maybe_parse_comment(&mut self, tb: &mut TokBuffer) -> Option<Node<Comment>> {
		let (tok, loc) = tb.peek(0);
		if let Tok::Comment {
			content,
			delimitation_thickness,
			no_end_hash_warning,
		} = tok
		{
			let comment_node = Node::from(
				Comment::new(content.to_owned(), delimitation_thickness.to_owned()),
				loc.to_owned(),
			);
			tb.disc();
			Some(comment_node)
		} else {
			None
		}
	}
}
