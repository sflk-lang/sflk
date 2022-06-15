use crate::{
	ast::{Chop as AstChop, Expr, Node, Program, Stmt, TargetExpr},
	log_indent::IndentedLogger,
	scu::Loc,
	tokenizer::{Kw, Matched, Op, SimpleTok, Tok, TokBuffer},
	utils::styles,
};
use std::{
	collections::HashMap,
	convert::{TryFrom, TryInto},
};

enum BlockLevelExpectedTerminator {
	Eof,
	RightCurly,
}

enum StmtExt {
	Stmt(Stmt),
	Expr(Expr),
	Targ(TargetExpr),
	None,
}

// The idea here is to get a parser that can perform one small step at a time
// so that it can be quite verbose in explaining what it does, as well as being
// interactive and all.
// It has a stack of `ParsingAction`, each of which sould represent a simple step
// that is to be performed or advanced in the next step call (possibly pushing
// more actions). For now a `ParsingFrame` does not quite represent a simple step
// and the whole code is messy and not very readable. This has to be improved.
// TODO.

enum ParsingData {
	BlockLevel {
		stmts: Vec<Node<Stmt>>,
		expected_terminator: BlockLevelExpectedTerminator,
	},
	Stmt {
		kw: Node<Kw>,
		content: Option<Node<StmtExt>>,
		exts: HashMap<Kw, Vec<StmtExt>>,
	},
	Expr {
		init: Node<Expr>,
		chops: Vec<Chop>,
	},
	Binop(Node<Binop>),
	StmtInvalid {
		error: Node<Expr>,
	},
}

struct Chop {
	binop: Node<Binop>,
	expr: Node<Expr>,
}

#[derive(Clone, Copy)]
enum Binop {
	Plus,
	Minus,
	Star,
	Slash,
}

/// Each of these should represent one simple action that the parser may perform.
/// A simple action is popped from the action stack, it can modify the data stack as
/// it pleases, it can push actions on the action stack, but it can either consume at
/// most one token or peek at most one token.
/// This restriction on its sight of the token stream will allow for debugging parsing
/// directives to be insertable anywhere in between tokens and it will allow the
/// step-by-step verbose parsing to go token-by-token.
enum ParsingAction {
	Terminate,
	ParseStmtOrStop,
	ParseStmt,         // ( -- stmt)
	AddStmt,           // (block stmt -- block)
	ParseContent,      // (stmt -- stmt content)
	SetContent,        // (stmt content -- stmt)
	ParseExpr,         // ( -- expr)
	ParseNonChainExpr, // ( -- expr)
	ParseChopOrStop,
	ParseChop, // ( -- binop expr)
	AddChop,   // (expr binop expr -- expr)
}

pub struct ParserDebuggingLogger {
	pub logger: Option<IndentedLogger>,
	// Logging parameters should be added here...
}

impl ParserDebuggingLogger {
	fn log_normal(&mut self, string: &str) {
		if let Some(logger) = &mut self.logger {
			logger.log_string(string, styles::NORMAL);
		}
	}

	fn log_normal_indent(&mut self, string: &str) {
		if let Some(logger) = &mut self.logger {
			logger.indent(string, false, styles::NORMAL);
		}
	}

	fn log_error(&mut self, string: &str) {
		if let Some(logger) = &mut self.logger {
			logger.log_string(string, styles::BOLD_LIGHT_RED);
		}
	}

	fn log_error_indent(&mut self, string: &str) {
		if let Some(logger) = &mut self.logger {
			logger.indent(string, false, styles::BOLD_LIGHT_RED);
		}
	}

	fn log_deindent(&mut self, string: &str) {
		if let Some(logger) = &mut self.logger {
			logger.deindent(string);
		}
	}

	fn log_peek_token(&mut self, tok: &Tok) {
		if let Some(logger) = &mut self.logger {
			logger.log_string(&format!("Peek {}", tok), styles::NORMAL);
		}
	}

	fn log_consume_token(&mut self, tok: &Tok) {
		if let Some(logger) = &mut self.logger {
			logger.log_string(&format!("Consume {}", tok), styles::NORMAL);
		}
	}
}

pub struct Parser {
	tb: TokBuffer,
	data_stack: Vec<ParsingData>,
	action_stack: Vec<ParsingAction>,
	lang: LanguageDescr,
	debug: ParserDebuggingLogger,
}

impl Parser {
	pub fn new(tb: TokBuffer, debug: ParserDebuggingLogger) -> Parser {
		Parser {
			tb,
			data_stack: Vec::new(),
			action_stack: Vec::new(),
			lang: LanguageDescr::new(),
			debug,
		}
	}

	pub fn parse_program(&mut self) -> Node<Program> {
		self.data_stack.push(ParsingData::BlockLevel {
			stmts: Vec::new(),
			expected_terminator: BlockLevelExpectedTerminator::Eof,
		});
		self.action_stack.push(ParsingAction::ParseStmtOrStop);
		self.parse();
		match self.data_stack.pop() {
			Some(ParsingData::BlockLevel { stmts, .. }) => {
				Node::from(Program { stmts }, Loc::total_of(self.tb.scu()))
			},
			_ => panic!(),
		}
	}

	fn parse(&mut self) {
		loop {
			self.perform_one_action();
			let terminated = matches!(self.action_stack.last(), Some(&ParsingAction::Terminate));
			if terminated {
				self.debug.log_normal("Done parsing");
				break;
			}
		}
	}

	fn perform_one_action(&mut self) {
		match self.action_stack.pop().unwrap() {
			ParsingAction::ParseStmtOrStop => {
				let (tok, loc) = self.tb.peek(0);
				self.debug.log_peek_token(tok);
				if let ParsingData::BlockLevel { expected_terminator, .. } =
					self.data_stack.last().unwrap()
				{
					match tok {
						Tok::Eof => {
							if matches!(expected_terminator, BlockLevelExpectedTerminator::Eof) {
								self.action_stack.push(ParsingAction::Terminate);
							} else {
								panic!("Unexpected end-of-file at line {}", loc.line());
							}
						},
						Tok::Right(Matched::Curly) => {
							if matches!(
								expected_terminator,
								BlockLevelExpectedTerminator::RightCurly
							) {
							} else {
								//panic!("Unexpected right curly at line {}", loc.line());
								self.debug
									.log_normal("TODO: error for unmached right curly");
								self.action_stack.push(ParsingAction::ParseStmtOrStop);
								self.action_stack.push(ParsingAction::AddStmt);
								self.action_stack.push(ParsingAction::ParseStmt);
							}
						},
						_ => {
							self.action_stack.push(ParsingAction::ParseStmtOrStop);
							self.action_stack.push(ParsingAction::AddStmt);
							self.action_stack.push(ParsingAction::ParseStmt);
						},
					}
				} else {
					panic!()
				}
			},
			ParsingAction::ParseStmt => {
				let (tok, loc) = self.tb.pop();
				self.debug.log_consume_token(&tok);
				if let Tok::Kw(kw) = tok {
					let stmt_descr = self.lang.stmts.get(&kw);
					if let Some(stmt_descr) = stmt_descr {
						self.debug.log_normal_indent(&format!(
							"Parsing {} {} statement",
							&stmt_descr.name_article, &stmt_descr.name
						));
						self.data_stack.push(ParsingData::Stmt {
							kw: Node::from(kw, loc),
							content: None,
							exts: HashMap::new(),
						});
						match stmt_descr.content_type {
							ExtType::None => (),
							ExtType::Expr => {
								self.action_stack.push(ParsingAction::SetContent);
								self.action_stack.push(ParsingAction::ParseContent);
							},
							_ => unimplemented!(),
						}
					} else {
						let error_string = format!("Unexpected keyword {}", kw);
						let error_line_string = format!("{} on line {}", error_string, loc.line());
						self.debug.log_error_indent(&error_string);
						self.data_stack.push(ParsingData::StmtInvalid {
							error: Node::from(Expr::StringLiteral(error_line_string), loc),
						});
					}
				} else {
					let error_string = format!("Unexpected token {}", tok);
					let error_line_string = format!("{} on line {}", error_string, loc.line());
					self.debug.log_error_indent(&error_string);
					self.data_stack.push(ParsingData::StmtInvalid {
						error: Node::from(Expr::StringLiteral(error_line_string), loc),
					});
				}
			},
			ParsingAction::AddStmt => match self.data_stack.pop().unwrap() {
				ParsingData::Stmt { kw, content, exts } => {
					match self.data_stack.last_mut().unwrap() {
						ParsingData::BlockLevel { stmts, .. } => {
							stmts.push(temporary_into_ast_stmt(kw, content, exts));
						},
						_ => panic!(),
					}
					self.debug.log_deindent("Done parsing statement");
				},
				ParsingData::StmtInvalid { error } => {
					let loc = error.loc().clone();
					match self.data_stack.last_mut().unwrap() {
						ParsingData::BlockLevel { stmts, .. } => {
							stmts.push(Node::from(Stmt::Invalid { error_expr: error }, loc));
						},
						_ => panic!(),
					}
					self.debug.log_deindent("Done parsing statement");
				},
				_ => {
					unimplemented!();
				},
			},
			ParsingAction::ParseContent => {
				if let ParsingData::Stmt { kw, content, .. } = self.data_stack.last_mut().unwrap() {
					let stmt_descr = self.lang.stmts.get(kw.unwrap_ref()).unwrap();
					match stmt_descr.content_type {
						ExtType::None => panic!(),
						ExtType::Expr => {
							self.debug
								.log_normal_indent("Parsing expression that is the main content");
							self.action_stack.push(ParsingAction::ParseExpr);
						},
						_ => unimplemented!(),
					}
				} else {
					panic!()
				}
			},
			ParsingAction::SetContent => {
				let content_data = self.data_stack.pop().unwrap();
				if let ParsingData::Stmt { kw, content, .. } = self.data_stack.last_mut().unwrap() {
					let stmt_descr = self.lang.stmts.get(kw.unwrap_ref()).unwrap();
					match (&stmt_descr.content_type, content_data) {
						(ExtType::None, _) => panic!(),
						(ExtType::Expr, ParsingData::Expr { init, chops }) => {
							self.debug.log_deindent("Done parsing expression");
							self.debug.log_deindent("Done parsing main content");
							content.insert(temporary_into_ast_expr(init, chops).map(StmtExt::Expr));
						},
						_ => unimplemented!(),
					}
				} else {
					panic!()
				}
			},
			ParsingAction::ParseNonChainExpr => {
				let (tok, loc) = self.tb.pop();
				self.debug.log_consume_token(&tok);
				match tok {
					Tok::Integer(integer_string) => self.data_stack.push(ParsingData::Expr {
						init: Node::from(Expr::IntegerLiteral(integer_string), loc),
						chops: Vec::new(),
					}),
					Tok::String { content, .. } => self.data_stack.push(ParsingData::Expr {
						init: Node::from(Expr::StringLiteral(content), loc),
						chops: Vec::new(),
					}),
					_ => {
						let error_string = format!("Unexpected token {}", tok);
						self.debug.log_error(&error_string);
						let error_line_string = format!("{} on line {}", error_string, loc.line());
						self.data_stack.push(ParsingData::Expr {
							init: Node::from(
								Expr::Invalid {
									error_expr: Box::new(Node::from(
										Expr::StringLiteral(error_line_string),
										loc.clone(),
									)),
								},
								loc,
							),
							chops: Vec::new(),
						})
					},
				}
			},
			ParsingAction::ParseExpr => {
				self.debug.log_normal_indent("Parsing expression");
				self.action_stack.push(ParsingAction::ParseChopOrStop);
				self.action_stack.push(ParsingAction::ParseNonChainExpr);
			},
			ParsingAction::ParseChopOrStop => {
				let (tok, loc) = self.tb.peek(0);
				self.debug.log_peek_token(tok);
				let simple_tok = SimpleTok::try_from(tok).ok();
				if simple_tok.is_some() && self.lang.binops.contains_key(&simple_tok.unwrap()) {
					self.action_stack.push(ParsingAction::ParseChopOrStop);
					self.action_stack.push(ParsingAction::AddChop);
					self.action_stack.push(ParsingAction::ParseChop);
				}
			},
			ParsingAction::ParseChop => {
				self.debug.log_normal_indent("Parsing chain operation");
				let (tok, loc) = self.tb.pop();
				self.debug.log_consume_token(&tok);
				let simple_tok = SimpleTok::try_from(&tok).unwrap();
				assert!(self.lang.binops.contains_key(&simple_tok));
				self.data_stack.push(ParsingData::Binop(Node::from(
					*self.lang.binops.get(&simple_tok).unwrap(),
					loc,
				)));
				self.action_stack.push(ParsingAction::ParseNonChainExpr);
			},
			ParsingAction::AddChop => {
				let expr = match self.data_stack.pop().unwrap() {
					ParsingData::Expr { init, chops } => temporary_into_ast_expr(init, chops),
					_ => panic!(),
				};
				let binop = match self.data_stack.pop().unwrap() {
					ParsingData::Binop(binop) => binop,
					_ => panic!(),
				};
				if let ParsingData::Expr { init, chops } = self.data_stack.last_mut().unwrap() {
					chops.push(Chop { binop, expr })
				}
				self.debug.log_deindent("Done parsing chain operation");
			},
			_ => {
				unimplemented!();
			},
		}
	}
}

enum ExtType {
	Stmt,
	Expr,
	Targ,
	None,
}

struct StmtExtDescr {
	content_type: ExtType,
	optional: bool,
	/// Can this extention be present multiple times in the same statement?
	can_stack: bool,
}

struct StmtDescr {
	name_article: String,
	name: String,
	content_type: ExtType,
	extentions: HashMap<Kw, StmtExtDescr>,
}

struct LanguageDescr {
	stmts: HashMap<Kw, StmtDescr>,
	binops: HashMap<SimpleTok, Binop>,
}

impl LanguageDescr {
	fn new() -> LanguageDescr {
		let mut stmts = HashMap::new();
		stmts.insert(
			Kw::Np,
			StmtDescr {
				name_article: "a".to_string(),
				name: "nop".to_string(),
				content_type: ExtType::None,
				extentions: HashMap::new(),
			},
		);
		stmts.insert(
			Kw::Nl,
			StmtDescr {
				name_article: "a".to_string(),
				name: "newline".to_string(),
				content_type: ExtType::None,
				extentions: HashMap::new(),
			},
		);
		stmts.insert(
			Kw::Pr,
			StmtDescr {
				name_article: "a".to_string(),
				name: "print".to_string(),
				content_type: ExtType::Expr,
				extentions: HashMap::new(),
			},
		);
		let mut binops = HashMap::new();
		binops.insert(SimpleTok::Op(Op::Plus), Binop::Plus);
		binops.insert(SimpleTok::Op(Op::Minus), Binop::Minus);
		binops.insert(SimpleTok::Op(Op::Star), Binop::Star);
		binops.insert(SimpleTok::Op(Op::Slash), Binop::Slash);
		LanguageDescr { stmts, binops }
	}
}

// TODO: Stop having to use `ast::Stmt` this early, or maybe at all.
fn temporary_into_ast_stmt(
	kw: Node<Kw>,
	content: Option<Node<StmtExt>>,
	exts: HashMap<Kw, Vec<StmtExt>>,
) -> Node<Stmt> {
	let kw_loc = kw.loc().to_owned();
	match kw.unwrap() {
		Kw::Np => Node::from(Stmt::Nop, kw_loc),
		Kw::Nl => Node::from(Stmt::Newline, kw_loc),
		Kw::Pr => Node::from(
			Stmt::Print {
				expr: {
					let node = content.unwrap();
					node.map(|ext| match ext {
						StmtExt::Expr(expr) => expr,
						_ => panic!(),
					})
				},
			},
			kw_loc,
		),
		_ => unimplemented!(),
	}
}

// TODO: Stop having to use `ast::Expr` this early, or maybe at all.
fn temporary_into_ast_expr(init: Node<Expr>, chops: Vec<Chop>) -> Node<Expr> {
	let init_loc = init.loc().to_owned();
	if chops.is_empty() {
		init
	} else {
		let chops: Vec<_> = chops
			.into_iter()
			.map(|chop| {
				let Chop { binop, expr } = chop;
				{
					let loc = expr.loc().clone();
					match binop.unwrap() {
						Binop::Plus => Node::from(AstChop::Plus(expr), loc),
						Binop::Minus => Node::from(AstChop::Minus(expr), loc),
						Binop::Star => Node::from(AstChop::Star(expr), loc),
						Binop::Slash => Node::from(AstChop::Slash(expr), loc),
					}
				}
			})
			.collect();
		let loc = chops.last().map_or(init_loc, |node| node.loc().clone());
		Node::from(Expr::Chain { init: Box::new(init), chops }, loc)
	}
}
