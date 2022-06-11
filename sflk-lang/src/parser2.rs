use crate::{
	ast::{Expr, Node, Program, Stmt, TargetExpr},
	log_indent::IndentedLogger,
	scu::Loc,
	tokenizer::{Kw, Matched, Tok, TokBuffer},
	utils::styles,
};
use std::collections::HashMap;

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
	Expr(Node<Expr>),
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
	ParseStmtOrStopOnTerminator,
	ParseStmt,    // ( -- stmt)
	AddStmt,      // (stmt -- )
	ParseContent, // (stmt -- stmt content)
	SetContent,   // (stmt content -- stmt)
	ParseExpr,    // ( -- expr)
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
		self.action_stack
			.push(ParsingAction::ParseStmtOrStopOnTerminator);
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
			ParsingAction::ParseStmtOrStopOnTerminator => {
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
								self.action_stack
									.push(ParsingAction::ParseStmtOrStopOnTerminator);
								self.action_stack.push(ParsingAction::ParseStmt);
							}
						},
						_ => {
							self.action_stack
								.push(ParsingAction::ParseStmtOrStopOnTerminator);
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
						self.action_stack.push(ParsingAction::AddStmt);
						self.data_stack.push(ParsingData::Stmt {
							kw: Node::from(kw, loc),
							content: None,
							exts: HashMap::new(),
						});
						self.debug.log_normal_indent(&format!(
							"Parsing {} {} statement",
							&stmt_descr.name_article, &stmt_descr.name
						));
						match stmt_descr.content_type {
							ExtType::None => (),
							ExtType::Expr => {
								self.action_stack.push(ParsingAction::SetContent);
								self.action_stack.push(ParsingAction::ParseContent);
							},
							_ => unimplemented!(),
						}
					} else {
						self.debug
							.log_normal(&format!("TODO: handle keyword {}", kw));
					}
				} else {
					self.debug
						.log_normal(&format!("TODO: handle token {}", tok));
				}
			},
			ParsingAction::AddStmt => {
				if let ParsingData::Stmt { kw, content, exts } = self.data_stack.pop().unwrap() {
					self.debug.log_deindent("Done parsing statement");
					match self.data_stack.last_mut().unwrap() {
						ParsingData::BlockLevel { stmts, .. } => {
							stmts.push(temporary_into_ast_stmt(kw, content, exts))
						},
						_ => panic!(),
					}
				} else {
					unimplemented!();
				}
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
						(ExtType::Expr, ParsingData::Expr(expr)) => {
							self.debug.log_deindent("Done parsing main content");
							content.insert(expr.map(StmtExt::Expr));
						},
						_ => unimplemented!(),
					}
				} else {
					panic!()
				}
			},
			ParsingAction::ParseExpr => {
				self.debug.log_normal_indent("Parsing expression");
				let (tok, loc) = self.tb.pop();
				self.debug.log_consume_token(&tok);
				match tok {
					Tok::Integer(integer_string) => self.data_stack.push(ParsingData::Expr(
						Node::from(Expr::IntegerLiteral(integer_string), loc),
					)),
					_ => self
						.data_stack
						.push(ParsingData::Expr(Node::from(Expr::NothingLiteral, loc))),
				}
				self.debug.log_deindent("Done parsing expression");
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
	name: String,
	name_article: String,
	content_type: ExtType,
	extentions: HashMap<Kw, StmtExtDescr>,
}

struct LanguageDescr {
	stmts: HashMap<Kw, StmtDescr>,
}

impl LanguageDescr {
	fn new() -> LanguageDescr {
		let mut stmts = HashMap::new();
		stmts.insert(
			Kw::Np,
			StmtDescr {
				name: "nop".to_string(),
				name_article: "a".to_string(),
				content_type: ExtType::None,
				extentions: HashMap::new(),
			},
		);
		stmts.insert(
			Kw::Pr,
			StmtDescr {
				name: "print".to_string(),
				name_article: "a".to_string(),
				content_type: ExtType::Expr,
				extentions: HashMap::new(),
			},
		);
		LanguageDescr { stmts }
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
