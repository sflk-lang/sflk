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
	fmt,
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

enum ParsingData {
	BlockLevel {
		stmts: Vec<Node<Stmt>>,
		expected_terminator: BlockLevelExpectedTerminator,
		begin: Option<Node<()>>,
		end: Option<Node<()>>,
	},
	Stmt {
		kw: Node<Kw>,
		content: Option<Node<StmtExt>>,
		exts: HashMap<Kw, Vec<Node<StmtExt>>>,
	},
	ExtKw(Node<Kw>),
	AssignmentStmt {
		target: Node<TargetExpr>,
		content: Option<Node<Expr>>,
	},
	Expr {
		init: Node<Expr>,
		chops: Vec<Chop>,
	},
	Binop(Node<Binop>),
	StmtInvalid {
		error: Node<Expr>,
	},
	Nothing,
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
#[derive(Clone, Copy)]
enum ParsingAction {
	Terminate,
	ParseStmtOrStop,
	AddRightCurly,
	ParseStmt,     // ( -- stmt)
	AddStmt,       // (block stmt -- block)
	ParseContent,  // (stmt -- stmt content)
	SetContent,    // (stmt content -- stmt)
	ConfirmToLeft, // (stmt -- stmt)
	ParseExtOrStop,
	ParseExt,          // (stmt -- stmt kw ext)
	SetExt,            // (stmt kw ext -- stmt)
	ParseExpr,         // ( -- expr)
	ParseNonChainExpr, // ( -- expr)
	ParseChopOrStop,
	ParseChop,          // ( -- binop expr)
	AddChop,            // (expr binop expr -- expr)
	BlockLevelIntoExpr, // (block -- expr)
}

impl fmt::Display for ParsingAction {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			ParsingAction::Terminate => write!(f, "Terminate"),
			ParsingAction::ParseStmtOrStop => write!(f, "ParseStmtOrStop"),
			ParsingAction::AddRightCurly => write!(f, "AddRightCurly"),
			ParsingAction::ParseStmt => write!(f, "ParseStmt"),
			ParsingAction::AddStmt => write!(f, "AddStmt"),
			ParsingAction::ParseContent => write!(f, "ParseContent"),
			ParsingAction::SetContent => write!(f, "SetContent"),
			ParsingAction::ConfirmToLeft => write!(f, "ConfirmToLeft"),
			ParsingAction::ParseExtOrStop => write!(f, "ParseExtOrStop"),
			ParsingAction::ParseExt => write!(f, "ParseExt"),
			ParsingAction::SetExt => write!(f, "SetExt"),
			ParsingAction::ParseExpr => write!(f, "ParseExpr"),
			ParsingAction::ParseNonChainExpr => write!(f, "ParseNonChainExpr"),
			ParsingAction::ParseChopOrStop => write!(f, "ParseChopOrStop"),
			ParsingAction::ParseChop => write!(f, "ParseChop"),
			ParsingAction::AddChop => write!(f, "AddChop"),
			ParsingAction::BlockLevelIntoExpr => write!(f, "BlockLevelIntoExpr"),
		}
	}
}

pub struct ParserDebuggingLogger {
	pub logger: Option<IndentedLogger>,
	pub log_lines: bool,
	pub log_actions: bool,
	pub last_line: usize,
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

	fn log_action(&mut self, action: ParsingAction) {
		if let Some(logger) = &mut self.logger {
			if self.log_actions {
				logger.log_string(&format!("Action {}", action), styles::NORMAL);
			}
		}
	}

	fn log_line_number_if_new(&mut self, line: usize) {
		if let Some(logger) = &mut self.logger {
			if self.log_lines && self.last_line != line {
				logger.log_string(&format!("Line {}", line), styles::NORMAL);
				self.last_line = line;
			}
		}
	}

	fn log_peek_token(&mut self, tok: &Tok, loc: &Loc) {
		self.log_line_number_if_new(loc.line());
		if let Some(logger) = &mut self.logger {
			logger.log_string(&format!("Peek {}", tok), styles::NORMAL);
		}
	}

	fn log_consume_token(&mut self, tok: &Tok, loc: &Loc) {
		self.log_line_number_if_new(loc.line());
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
			begin: None,
			end: None,
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
		let action = self.action_stack.pop().unwrap();
		self.debug.log_action(action);
		match action {
			ParsingAction::Terminate => panic!(),
			ParsingAction::ParseStmtOrStop => {
				let (tok, loc) = self.tb.peek(0);
				self.debug.log_peek_token(tok, loc);
				if let ParsingData::BlockLevel { expected_terminator, end, .. } =
					self.data_stack.last_mut().unwrap()
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
								self.action_stack.push(ParsingAction::AddRightCurly);
							} else {
								panic!("Unexpected right curly at line {}", loc.line());
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
			ParsingAction::AddRightCurly => {
				let (tok, loc) = self.tb.pop();
				self.debug.log_consume_token(&tok, &loc);
				if let ParsingData::BlockLevel { end, .. } = self.data_stack.last_mut().unwrap() {
					end.insert(Node::from((), loc.clone()));
				} else {
					panic!();
				}
			},
			ParsingAction::ParseStmt => {
				let (tok, loc) = self.tb.pop();
				self.debug.log_consume_token(&tok, &loc);
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
							exts: stmt_descr.initial_exts_map(),
						});
						if !stmt_descr.extentions.is_empty() {
							self.action_stack.push(ParsingAction::ParseExtOrStop);
						}
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
				} else if let Tok::Name { string, .. } = tok {
					self.debug
						.log_normal_indent("Parsing an assignment statement");
					self.data_stack.push(ParsingData::AssignmentStmt {
						target: Node::from(TargetExpr::VariableName(string), loc),
						content: None,
					});
					self.action_stack.push(ParsingAction::SetContent);
					self.action_stack.push(ParsingAction::ParseContent);
					self.action_stack.push(ParsingAction::ConfirmToLeft);
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
					let stmt_descr = self.lang.stmts.get(kw.unwrap_ref()).unwrap();
					for (ext_kw, ext_descr) in stmt_descr.extentions.iter() {
						if !ext_descr.optional && exts.get(ext_kw).unwrap().is_empty() {
							panic!(
								"Extention {} is non-optional but \
								does not appear in the same statement",
								ext_kw
							);
						}
					}
					match self.data_stack.last_mut().unwrap() {
						ParsingData::BlockLevel { stmts, .. } => {
							stmts.push(temporary_into_ast_stmt(kw, content, exts));
						},
						_ => panic!(),
					}
					self.debug.log_deindent("Done parsing statement");
				},
				ParsingData::AssignmentStmt { target, content } => {
					match self.data_stack.last_mut().unwrap() {
						ParsingData::BlockLevel { stmts, .. } => {
							stmts.push(temporary_assignment_into_ast_stmt(target, content));
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
				if let ParsingData::Stmt { kw, .. } = self.data_stack.last().unwrap() {
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
				} else if let ParsingData::AssignmentStmt { .. } = self.data_stack.last().unwrap() {
					self.debug
						.log_normal_indent("Parsing expression that is the main content");
					self.action_stack.push(ParsingAction::ParseExpr);
				} else {
					panic!();
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
				} else if let ParsingData::AssignmentStmt { content, .. } =
					self.data_stack.last_mut().unwrap()
				{
					self.debug.log_deindent("Done parsing expression");
					self.debug.log_deindent("Done parsing main content");
					if let ParsingData::Expr { init, chops } = content_data {
						content.insert(temporary_into_ast_expr(init, chops));
					} else {
						panic!();
					}
				} else {
					panic!();
				}
			},
			ParsingAction::ConfirmToLeft => {
				let (tok, loc) = self.tb.pop();
				self.debug.log_consume_token(&tok, &loc);
				if let Tok::Op(Op::ToLeft) = tok {
				} else {
					self.debug.log_error(&format!(
						"Expected {} for assignment statement but got {} at line {}",
						Tok::Op(Op::ToLeft),
						tok,
						loc.line()
					));
				}
			},
			ParsingAction::ParseExtOrStop => {
				let (tok, loc) = self.tb.peek(0);
				self.debug.log_peek_token(tok, loc);
				let has_ext = if let Tok::Kw(ext_kw) = tok {
					if let ParsingData::Stmt { kw, .. } = self.data_stack.last().unwrap() {
						self.lang
							.stmts
							.get(kw.unwrap_ref())
							.unwrap()
							.extentions
							.contains_key(ext_kw)
					} else {
						false
					}
				} else {
					false
				};
				if has_ext {
					self.action_stack.push(ParsingAction::ParseExtOrStop);
					self.action_stack.push(ParsingAction::SetExt);
					self.action_stack.push(ParsingAction::ParseExt);
				}
			},
			ParsingAction::ParseExt => {
				let (tok, loc) = self.tb.pop();
				self.debug.log_consume_token(&tok, &loc);
				let ext_kw = if let Tok::Kw(ext_kw) = tok {
					ext_kw
				} else {
					panic!()
				};
				let ext_descr =
					if let ParsingData::Stmt { kw, .. } = self.data_stack.last().unwrap() {
						self.lang
							.stmts
							.get(kw.unwrap_ref())
							.unwrap()
							.extentions
							.get(&ext_kw)
							.unwrap()
					} else {
						panic!();
					};
				if !ext_descr.can_stack {
					if let ParsingData::Stmt { exts, .. } = self.data_stack.last().unwrap() {
						if !exts.get(&ext_kw).unwrap().is_empty() {
							panic!(
								"Extention {} is non-stackable but appears \
								multiple times in the same statement",
								ext_kw
							);
						}
					} else {
						panic!();
					};
				}
				self.debug
					.log_normal_indent(&format!("Parsing extention {}", ext_kw));
				self.data_stack
					.push(ParsingData::ExtKw(Node::from(ext_kw, loc)));
				match ext_descr.content_type {
					ExtType::None => {
						self.data_stack.push(ParsingData::Nothing);
					},
					ExtType::Expr => {
						self.action_stack.push(ParsingAction::ParseExpr);
					},
					ExtType::Stmt => {
						self.action_stack.push(ParsingAction::ParseStmt);
					},
					ExtType::Targ => todo!(),
				}
			},
			ParsingAction::SetExt => {
				let content_data = self.data_stack.pop().unwrap();
				let ext_kw = if let ParsingData::ExtKw(ext_kw) = self.data_stack.pop().unwrap() {
					ext_kw
				} else {
					panic!()
				};
				if let ParsingData::Stmt { kw, exts, .. } = self.data_stack.last_mut().unwrap() {
					let stmt_descr = self.lang.stmts.get(kw.unwrap_ref()).unwrap();
					match (
						&stmt_descr
							.extentions
							.get(ext_kw.unwrap_ref())
							.unwrap()
							.content_type,
						content_data,
					) {
						(ExtType::None, ParsingData::Nothing) => {
							self.debug.log_deindent(&format!(
								"Done parsing extention {}",
								ext_kw.unwrap_ref()
							));
							exts.get_mut(ext_kw.unwrap_ref())
								.unwrap()
								.push(Node::from(StmtExt::None, ext_kw.loc().clone()));
						},
						(ExtType::Stmt, ParsingData::Stmt { kw: kw_, content, exts: exts_ }) => {
							self.debug.log_deindent("Done parsing statement");
							self.debug.log_deindent(&format!(
								"Done parsing extention {}",
								ext_kw.unwrap_ref()
							));
							exts.get_mut(ext_kw.unwrap_ref()).unwrap().push(
								temporary_into_ast_stmt(kw_, content, exts_).map(StmtExt::Stmt),
							);
						},
						(ExtType::Stmt, ParsingData::AssignmentStmt { target, content }) => {
							self.debug.log_deindent("Done parsing statement");
							self.debug.log_deindent(&format!(
								"Done parsing extention {}",
								ext_kw.unwrap_ref()
							));
							exts.get_mut(ext_kw.unwrap_ref()).unwrap().push(
								temporary_assignment_into_ast_stmt(target, content)
									.map(StmtExt::Stmt),
							);
						},
						(ExtType::Expr, ParsingData::Expr { init, chops }) => {
							self.debug.log_deindent("Done parsing expression");
							self.debug.log_deindent(&format!(
								"Done parsing extention {}",
								ext_kw.unwrap_ref()
							));
							exts.get_mut(ext_kw.unwrap_ref())
								.unwrap()
								.push(temporary_into_ast_expr(init, chops).map(StmtExt::Expr));
						},
						_ => unimplemented!(),
					}
				} else {
					panic!();
				}
			},
			ParsingAction::ParseNonChainExpr => {
				let (tok, loc) = self.tb.pop();
				self.debug.log_consume_token(&tok, &loc);
				match tok {
					Tok::Integer(integer_string) => self.data_stack.push(ParsingData::Expr {
						init: Node::from(Expr::IntegerLiteral(integer_string), loc),
						chops: Vec::new(),
					}),
					Tok::String { content, .. } => self.data_stack.push(ParsingData::Expr {
						init: Node::from(Expr::StringLiteral(content), loc),
						chops: Vec::new(),
					}),
					Tok::Name { string, .. } => self.data_stack.push(ParsingData::Expr {
						init: Node::from(Expr::VariableName(string), loc),
						chops: Vec::new(),
					}),
					Tok::Left(Matched::Curly) => {
						self.data_stack.push(ParsingData::BlockLevel {
							stmts: Vec::new(),
							expected_terminator: BlockLevelExpectedTerminator::RightCurly,
							begin: Some(Node::from((), loc)),
							end: None,
						});
						self.action_stack.push(ParsingAction::BlockLevelIntoExpr);
						self.action_stack.push(ParsingAction::ParseStmtOrStop);
					},
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
				self.debug.log_peek_token(tok, loc);
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
				self.debug.log_consume_token(&tok, &loc);
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
			ParsingAction::BlockLevelIntoExpr => {
				if let ParsingData::BlockLevel { stmts, begin, end, .. } =
					self.data_stack.pop().unwrap()
				{
					let loc = begin.unwrap().loc() + end.unwrap().loc();
					self.data_stack.push(ParsingData::Expr {
						init: Node::from(Expr::BlockLiteral(stmts), loc),
						chops: Vec::new(),
					})
				} else {
					panic!();
				}
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
		stmts.insert(
			Kw::If,
			StmtDescr {
				name_article: "an".to_string(),
				name: "if".to_string(),
				content_type: ExtType::Expr,
				extentions: {
					let mut exts = HashMap::new();
					exts.insert(
						Kw::Th,
						StmtExtDescr {
							content_type: ExtType::Stmt,
							optional: true,
							can_stack: true,
						},
					);
					exts.insert(
						Kw::El,
						StmtExtDescr {
							content_type: ExtType::Stmt,
							optional: true,
							can_stack: true,
						},
					);
					exts
				},
			},
		);
		stmts.insert(
			Kw::Lp,
			StmtDescr {
				name_article: "a".to_string(),
				name: "loop".to_string(),
				content_type: ExtType::None,
				extentions: {
					let mut exts = HashMap::new();
					exts.insert(
						Kw::Wh,
						StmtExtDescr {
							content_type: ExtType::Expr,
							optional: true,
							can_stack: true,
						},
					);
					exts.insert(
						Kw::Bd,
						StmtExtDescr {
							content_type: ExtType::Stmt,
							optional: true,
							can_stack: true,
						},
					);
					exts.insert(
						Kw::Sp,
						StmtExtDescr {
							content_type: ExtType::Stmt,
							optional: true,
							can_stack: true,
						},
					);
					exts.insert(
						Kw::Ao,
						StmtExtDescr {
							content_type: ExtType::None,
							optional: true,
							can_stack: false,
						},
					);
					exts
				},
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

impl StmtDescr {
	fn initial_exts_map(&self) -> HashMap<Kw, Vec<Node<StmtExt>>> {
		let mut exts = HashMap::new();
		for ext_kw in self.extentions.keys() {
			exts.insert(*ext_kw, Vec::new());
		}
		exts
	}
}

// TODO: Stop having to use `ast::Stmt` this early, or maybe at all.
fn temporary_into_ast_stmt(
	kw: Node<Kw>,
	content: Option<Node<StmtExt>>,
	mut exts: HashMap<Kw, Vec<Node<StmtExt>>>,
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
		Kw::If => Node::from(
			Stmt::If {
				cond_expr: {
					let node = content.unwrap();
					node.map(|ext| match ext {
						StmtExt::Expr(expr) => expr,
						_ => panic!(),
					})
				},
				th_stmts: exts
					.remove(&Kw::Th)
					.unwrap()
					.into_iter()
					.map(|stmt_ext| {
						let loc = stmt_ext.loc().clone();
						match stmt_ext.unwrap() {
							StmtExt::Stmt(stmt) => Node::from(stmt, loc),
							_ => panic!(),
						}
					})
					.collect(),
				el_stmts: exts
					.remove(&Kw::El)
					.unwrap()
					.into_iter()
					.map(|stmt_ext| {
						let loc = stmt_ext.loc().clone();
						match stmt_ext.unwrap() {
							StmtExt::Stmt(stmt) => Node::from(stmt, loc),
							_ => panic!(),
						}
					})
					.collect(),
			},
			kw_loc,
		),
		Kw::Lp => Node::from(
			Stmt::Loop {
				wh_exprs: exts
					.remove(&Kw::Wh)
					.unwrap()
					.into_iter()
					.map(|expr_ext| {
						let loc = expr_ext.loc().clone();
						match expr_ext.unwrap() {
							StmtExt::Expr(expr) => Node::from(expr, loc),
							_ => panic!(),
						}
					})
					.collect(),
				bd_stmts: exts
					.remove(&Kw::Bd)
					.unwrap()
					.into_iter()
					.map(|stmt_ext| {
						let loc = stmt_ext.loc().clone();
						match stmt_ext.unwrap() {
							StmtExt::Stmt(stmt) => Node::from(stmt, loc),
							_ => panic!(),
						}
					})
					.collect(),
				sp_stmts: exts
					.remove(&Kw::Sp)
					.unwrap()
					.into_iter()
					.map(|stmt_ext| {
						let loc = stmt_ext.loc().clone();
						match stmt_ext.unwrap() {
							StmtExt::Stmt(stmt) => Node::from(stmt, loc),
							_ => panic!(),
						}
					})
					.collect(),
				ao_flag: exts
					.remove(&Kw::Ao)
					.unwrap()
					.into_iter()
					.next()
					.map(|node| node.map(|non_ext| ())),
			},
			kw_loc,
		),
		_ => unimplemented!(),
	}
}

fn temporary_assignment_into_ast_stmt(
	target: Node<TargetExpr>,
	content: Option<Node<Expr>>,
) -> Node<Stmt> {
	let loc = target.loc() + content.as_ref().unwrap().loc();
	Node::from((Stmt::Assign { target, expr: content.unwrap() }), loc)
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
