//! This parser consumes one token at a time in a loop, and keeps track
//! of the context by manipulating two stacks: a data stack and an action stack.
//! These stacks are in `Parser`.
//!
//! At each iteration, an action of the action stack is popped and executed.
//! One action can either peek on the next token, consume the next token or
//! don't look at the nect token at all. This restriction allows for the
//! parsing loop to guarentee that at most one token is consumed or even
//! considered at each iteration. This makes the parser eaiser to debug,
//! and will allow for parsing directives to be insertable inbetween any
//! two consecutive tokens.
//!
//! The action stack allows the parser to keep track of its future tasks,
//! and the data stack allows it to keep track of what has already been
//! partially parsed.

use crate::{
	ast::{Chop as AstChop, Expr, Node, Program, Stmt, TargetExpr, Unop as AstUnop},
	log_indent::IndentedLogger,
	scu::Loc,
	tokenizer::{Kw, Matched, Op, SimpleTok, Tok, TokBuffer},
	utils::styles,
};
use std::{collections::HashMap, convert::TryFrom, fmt};

#[derive(Debug)]
pub struct ParsingWarning {
	// TODO
}

enum ParsingData {
	BlockLevel {
		stmts: Vec<Node<Stmt>>,
		expected_terminator: BlockLevelExpectedTerminator,
		left_curly: Option<Node<()>>,
		right_curly: Option<Node<()>>,
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
		init: Option<Node<Expr>>,
		chops: Vec<Chop>,
		left_paren: Option<Node<()>>,
	},
	LeftParen(Node<()>),
	Binop(Node<Binop>),
	Unop(Node<Unop>),
	StmtInvalid {
		error: Node<Expr>,
	},
	Nothing,
	Targ(Node<TargetExpr>),
}

enum StmtExt {
	Stmt(Stmt),
	Expr(Expr),
	Targ(TargetExpr),
	None,
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
	ToRight,
	Comma,
	DoubleComma,
	Ix,
}

#[derive(Clone, Copy)]
enum Unop {
	Minus,
	File,
	Ordered,
	OrderedStrictly,
	Length,
}

enum BlockLevelExpectedTerminator {
	Eof,
	RightCurly,
}

/// Each of these represents one simple action that the parser may perform.
/// When an action is popped from the action stack, it can modify the data stack as
/// it pleases, it can push actions on the action stack, but it can either consume at
/// most one token or peek at most one token or not look at the tokens at all.
///
/// The restriction on the way the token stream is consumed will allow for debugging
/// parsing directives to be insertable anywhere in between tokens and it will allow the
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
	BangOrToLeft,  // (stmt -- stmt)
	ParseExtOrStop,
	ParseExt,          // (stmt -- stmt kw ext)
	SetExt,            // (stmt kw ext -- stmt)
	ParseExpr,         // ( -- expr)
	AddUnop,           // (unop expr -- expr)
	ParseNonChainExpr, // ( -- [paren] expr)
	ParseDottedString, // ( -- expr)
	ParseChopOrStop,
	ConsumeDot,
	ConsumeRightParen,  // (paren expr -- expr)
	ParseChop,          // ( -- binop expr)
	AddChop,            // (expr binop expr -- expr)
	BlockLevelIntoExpr, // (block -- expr)
	ParseTarg,          // ( -- targ)
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
			ParsingAction::BangOrToLeft => write!(f, "BangOrToLeft"),
			ParsingAction::ParseExtOrStop => write!(f, "ParseExtOrStop"),
			ParsingAction::ParseExt => write!(f, "ParseExt"),
			ParsingAction::SetExt => write!(f, "SetExt"),
			ParsingAction::ParseExpr => write!(f, "ParseExpr"),
			ParsingAction::AddUnop => write!(f, "AddUnop"),
			ParsingAction::ParseNonChainExpr => write!(f, "ParseNonChainExpr"),
			ParsingAction::ParseDottedString => write!(f, "ParseDottedString"),
			ParsingAction::ParseChopOrStop => write!(f, "ParseChopOrStop"),
			ParsingAction::ConsumeDot => write!(f, "ConsumeDot"),
			ParsingAction::ConsumeRightParen => write!(f, "ConsumeRightParen"),
			ParsingAction::ParseChop => write!(f, "ParseChop"),
			ParsingAction::AddChop => write!(f, "AddChop"),
			ParsingAction::BlockLevelIntoExpr => write!(f, "BlockLevelIntoExpr"),
			ParsingAction::ParseTarg => write!(f, "ParseTarg"),
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
				logger.log_string(&format!("Action {}", action), styles::YELLOW);
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
			logger.log_string(&format!("Consume {}", tok), styles::BOLD);
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
			left_curly: None,
			right_curly: None,
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

	fn peek_tok(&mut self) -> (Tok, Loc) {
		let (tok, loc) = self.tb.peek(0).clone();
		self.debug.log_peek_token(&tok, &loc);
		(tok, loc)
	}

	fn consume_tok(&mut self) -> (Tok, Loc) {
		let (tok, loc) = self.tb.pop();
		self.debug.log_consume_token(&tok, &loc);
		(tok, loc)
	}

	/// This is the core of the whole parser.
	///
	/// TODO: Document what goes on in there.
	fn perform_one_action(&mut self) {
		let action = self.action_stack.pop().unwrap();
		self.debug.log_action(action);
		match action {
			ParsingAction::Terminate => panic!(),
			ParsingAction::ParseStmtOrStop => {
				let (tok, loc) = self.peek_tok();
				if let ParsingData::BlockLevel { expected_terminator, .. } =
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
				let (tok, loc) = self.consume_tok();
				assert!(matches!(tok, Tok::Right(Matched::Curly)));
				if let ParsingData::BlockLevel { right_curly: end, .. } =
					self.data_stack.last_mut().unwrap()
				{
					*end = Some(Node::from((), loc));
				} else {
					panic!();
				}
			},
			ParsingAction::ParseStmt => {
				let (tok, loc) = self.consume_tok();
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
							ContentType::None => (),
							ContentType::Expr => {
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
					self.action_stack.push(ParsingAction::BangOrToLeft);
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
						ContentType::None => panic!(),
						ContentType::Expr => {
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
						(ContentType::None, _) => panic!(),
						(ContentType::Expr, ParsingData::Expr { init, chops, .. }) => {
							self.debug.log_deindent("Done parsing expression");
							self.debug.log_deindent("Done parsing main content");
							*content =
								Some(temporary_into_ast_expr(init, chops).map(StmtExt::Expr));
						},
						_ => unimplemented!(),
					}
				} else if let ParsingData::AssignmentStmt { content, .. } =
					self.data_stack.last_mut().unwrap()
				{
					self.debug.log_deindent("Done parsing expression");
					self.debug.log_deindent("Done parsing main content");
					if let ParsingData::Expr { init, chops, .. } = content_data {
						*content = Some(temporary_into_ast_expr(init, chops));
					} else {
						panic!();
					}
				} else {
					panic!();
				}
			},
			ParsingAction::BangOrToLeft => {
				let (tok, loc) = self.consume_tok();
				if let Tok::Op(Op::ToLeft) = tok {
					// ToLeft confirmed, nothing else to do.
				} else if let Tok::Op(Op::Bang) = tok {
					if let ParsingData::AssignmentStmt { target, .. } =
						self.data_stack.last_mut().unwrap()
					{
						let loc = target.loc().clone();
						let name = if let TargetExpr::VariableName(name) = target.unwrap_ref() {
							name.clone()
						} else {
							panic!()
						};
						*target = Node::from(TargetExpr::DeclVariableName(name), loc);
					} else {
						panic!();
					}
					self.action_stack.push(ParsingAction::ConfirmToLeft);
				} else {
					self.debug.log_error(&format!(
						"Expected {} for assignment statement but got {} at line {}",
						Tok::Op(Op::ToLeft),
						tok,
						loc.line()
					));
				}
			},
			ParsingAction::ConfirmToLeft => {
				let (tok, loc) = self.consume_tok();
				if let Tok::Op(Op::ToLeft) = tok {
					// ToLeft confirmed, nothing else to do.
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
				let (tok, _loc) = self.peek_tok();
				let has_ext = if let Tok::Kw(ext_kw) = tok {
					if let ParsingData::Stmt { kw, .. } = self.data_stack.last().unwrap() {
						self.lang
							.stmts
							.get(kw.unwrap_ref())
							.unwrap()
							.extentions
							.contains_key(&ext_kw)
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
				let (tok, loc) = self.consume_tok();
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
					ContentType::None => {
						self.data_stack.push(ParsingData::Nothing);
					},
					ContentType::Expr => {
						self.action_stack.push(ParsingAction::ParseExpr);
					},
					ContentType::Stmt => {
						self.action_stack.push(ParsingAction::ParseStmt);
					},
					ContentType::Targ => {
						self.action_stack.push(ParsingAction::ParseTarg);
					},
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
						(ContentType::None, ParsingData::Nothing) => {
							self.debug.log_deindent(&format!(
								"Done parsing extention {}",
								ext_kw.unwrap_ref()
							));
							exts.get_mut(ext_kw.unwrap_ref())
								.unwrap()
								.push(Node::from(StmtExt::None, ext_kw.loc().clone()));
						},
						(
							ContentType::Stmt,
							ParsingData::Stmt { kw: kw_, content, exts: exts_ },
						) => {
							self.debug.log_deindent("Done parsing statement");
							self.debug.log_deindent(&format!(
								"Done parsing extention {}",
								ext_kw.unwrap_ref()
							));
							exts.get_mut(ext_kw.unwrap_ref()).unwrap().push(
								temporary_into_ast_stmt(kw_, content, exts_).map(StmtExt::Stmt),
							);
						},
						(ContentType::Stmt, ParsingData::AssignmentStmt { target, content }) => {
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
						(ContentType::Expr, ParsingData::Expr { init, chops, .. }) => {
							self.debug.log_deindent("Done parsing expression");
							self.debug.log_deindent(&format!(
								"Done parsing extention {}",
								ext_kw.unwrap_ref()
							));
							exts.get_mut(ext_kw.unwrap_ref())
								.unwrap()
								.push(temporary_into_ast_expr(init, chops).map(StmtExt::Expr));
						},
						(ContentType::Targ, ParsingData::Targ(targ)) => {
							self.debug.log_deindent(&format!(
								"Done parsing extention {}",
								ext_kw.unwrap_ref()
							));
							exts.get_mut(ext_kw.unwrap_ref())
								.unwrap()
								.push(targ.map(StmtExt::Targ));
						},
						_ => panic!(),
					}
				} else {
					panic!();
				}
			},
			ParsingAction::ParseNonChainExpr => {
				let (tok, loc) = self.consume_tok();
				let simple_tok = SimpleTok::try_from(&tok).ok();
				if simple_tok.is_some()
					&& self.lang.unops.contains_key(simple_tok.as_ref().unwrap())
				{
					self.data_stack.push(ParsingData::Unop(Node::from(
						*self.lang.unops.get(&simple_tok.unwrap()).unwrap(),
						loc,
					)));
					self.action_stack.push(ParsingAction::AddUnop);
					self.action_stack.push(ParsingAction::ParseExpr);
				} else {
					match tok {
						Tok::Kw(Kw::In) => self.data_stack.push(ParsingData::Expr {
							init: Some(Node::from(Expr::Input, loc)),
							chops: Vec::new(),
							left_paren: None,
						}),
						Tok::Kw(Kw::Cx) => self.data_stack.push(ParsingData::Expr {
							init: Some(Node::from(Expr::Context, loc)),
							chops: Vec::new(),
							left_paren: None,
						}),
						Tok::Integer(integer_string) => self.data_stack.push(ParsingData::Expr {
							init: Some(Node::from(Expr::IntegerLiteral(integer_string), loc)),
							chops: Vec::new(),
							left_paren: None,
						}),
						Tok::String { content, .. } => self.data_stack.push(ParsingData::Expr {
							init: Some(Node::from(Expr::StringLiteral(content), loc)),
							chops: Vec::new(),
							left_paren: None,
						}),
						Tok::Op(Op::Dot) => {
							self.action_stack.push(ParsingAction::ParseDottedString);
						},
						Tok::Name { string, .. } => self.data_stack.push(ParsingData::Expr {
							init: Some(Node::from(Expr::VariableName(string), loc)),
							chops: Vec::new(),
							left_paren: None,
						}),
						Tok::Left(Matched::Curly) => {
							self.data_stack.push(ParsingData::BlockLevel {
								stmts: Vec::new(),
								expected_terminator: BlockLevelExpectedTerminator::RightCurly,
								left_curly: Some(Node::from((), loc)),
								right_curly: None,
							});
							self.action_stack.push(ParsingAction::BlockLevelIntoExpr);
							self.action_stack.push(ParsingAction::ParseStmtOrStop);
						},
						Tok::Left(Matched::Paren) => {
							self.data_stack
								.push(ParsingData::LeftParen(Node::from((), loc)));
							self.action_stack.push(ParsingAction::ConsumeRightParen);
							self.action_stack.push(ParsingAction::ParseExpr);
						},
						_ => {
							let error_string = format!("Unexpected token {}", tok);
							self.debug.log_error(&error_string);
							let error_line_string =
								format!("{} on line {}", error_string, loc.line());
							self.data_stack.push(ParsingData::Expr {
								init: Some(Node::from(
									Expr::Invalid {
										error_expr: Box::new(Node::from(
											Expr::StringLiteral(error_line_string),
											loc.clone(),
										)),
									},
									loc,
								)),
								chops: Vec::new(),
								left_paren: None,
							})
						},
					}
				}
			},
			ParsingAction::ParseDottedString => {
				let (tok, loc) = self.consume_tok();
				match tok {
					Tok::Name { string, .. } => self.data_stack.push(ParsingData::Expr {
						init: Some(Node::from(Expr::StringLiteral(string), loc)),
						chops: Vec::new(),
						left_paren: None,
					}),
					Tok::Kw(kw) => self.data_stack.push(ParsingData::Expr {
						init: Some(Node::from(Expr::StringLiteral(kw.to_string()), loc)),
						chops: Vec::new(),
						left_paren: None,
					}),
					_ => {
						let error_string = format!("Unexpected token {} for dotted string", tok);
						self.debug.log_error(&error_string);
						let error_line_string = format!("{} on line {}", error_string, loc.line());
						self.data_stack.push(ParsingData::Expr {
							init: Some(Node::from(
								Expr::Invalid {
									error_expr: Box::new(Node::from(
										Expr::StringLiteral(error_line_string),
										loc.clone(),
									)),
								},
								loc,
							)),
							chops: Vec::new(),
							left_paren: None,
						})
					},
				}
			},
			ParsingAction::ParseExpr => {
				let (tok, loc) = self.peek_tok();
				self.debug.log_normal_indent("Parsing expression");
				if matches!(tok, Tok::Right(Matched::Paren)) {
					if let ParsingData::LeftParen(left) = self.data_stack.last().unwrap() {
						let left_loc = left.loc().clone();
						self.data_stack.push(ParsingData::Expr {
							init: Some(Node::from(Expr::NothingLiteral, left_loc + loc)),
							chops: Vec::new(),
							left_paren: None,
						});
					} else {
						panic!();
					}
				} else {
					self.action_stack.push(ParsingAction::ParseChopOrStop);
					self.action_stack.push(ParsingAction::ParseNonChainExpr);
				}
			},
			ParsingAction::AddUnop => {
				self.debug.log_deindent("Done parsing expression");
				let (init, chops) =
					if let ParsingData::Expr { init, chops, .. } = self.data_stack.pop().unwrap() {
						(init, chops)
					} else {
						panic!();
					};
				let (unop, unop_loc) =
					if let ParsingData::Unop(node) = self.data_stack.pop().unwrap() {
						let loc = node.loc().clone();
						(node.unwrap(), loc)
					} else {
						panic!();
					};
				let expr = temporary_into_ast_expr(init, chops);
				let expr_loc = expr.loc().clone();
				self.data_stack.push(ParsingData::Expr {
					init: Some(Node::from(
						temporary_unop_into_ast_expr(unop, expr),
						unop_loc + expr_loc,
					)),
					chops: Vec::new(),
					left_paren: None,
				});
			},
			ParsingAction::ParseChopOrStop => {
				let (tok, _loc) = self.peek_tok();
				let simple_tok = SimpleTok::try_from(&tok).ok();
				if simple_tok.is_some() && self.lang.binops.contains_key(&simple_tok.unwrap()) {
					self.action_stack.push(ParsingAction::ParseChopOrStop);
					self.action_stack.push(ParsingAction::AddChop);
					self.action_stack.push(ParsingAction::ParseChop);
				} else if matches!(tok, Tok::Op(Op::Dot)) {
					self.action_stack.push(ParsingAction::ConsumeDot);
				}
			},
			ParsingAction::ConsumeDot => {
				let (tok, _loc) = self.consume_tok();
				assert!(matches!(tok, Tok::Op(Op::Dot)));
			},
			ParsingAction::ConsumeRightParen => {
				self.debug.log_deindent("Done parsing expression");
				let (tok, _loc) = self.consume_tok();
				assert!(matches!(tok, Tok::Right(Matched::Paren)));
				let expr = self.data_stack.pop().unwrap();
				let left_paren = self.data_stack.pop().unwrap();
				assert!(matches!(left_paren, ParsingData::LeftParen(_)));
				self.data_stack.push(expr);
			},
			ParsingAction::ParseChop => {
				self.debug.log_normal_indent("Parsing chain operation");
				let (tok, loc) = self.consume_tok();
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
					ParsingData::Expr { init, chops, .. } => temporary_into_ast_expr(init, chops),
					_ => panic!(),
				};
				let binop = match self.data_stack.pop().unwrap() {
					ParsingData::Binop(binop) => binop,
					_ => panic!(),
				};
				if let ParsingData::Expr { chops, .. } = self.data_stack.last_mut().unwrap() {
					chops.push(Chop { binop, expr })
				}
				self.debug.log_deindent("Done parsing chain operation");
			},
			ParsingAction::BlockLevelIntoExpr => {
				if let ParsingData::BlockLevel {
					stmts, left_curly: begin, right_curly: end, ..
				} = self.data_stack.pop().unwrap()
				{
					let loc = begin.unwrap().loc() + end.unwrap().loc();
					self.data_stack.push(ParsingData::Expr {
						init: Some(Node::from(Expr::BlockLiteral(stmts), loc)),
						chops: Vec::new(),
						left_paren: None,
					})
				} else {
					panic!();
				}
			},
			ParsingAction::ParseTarg => {
				let (tok, loc) = self.consume_tok();
				self.data_stack.push(ParsingData::Targ(Node::from(
					match tok {
						Tok::Name { string, .. } => TargetExpr::VariableName(string),
						_ => TargetExpr::Invalid,
					},
					loc,
				)));
			},
		}
	}
}

enum ContentType {
	Stmt,
	Expr,
	Targ,
	/// An extention may consist of just a keyword without any content,
	/// for example the at-least-once (`ao`) extention to loop statements.
	None,
}

/// Description of one possible extention to a parent statement description.
/// For example, the if statement can have then branches that are extentions,
/// so the if statement description should contain one `StmtExtDescr` that
/// describes the fact that then extentions contain statements, are optional
/// and stackable.
struct StmtExtDescr {
	content_type: ContentType,
	optional: bool,
	/// Can this extention be present multiple times in the same statement?
	can_stack: bool,
}

/// Description of one type of statement that must begin with a keyword.
/// For example, an if statement must begin with the `if` keyword, so the
/// language description should contain one `StmtDescr` that describes the
/// fact that if statements contain an extention, and can have then and else
/// extentions, etc.
///
/// The assignment statement does not begin with a keyword and thus is not
/// described by such structure.
struct StmtDescr {
	name_article: String,
	name: String,
	content_type: ContentType,
	extentions: HashMap<Kw, StmtExtDescr>,
}

struct LanguageDescr {
	stmts: HashMap<Kw, StmtDescr>,
	binops: HashMap<SimpleTok, Binop>,
	unops: HashMap<SimpleTok, Unop>,
}

impl LanguageDescr {
	fn new() -> LanguageDescr {
		let mut stmts = HashMap::new();
		stmts.insert(
			Kw::Np,
			StmtDescr {
				name_article: "a".to_string(),
				name: "nop".to_string(),
				content_type: ContentType::None,
				extentions: HashMap::new(),
			},
		);
		stmts.insert(
			Kw::Nl,
			StmtDescr {
				name_article: "a".to_string(),
				name: "newline".to_string(),
				content_type: ContentType::None,
				extentions: HashMap::new(),
			},
		);
		stmts.insert(
			Kw::Pr,
			StmtDescr {
				name_article: "a".to_string(),
				name: "print".to_string(),
				content_type: ContentType::Expr,
				extentions: HashMap::new(),
			},
		);
		stmts.insert(
			Kw::If,
			StmtDescr {
				name_article: "an".to_string(),
				name: "if".to_string(),
				content_type: ContentType::Expr,
				extentions: {
					let mut exts = HashMap::new();
					exts.insert(
						Kw::Th,
						StmtExtDescr {
							content_type: ContentType::Stmt,
							optional: true,
							can_stack: true,
						},
					);
					exts.insert(
						Kw::El,
						StmtExtDescr {
							content_type: ContentType::Stmt,
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
				content_type: ContentType::None,
				extentions: {
					let mut exts = HashMap::new();
					exts.insert(
						Kw::Wh,
						StmtExtDescr {
							content_type: ContentType::Expr,
							optional: true,
							can_stack: true,
						},
					);
					exts.insert(
						Kw::Bd,
						StmtExtDescr {
							content_type: ContentType::Stmt,
							optional: true,
							can_stack: true,
						},
					);
					exts.insert(
						Kw::Sp,
						StmtExtDescr {
							content_type: ContentType::Stmt,
							optional: true,
							can_stack: true,
						},
					);
					exts.insert(
						Kw::Ao,
						StmtExtDescr {
							content_type: ContentType::None,
							optional: true,
							can_stack: false,
						},
					);
					exts
				},
			},
		);
		stmts.insert(
			Kw::Do,
			StmtDescr {
				name_article: "a".to_string(),
				name: "do".to_string(),
				content_type: ContentType::Expr,
				extentions: {
					let mut exts = HashMap::new();
					exts.insert(
						Kw::Wi,
						StmtExtDescr {
							content_type: ContentType::Expr,
							optional: true,
							can_stack: false,
						},
					);
					exts
				},
			},
		);
		stmts.insert(
			Kw::Dh,
			StmtDescr {
				name_article: "a".to_string(),
				name: "do here".to_string(),
				content_type: ContentType::Expr,
				extentions: HashMap::new(),
			},
		);
		stmts.insert(
			Kw::Ev,
			StmtDescr {
				name_article: "an".to_string(),
				name: "evaluate".to_string(),
				content_type: ContentType::Expr,
				extentions: HashMap::new(),
			},
		);
		stmts.insert(
			Kw::Ri,
			StmtDescr {
				name_article: "a".to_string(),
				name: "register interceptor".to_string(),
				content_type: ContentType::Expr,
				extentions: HashMap::new(),
			},
		);
		stmts.insert(
			Kw::Em,
			StmtDescr {
				name_article: "an".to_string(),
				name: "emit".to_string(),
				content_type: ContentType::Expr,
				extentions: {
					let mut exts = HashMap::new();
					exts.insert(
						Kw::Rs,
						StmtExtDescr {
							content_type: ContentType::Targ,
							optional: true,
							can_stack: false,
						},
					);
					exts
				},
			},
		);
		stmts.insert(
			Kw::Cy,
			StmtDescr {
				name_article: "a".to_string(),
				name: "context deployment".to_string(),
				content_type: ContentType::Expr,
				extentions: HashMap::new(),
			},
		);
		let mut binops = HashMap::new();
		binops.insert(SimpleTok::Op(Op::Plus), Binop::Plus);
		binops.insert(SimpleTok::Op(Op::Minus), Binop::Minus);
		binops.insert(SimpleTok::Op(Op::Star), Binop::Star);
		binops.insert(SimpleTok::Op(Op::Slash), Binop::Slash);
		binops.insert(SimpleTok::Op(Op::ToRight), Binop::ToRight);
		binops.insert(SimpleTok::Op(Op::Comma), Binop::Comma);
		binops.insert(SimpleTok::Op(Op::DoubleComma), Binop::DoubleComma);
		binops.insert(SimpleTok::Kw(Kw::Ix), Binop::Ix);
		let mut unops = HashMap::new();
		unops.insert(SimpleTok::Op(Op::Minus), Unop::Minus);
		unops.insert(SimpleTok::Kw(Kw::Fi), Unop::File);
		unops.insert(SimpleTok::Kw(Kw::Od), Unop::Ordered);
		unops.insert(SimpleTok::Kw(Kw::Os), Unop::OrderedStrictly);
		unops.insert(SimpleTok::Kw(Kw::Ln), Unop::Length);
		LanguageDescr { stmts, binops, unops }
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

// The current AST is pretty much legacy and will probably change in the future,
// the code after this point is dedicated to converting code representation
// manipulated here into pieces of legacy AST.

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
					.map(|node| node.map(|_none_ext| ())),
			},
			kw_loc,
		),
		Kw::Do => Node::from(
			Stmt::Do {
				expr: {
					let node = content.unwrap();
					node.map(|ext| match ext {
						StmtExt::Expr(expr) => expr,
						_ => panic!(),
					})
				},
				wi_expr: exts
					.remove(&Kw::Wi)
					.unwrap()
					.into_iter()
					.next()
					.map(|expr_ext| {
						let loc = expr_ext.loc().clone();
						match expr_ext.unwrap() {
							StmtExt::Expr(expr) => Node::from(expr, loc),
							_ => panic!(),
						}
					}),
			},
			kw_loc,
		),
		Kw::Dh => Node::from(
			Stmt::DoHere {
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
		Kw::Ev => Node::from(
			Stmt::Evaluate {
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
		Kw::Ri => Node::from(
			Stmt::RegisterInterceptor {
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
		Kw::Em => Node::from(
			Stmt::Emit {
				expr: {
					let node = content.unwrap();
					node.map(|ext| match ext {
						StmtExt::Expr(expr) => expr,
						_ => panic!(),
					})
				},
				target: exts
					.remove(&Kw::Rs)
					.unwrap()
					.into_iter()
					.next()
					.map(|targ_ext| {
						let loc = targ_ext.loc().clone();
						match targ_ext.unwrap() {
							StmtExt::Targ(targ) => Node::from(targ, loc),
							_ => panic!(),
						}
					}),
			},
			kw_loc,
		),
		Kw::Cy => Node::from(
			Stmt::DeployContext {
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

// TODO: Stop having to use `ast::Stmt` this early, or maybe at all.
fn temporary_assignment_into_ast_stmt(
	target: Node<TargetExpr>,
	content: Option<Node<Expr>>,
) -> Node<Stmt> {
	let loc = target.loc() + content.as_ref().unwrap().loc();
	Node::from(Stmt::Assign { target, expr: content.unwrap() }, loc)
}

// TODO: Stop having to use `ast::Expr` this early, or maybe at all.
fn temporary_into_ast_expr(init: Option<Node<Expr>>, chops: Vec<Chop>) -> Node<Expr> {
	let init = init.unwrap();
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
						Binop::ToRight => Node::from(AstChop::ToRight(expr), loc),
						Binop::Comma => Node::from(AstChop::Comma(expr), loc),
						Binop::DoubleComma => Node::from(AstChop::DoubleComma(expr), loc),
						Binop::Ix => Node::from(AstChop::Index(expr), loc),
					}
				}
			})
			.collect();
		let loc = chops.last().map_or(init_loc, |node| node.loc().clone());
		Node::from(Expr::Chain { init: Box::new(init), chops }, loc)
	}
}

// TODO: Stop having to use `ast::Expr` this early, or maybe at all.
fn temporary_unop_into_ast_expr(unop: Unop, expr: Node<Expr>) -> Expr {
	Expr::Unop(match unop {
		Unop::Minus => AstUnop::Negate(Box::new(expr)),
		Unop::File => AstUnop::ReadFile(Box::new(expr)),
		Unop::Ordered => AstUnop::Ordered(Box::new(expr)),
		Unop::OrderedStrictly => AstUnop::OrderedStrictly(Box::new(expr)),
		Unop::Length => AstUnop::Length(Box::new(expr)),
	})
}
