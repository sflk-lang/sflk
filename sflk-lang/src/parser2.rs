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

/// The idea here is to get a parser that can perform one small step at a time
/// so that it can be quite verbose in explaining what it does, as well as being
/// interactive and all.
/// It has a stack of `ParsingFrame`, each of which sould represent a simple step
/// that is to be performed or advanced in the next step call (possibly pushing
/// more frames). For now a `ParsingFrame` does not quite represent a simple step
/// and the whole code is messy and not very readable. This has to be improved.
/// TODO.
enum ParsingFrame {
	BlockLevel {
		stmts: Vec<Node<Stmt>>,
		expected_terminator: BlockLevelExpectedTerminator,
	},
	Stmt {
		kw: Node<Kw>,
		content: Option<Node<StmtExt>>,
		exts: HashMap<Kw, Vec<StmtExt>>,
	},
	Ext(Kw),
	Expr(Option<Node<Expr>>),
	ExprEnd,
}

pub struct ParserDebuggingLogger {
	pub logger: Option<IndentedLogger>,
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

	fn log_deindent(&mut self) {
		if let Some(logger) = &mut self.logger {
			logger.deindent();
		}
	}

	fn log_encounter(&mut self, string: &str) {
		if let Some(logger) = &mut self.logger {
			logger.log_string(&format!("Encounter {}", string), styles::NORMAL);
		}
	}
}

pub struct Parser {
	tb: TokBuffer,
	stack: Vec<ParsingFrame>,
	done: bool,
	lang: LanguageDescr,
	debug: ParserDebuggingLogger,
}

impl Parser {
	pub fn new(tb: TokBuffer, debug: ParserDebuggingLogger) -> Parser {
		Parser {
			tb,
			stack: Vec::new(),
			done: false,
			lang: LanguageDescr::new(),
			debug,
		}
	}

	pub fn parse_program(&mut self) -> Node<Program> {
		self.stack.push(ParsingFrame::BlockLevel {
			stmts: Vec::new(),
			expected_terminator: BlockLevelExpectedTerminator::Eof,
		});
		self.parse();
		match self.stack.pop() {
			Some(ParsingFrame::BlockLevel { stmts, .. }) => {
				Node::from(Program { stmts }, Loc::total_of(self.tb.scu()))
			},
			_ => panic!(),
		}
	}

	fn parse(&mut self) {
		loop {
			self.parse_step();
			if self.done {
				break;
			}
		}
	}

	fn parse_step(&mut self) {
		let (tok, loc) = self.tb.pop();
		self.debug.log_encounter(&format!("{}", tok));
		let mut top_frame = self.stack.last_mut().unwrap();
		match &mut top_frame {
			ParsingFrame::BlockLevel { expected_terminator, .. } => match tok {
				Tok::Eof => {
					if matches!(expected_terminator, BlockLevelExpectedTerminator::Eof) {
						self.done = true;
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
						panic!("Unexpected right curly at line {}", loc.line());
					}
				},
				Tok::Kw(kw) => {
					let stmt_descr = self.lang.stmts.get(&kw);
					if let Some(stmt_descr) = stmt_descr {
						self.stack.push(ParsingFrame::Stmt {
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
								self.stack.push(ParsingFrame::Expr(None));
								self.debug.log_normal_indent(&format!(
									"Parsing expression that will be \
									the main content of the current {} statement",
									&stmt_descr.name
								));
							},
							_ => unimplemented!(),
						}
					} else {
						self.debug.log_normal("TODO: handle keyword");
					}
				},
				_ => {
					self.debug.log_normal("TODO: handle token");
				},
			},
			ParsingFrame::Expr(expr) => {
				if expr.is_none() {
					self.debug.log_normal("TODO: parse expression");
					expr.insert(Node::from(Expr::NothingLiteral, loc));
				} else {
					self.stack.push(ParsingFrame::ExprEnd);
					self.debug.log_normal("Done parsing expression");
					self.debug.log_deindent();
				}
			},
			ParsingFrame::ExprEnd => {
				self.stack.pop();
				let expr_frame = self.stack.pop();
				assert!(matches!(expr_frame, Some(ParsingFrame::Expr(_))));
				match self.stack.last_mut().unwrap() {
					ParsingFrame::Stmt { kw, content, exts } => {
						let expr_node = match expr_frame.unwrap() {
							ParsingFrame::Expr(expr) => expr,
							_ => panic!("bug"),
						}
						.unwrap();
						assert!(content.is_none());
						content.insert(expr_node.map(StmtExt::Expr));
						self.debug.log_normal("Done parsing statement main content");
					},
					_ => unimplemented!(),
				}
			},
			ParsingFrame::Stmt { kw, content, exts } => {
				if content.is_some() || (content.is_none() && matches!(
					self.lang.stmts.get(kw.unwrap_ref()).unwrap().content_type,
					ExtType::None
				)) {
					self.debug.log_normal("Done parsing statement");
					self.debug.log_deindent();
					if let ParsingFrame::Stmt { kw, content, exts } = self.stack.pop().unwrap() {
						match self.stack.last_mut().unwrap() {
							ParsingFrame::BlockLevel { stmts, .. } => {
								stmts.push(temporary_into_ast_stmt(kw, content, exts))
							},
							_ => panic!("bug"),
						}
					} else {
						panic!("bug");
					}
				} else {
					unimplemented!();
				}
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
						_ => panic!("bug"),
					})
				},
			},
			kw_loc,
		),
		_ => unimplemented!(),
	}
}
