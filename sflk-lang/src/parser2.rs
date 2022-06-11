use std::collections::HashMap;
use crate::{
	ast::{Node, Program, Stmt},
	log_indent::IndentedLogger,
	scu::Loc,
	tokenizer::{Kw, Tok, TokBuffer},
	utils::styles,
};

enum ParsingFrame {
	Program(Program),
	Stmt(Stmt),
}

pub struct ParserDebuggingLogger {
	pub logger: IndentedLogger,
}

pub struct Parser {
	tb: TokBuffer,
	stack: Vec<ParsingFrame>,
	done: bool,
	debug: Option<ParserDebuggingLogger>,
}

impl Parser {
	pub fn new(tb: TokBuffer, debug: Option<ParserDebuggingLogger>) -> Parser {
		Parser { tb, stack: Vec::new(), done: false, debug }
	}

	pub fn parse_program(&mut self) -> Node<Program> {
		self.stack
			.push(ParsingFrame::Program(Program { stmts: Vec::new() }));
		self.parse();
		match self.stack.pop() {
			Some(ParsingFrame::Program(program)) => {
				Node::from(program, Loc::total_of(self.tb.scu()))
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

	fn log_normal(&mut self, string: &str) {
		if let Some(debug) = &mut self.debug {
			debug.logger.log_string(string, styles::NORMAL);
		}
	}

	fn log_encounter(&mut self, string: &str) {
		if let Some(debug) = &mut self.debug {
			debug
				.logger
				.log_string(&format!("Encounter: {}", string), styles::NORMAL);
		}
	}

	fn parse_step(&mut self) {
		let (tok, loc) = self.tb.pop();
		self.log_encounter(&format!("{}", tok));
		let mut top_frame = self.stack.last_mut().unwrap();
		match &mut top_frame {
			ParsingFrame::Program(program) => {
				match tok {
					Tok::Eof => {
						self.done = true;
					},
					Tok::Kw(Kw::Np) => {
						program.stmts.push(Node::from(Stmt::Nop, loc));
					},
					Tok::Kw(Kw::Pr) => {
						self.log_normal("TODO");
					},
					_ => {
						self.log_normal("TODO");
					},
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
				content_type: ExtType::None,
				extentions: HashMap::new(),
			},
		);
		stmts.insert(
			Kw::Pr,
			StmtDescr {
				content_type: ExtType::Expr,
				extentions: HashMap::new(),
			},
		);
		LanguageDescr { stmts }
	}
}
