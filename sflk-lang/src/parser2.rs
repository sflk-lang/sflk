use crate::{
	ast::{Node, Program, Stmt},
	log_indent::IndentedLogger,
	scu::Loc,
	tokenizer::{Tok, TokBuffer, Kw},
	utils::styles,
};

enum ParserFrame {
	Program(Program),
}

pub struct ParserDebuggingLogger {
	pub logger: IndentedLogger,
}

pub struct Parser {
	tb: TokBuffer,
	stack: Vec<ParserFrame>,
	done: bool,
	debug: Option<ParserDebuggingLogger>,
}

impl Parser {
	pub fn new(tb: TokBuffer, debug: Option<ParserDebuggingLogger>) -> Parser {
		Parser { tb, stack: Vec::new(), done: false, debug }
	}

	pub fn parse_program(&mut self) -> Node<Program> {
		self.stack
			.push(ParserFrame::Program(Program { stmts: Vec::new() }));
		self.parse();
		match self.stack.pop() {
			Some(ParserFrame::Program(program)) => {
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
		let mut top_frame = self.stack.pop().unwrap();
		match &mut top_frame {
			ParserFrame::Program(program) => {
				let (tok, loc) = self.tb.peek(0).clone();
				self.log_encounter(&format!("{}", tok));
				match tok {
					Tok::Eof => {
						self.done = true;
					},
					Tok::Kw(Kw::Np) => {
						self.tb.pop();
						program.stmts.push(Node::from(Stmt::Nop, loc));
					},
					_ => {
						self.tb.pop();
						self.log_normal("TODO");
					},
				}
			},
			_ => {
				self.tb.pop();
				unimplemented!();
			},
		}
		self.stack.push(top_frame);
	}
}
