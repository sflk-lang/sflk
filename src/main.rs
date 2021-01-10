
trait CodeStream {
	fn peek(& self) -> Option<char>;
	fn disc(&mut self);
}

struct CodeStreamString {
	string: String,
	index: usize,
}

impl CodeStreamString {
	fn from(string: &str) -> Self {
		CodeStreamString {
			string: string.to_string(),
			index: 0,
		}
	}
}

impl CodeStream for CodeStreamString {
	fn peek(& self) -> Option<char> {
		self.string[self.index..].chars().next()
	}

	fn disc(&mut self) {
		match self.peek() {
			Some(ch) => self.index += ch.len_utf8(),
			None => (),
		}
	}
}

#[derive(Debug)]
enum ParsingError {
	UnexpectedCharacter(char),
	UnexpectedToken(Tok),
}

impl std::fmt::Display for ParsingError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			ParsingError::UnexpectedCharacter(ch) =>
				write!(f, "unexpected character `{}`", ch),
			ParsingError::UnexpectedToken(tok) =>
				write!(f, "unexpected token `{}`", tok),
		}
	}
}

#[derive(Debug)]
enum Tok {
	Word(String),
	StringLitteral(String),
	OpToLeft,
	Void,
}

impl std::fmt::Display for Tok {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Tok::Word(word) => write!(f, "{}", word),
			Tok::StringLitteral(string) => write!(f, "\"{}\"", string),
			Tok::OpToLeft => write!(f, "<"),
			Tok::Void => write!(f, ""),
		}
	}
}

#[derive(Debug)]
enum Inst {
	Nop,
	PushConst(Obj),
	AssignVar,
	ReadVar,
	Print,
	PushCx,
}

#[derive(Debug)]
struct StmtEx {
	insts: Vec<Inst>,
}

#[derive(Debug)]
enum Stmt {
	Nop,
	Assign(String, String),
	PrintVar(String),
	PrintJej,
}

#[derive(Debug)]
struct Prog {
	stmts: Vec<Stmt>,
}

trait Parsable: CodeStream {
	fn disc_ws(&mut self);
	fn parse_word_string(&mut self) -> String;
	fn parse_string_litteral_string(&mut self) -> String;
	fn parse_tok(&mut self) -> Result<Tok, ParsingError>;
	fn parse_stmt(&mut self) -> Result<Stmt, ParsingError>;
	fn parse_prog(&mut self) -> Result<Prog, ParsingError>;
}

impl<T> Parsable for T where T: CodeStream {
	fn disc_ws(&mut self) {
		loop {
			match self.peek() {
				Some(ch) if ch.is_ascii_whitespace() => self.disc(),
				_ => break,
			}
		}
	}

	fn parse_word_string(&mut self) -> String {
		std::assert!(self.peek().map_or(false, |ch| ch.is_ascii_alphabetic()));
		let mut word_string = String::new();
		while let Some(ch) = self.peek() {
			if !ch.is_ascii_alphabetic() {
				break;
			}
			word_string.push(ch);
			self.disc();
		}
		word_string
	}

	fn parse_string_litteral_string(&mut self) -> String {
		std::assert!(self.peek().map_or(false, |ch| ch == '\"'));
		self.disc(); // Initial `"` character
		let mut string_litteral_string = String::new();
		while let Some(ch) = self.peek() {
			if ch == '\"' {
				self.disc();
				break;
			}
			string_litteral_string.push(ch);
			self.disc();
		}
		string_litteral_string
	}

	fn parse_tok(&mut self) -> Result<Tok, ParsingError> {
		self.disc_ws();
		match self.peek() {
			Some(ch) if ch.is_ascii_alphabetic() =>
				Ok(Tok::Word(self.parse_word_string())),
			Some(ch) if ch == '\"' =>
				Ok(Tok::StringLitteral(self.parse_string_litteral_string())),
			Some(ch) if ch == '<' => {
				self.disc();
				Ok(Tok::OpToLeft)
			},
			None => Ok(Tok::Void),
			Some(ch) => {
				self.disc();
				Err(ParsingError::UnexpectedCharacter(ch))
			},
		}
	}

	fn parse_stmt(&mut self) -> Result<Stmt, ParsingError> {
		match self.parse_tok()? {
			Tok::Word(word) if word == "prjej" =>
				Ok(Stmt::PrintJej),
			Tok::Word(word) if word == "nop" =>
				Ok(Stmt::Nop),
			Tok::Word(left_word) if left_word == "pr" =>
				match self.parse_tok()? {
					Tok::Word(right_word) => Ok(Stmt::PrintVar(right_word)),
					tok => Err(ParsingError::UnexpectedToken(tok)),
				},
			Tok::Word(left_word) => match self.parse_tok()? {
				Tok::OpToLeft => match self.parse_tok()? {
					Tok::StringLitteral(right_string) =>
						Ok(Stmt::Assign(left_word, right_string)),
					tok => Err(ParsingError::UnexpectedToken(tok)),
				},
				tok => Err(ParsingError::UnexpectedToken(tok)),
			},
			tok => Err(ParsingError::UnexpectedToken(tok)),
		}
	}

	fn parse_prog(&mut self) -> Result<Prog, ParsingError> {
		let mut stmts: Vec<Stmt> = Vec::new();
		while {self.disc_ws(); self.peek().is_some()} {
			stmts.push(self.parse_stmt()?);
		}
		Ok(Prog {
			stmts: stmts,
		})
	}
}

use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Obj {
	String(String),
}

impl std::fmt::Display for Obj {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Obj::String(string) => write!(f, "\"{}\"", string),
		}
	}
}

struct Cx {
	i: usize,
	vars: HashMap<String, Obj>,
}

struct Mem {
	cxs: Vec<Cx>,
}

impl Mem {
	fn cx(&self) -> &Cx {
		self.cxs.last().expect("was jesus an alien")
	}

	fn cx_mut(&mut self) -> &mut Cx {
		self.cxs.last_mut().expect("was jesus an alien")
	}
}

impl Mem {
	fn exec_prog(&mut self, prog: &Prog) {
		while self.cx().i < prog.stmts.len() {
			self.exec_stmt(&prog.stmts[self.cx().i]);
			self.cx_mut().i += 1;
		}
	}

	fn exec_stmt(&mut self, stmt: &Stmt) {
		match stmt {
			Stmt::Nop => (),
			Stmt::Assign(varname, value) => {
				self.cx_mut().vars.insert(varname.to_string(),
					Obj::String(value.to_string()));
				()
			},
			Stmt::PrintVar(varname) =>
				println!("{}",
					self.cx_mut().vars.get(varname)
						.map_or("empty".to_string(), |val| val.to_string())),
			Stmt::PrintJej => println!("jej"),
		}
	}

	fn exec_stmt_ex(&mut self, stmt: &StmtEx) {
		let mut stack: Vec<Obj> = Vec::new();
		for inst in &stmt.insts {
			match inst {
				Inst::Nop => (),
				Inst::PushConst(obj) => stack.push(obj.clone()),
				Inst::AssignVar => {
					std::assert!(stack.len() >= 2);
					let varname = stack.pop().unwrap().to_string();
					let value = stack.pop().unwrap();
					self.cx_mut().vars.insert(varname, value);
				},
				Inst::ReadVar => {
					std::assert!(stack.len() >= 1);
					let varname = stack.pop().unwrap().to_string();
					stack.push(self.cx_mut().vars.get(&varname)
						.map_or(Obj::String("none".to_string()),
							|obj| obj.clone())
					);
				},
				Inst::Print => {
					std::assert!(stack.len() >= 1);
					let string = stack.pop().unwrap().to_string();
					print!("{}", string);
				},
				Inst::PushCx => self.cxs.push(Cx {
					i: 0,
					vars: HashMap::new(),
				}),
			}
		}
	}
}

struct Settings {
	src_filename: String,
	debug_mode: bool,
}

impl Settings {
	fn from_args() -> Settings {
		let mut args = std::env::args();
		args.next();
		let src_filename = args.next().expect("no source file provided");
		let mut debug_mode = false;
		for arg in args {
			if arg == "-d" {
				debug_mode = true;
			}
		}
		Settings {
			src_filename: src_filename,
			debug_mode: debug_mode,
		}
	}
}

fn main() {
	let settings = Settings::from_args();

	let src = std::fs::read_to_string(&settings.src_filename)
		.expect(&format!("source file `{}` couldn't be read",
			&settings.src_filename));
	let prog = CodeStreamString::from(&src)
		.parse_prog()
		.expect("gura flat");

	if settings.debug_mode {
		println!("\x1b[32m{:?}\x1b[39m", prog);
	}

	let mut mem = Mem {
		cxs: vec![Cx {
			i: 0,
			vars: HashMap::new(),
		}],
	};
	mem.exec_prog(&prog);

	let mut mem = Mem {
		cxs: Vec::new(),
	};
	mem.exec_stmt_ex(&StmtEx {insts: vec![
		Inst::PushConst(Obj::String("owo".to_string())),
		Inst::Print,
	],});
	println!("");
}
