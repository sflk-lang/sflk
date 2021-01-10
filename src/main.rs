
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

#[derive(Debug, Clone)]
enum Tok {
	Word(String),
	StringLitteral(String),
	OpToLeft,
	OpPlus,
	Void,
}

impl std::fmt::Display for Tok {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Tok::Word(word) => write!(f, "{}", word),
			Tok::StringLitteral(string) => write!(f, "\"{}\"", string),
			Tok::OpToLeft => write!(f, "<"),
			Tok::OpPlus => write!(f, "+"),
			Tok::Void => write!(f, ""),
		}
	}
}

impl Tok {
	fn is_void(&self) -> bool {
		match self {
			Tok::Void => true,
			_ => false,
		}
	}

	fn is_bin_op(&self) -> bool {
		match self {
			Tok::OpPlus => true,
			_ => false,
		}
	}
}

#[derive(Debug)]
enum Expr {
	Const(Obj),
	Var(String),
	Plus(Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
enum Stmt {
	Nop,
	Assign(String, Expr),
	Print(Expr),
}

#[derive(Debug)]
struct Prog {
	stmts: Vec<Stmt>,
}

struct TokStream<Cs: CodeStream> {
	cs: Cs,
	tok_opt: Option<Tok>,
}

impl<Cs: CodeStream> TokStream<Cs> {
	fn from(cs: Cs) -> TokStream<Cs> {
		TokStream {
			cs: cs,
			tok_opt: None,
		}
	}
}

impl<Cs: CodeStream> TokStream<Cs> {
	fn disc_ws(&mut self) {
		loop {
			match self.cs.peek() {
				Some(ch) if ch.is_ascii_whitespace() => self.cs.disc(),
				_ => break,
			}
		}
	}

	fn parse_word_string(&mut self) -> String {
		std::assert!(self.cs.peek().map_or(false, |ch| ch.is_ascii_alphabetic()));
		let mut word_string = String::new();
		while let Some(ch) = self.cs.peek() {
			if !ch.is_ascii_alphabetic() {
				break;
			}
			word_string.push(ch);
			self.cs.disc();
		}
		word_string
	}

	fn parse_string_litteral_string(&mut self) -> String {
		std::assert!(self.cs.peek().map_or(false, |ch| ch == '\"'));
		self.cs.disc(); // Initial `"` character
		let mut string_litteral_string = String::new();
		while let Some(ch) = self.cs.peek() {
			if ch == '\"' {
				self.cs.disc();
				break;
			}
			string_litteral_string.push(ch);
			self.cs.disc();
		}
		string_litteral_string
	}

	fn parse_tok(&mut self) -> Result<Tok, ParsingError> {
		self.disc_ws();
		match self.cs.peek() {
			Some(ch) if ch.is_ascii_alphabetic() =>
				Ok(Tok::Word(self.parse_word_string())),
			Some(ch) if ch == '\"' =>
				Ok(Tok::StringLitteral(self.parse_string_litteral_string())),
			Some(ch) if ch == '<' => {
				self.cs.disc();
				Ok(Tok::OpToLeft)
			},
			Some(ch) if ch == '+' => {
				self.cs.disc();
				Ok(Tok::OpPlus)
			},
			None => Ok(Tok::Void),
			Some(ch) => {
				self.cs.disc();
				Err(ParsingError::UnexpectedCharacter(ch))
			},
		}
	}
}

impl<Cs: CodeStream> TokStream<Cs> {
	fn peek(&mut self) -> Result<&Tok, ParsingError> {
		if self.tok_opt.is_none() {
			self.tok_opt = Some(self.parse_tok()?);
		}
		if let Some(tok) = &self.tok_opt {
			Ok(tok)
		} else {
			unreachable!()
		}
	}

	fn disc(&mut self) -> Result<(), ParsingError> {
		if self.tok_opt.is_none() {
			self.parse_tok()?;
		} else {
			self.tok_opt = None;
		}
		Ok(())
	}

	fn pop(&mut self) -> Result<Tok, ParsingError> {
		match &self.tok_opt {
			Some(tok) => {
				let tok_copy = tok.clone();
				self.tok_opt = None;
				Ok(tok_copy)
			},
			None => self.parse_tok(),
		}
	}
}

impl<Cs: CodeStream> TokStream<Cs> {
	fn parse_expr_left(&mut self) -> Result<Expr, ParsingError> {
		match self.pop()? {
			Tok::Word(word) =>
				Ok(Expr::Var(word)),
			Tok::StringLitteral(string) =>
				Ok(Expr::Const(Obj::String(string))),
			tok =>
				Err(ParsingError::UnexpectedToken(tok)),
		}
	}

	fn parse_expr(&mut self) -> Result<Expr, ParsingError> {
		let mut expr = self.parse_expr_left()?;
		while self.peek()?.is_bin_op() {
			let op = self.peek()?.to_owned();
			self.disc()?;
			match op {
				Tok::OpPlus =>
					expr = Expr::Plus(Box::new(expr),
						Box::new(self.parse_expr_left()?)),
				_ => unreachable!(),
			}
		}

		Ok(expr)
	}

	fn parse_stmt(&mut self) -> Result<Stmt, ParsingError> {
		match self.pop()? {
			Tok::Word(word) if word == "nop" =>
				Ok(Stmt::Nop),
			Tok::Word(word) if word == "pr" =>
				Ok(Stmt::Print(self.parse_expr()?)),
			Tok::Word(word) => match self.pop()? {
				Tok::OpToLeft =>
					Ok(Stmt::Assign(word, self.parse_expr()?)),
				tok => Err(ParsingError::UnexpectedToken(tok)),
			},
			tok => Err(ParsingError::UnexpectedToken(tok)),
		}
	}

	fn parse_prog(&mut self) -> Result<Prog, ParsingError> {
		let mut stmts: Vec<Stmt> = Vec::new();
		while !self.peek()?.is_void() {
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

impl Obj {
	fn op_plus(left: &Obj, right: &Obj) -> Obj {
		match (left, right) {
			(Obj::String(string_left), Obj::String(string_right)) =>
				Obj::String(string_left.to_owned() + string_right),
			_ => panic!("o shit"),
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
	fn new() -> Mem {
		Mem {
			cxs: Vec::new(),
		}
	}
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
		self.cxs.push(Cx {
			i: 0,
			vars: HashMap::new(),
		});
		while self.cx().i < prog.stmts.len() {
			self.exec_stmt(&prog.stmts[self.cx().i]);
			self.cx_mut().i += 1;
		}
	}

	fn exec_stmt(&mut self, stmt: &Stmt) {
		match stmt {
			Stmt::Nop => (),
			Stmt::Assign(varname, expr) => {
				let value = self.eval_expr(expr);
				self.cx_mut().vars.insert(varname.to_string(), value);
				()
			},
			Stmt::Print(expr) =>
				println!("{}", self.eval_expr(expr).to_string()),
		}
	}

	fn eval_expr(&mut self, expr: &Expr) -> Obj {
		match expr {
			Expr::Const(obj) => obj.clone(),
			Expr::Var(varname) =>
				self.cx_mut().vars.get(varname)
					.unwrap_or(&Obj::String("none".to_string()))
					.clone(),
			Expr::Plus(left, right) =>
				Obj::op_plus(&self.eval_expr(left), &self.eval_expr(right)),
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
	let prog = TokStream::from(CodeStreamString::from(&src))
		.parse_prog()
		.expect("gura flat");

	if settings.debug_mode {
		println!("\x1b[32m{:?}\x1b[39m", prog);
	}

	let mut mem = Mem::new();
	mem.exec_prog(&prog);
}
