/*
use std::rc::Rc;

#[derive(Debug)]
struct CharLoc {
	source_name: Option<Rc<String>>,
	line: usize,
	column: usize,
}

impl CharLoc {
	fn new(source_name: Option<Rc<String>>) -> CharLoc {
		CharLoc {
			source_name: source_name,
			line: 1,
			column: 1,
		}
	}

	fn pass(&mut self, ch: char) {
		if ch == '\n' {
			self.line += 1;
			self.column = 1;
		} else {
			self.column += 1;
		}
	}

	fn no_source(&self) -> CharLoc {
		CharLoc {
			source_name: None,
			line: self.line,
			column: self.column,
		}
	}
}

impl std::fmt::Display for CharLoc {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self.source_name {
			opt if opt.map_or(true, |_| f.alternate()) =>
				write!(f, "line {} col {}",
					self.line, self.column),
			Some(source_name) =>
				write!(f, "{}: line {} col {}",
					source_name, self.line, self.column),
		}
	}
}

#[derive(Debug)]
struct StringLoc {
	beg: CharLoc,
	end: CharLoc, // excluded
}

impl StringLoc {
	fn new(beg: CharLoc, end: CharLoc) -> StringLoc {
		StringLoc {
			beg: beg,
			end: end,
		}
	}
}

impl std::fmt::Display for StringLoc {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self.beg.source_name {
			opt if opt.map_or(true, |_| f.alternate()) =>
				write!(f, "from {:#} to (excl) {:#}",
					self.beg, self.end),
			Some(source_name) =>
				write!(f, "{}: from {:#} to (excl) {:#}",
					source_name, self.beg, self.end),
		}
	}
}

trait CharStream {
	fn peek(&self) -> Option<char>;
	fn disc(&mut self);
	fn loc(&self) -> CharLoc;
}

struct CharStreamString {
	string: String,
	index: usize,
	loc: CharLoc,
}

impl CharStreamString {
	fn new(string: &str) -> Self {
		CharStreamString {
			string: string.to_string(),
			index: 0,
			loc: CharLoc::new(Some(Rc::new("source_code".to_string()))),
		}
	}
}

impl CharStream for CharStreamString {
	fn peek(&self) -> Option<char> {
		self.string[self.index..].chars().next()
	}

	fn disc(&mut self) {
		match self.peek() {
			Some(ch) => {
				self.index += ch.len_utf8();
				self.loc.pass(ch);
			},
			None => (),
		}
	}

	fn loc(&self) -> CharLoc {
		self.loc
	}
}

#[derive(Debug)]
enum ParsingError {
	UnexpectedCharacter(char, CharLoc),
	UnexpectedToken(Tok, StringLoc),
	UnexpectedEndOfFileInComment,
}

impl std::fmt::Display for ParsingError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			ParsingError::UnexpectedCharacter(ch, loc) =>
				write!(f, "unexpected character `{}` at {}",
					ch, loc),
			ParsingError::UnexpectedToken(tok, loc) =>
				write!(f, "unexpected token `{}` beginning at {}",
					tok, loc.beg),
			ParsingError::UnexpectedEndOfFileInComment =>
				write!(f, "unexpected end-of-file in comment"),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Tok {
	Word(String),
	StringLitteral(String),
	IntegerLitteral(String),
	OpLeft,
	OpPlus,
	OpMinus,
	ParenLeft,
	ParenRight,
	Void,
}

impl std::fmt::Display for Tok {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Tok::Word(word) => write!(f, "{}", word),
			Tok::StringLitteral(string) => write!(f, "\"{}\"", string),
			Tok::IntegerLitteral(integer) => write!(f, "{}", integer),
			Tok::OpLeft => write!(f, "<"),
			Tok::OpPlus => write!(f, "+"),
			Tok::OpMinus => write!(f, "-"),
			Tok::ParenLeft => write!(f, "("),
			Tok::ParenRight => write!(f, ")"),
			Tok::Void => write!(f, ""),
		}
	}
}

impl Tok {
	fn is_bin_op(&self) -> bool {
		match self {
			Tok::OpPlus => true,
			Tok::OpMinus => true,
			_ => false,
		}
	}
}

#[derive(Debug)]
enum Expr {
	Const(Obj),
	Var(String),
	Plus(Box<Expr>, Box<Expr>),
	Minus(Box<Expr>, Box<Expr>),
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

struct TokStream {
	cs: Box<dyn CharStream>,
	tok_loc_stack: Vec<(Tok, StringLoc)>,
}

impl TokStream {
	fn from(cs: Box<dyn CharStream>) -> TokStream {
		TokStream {
			cs: cs,
			tok_loc_stack: Vec::new(),
		}
	}
}

impl TokStream {
	fn disc_ws(&mut self) -> Result<(), ParsingError> {
		let mut comment_mode = false;
		loop {
			match self.cs.peek() {
				Some('#') if !comment_mode =>
					comment_mode = true,
				Some(ch) if !(ch.is_ascii_whitespace() || comment_mode) =>
					break,
				Some('#') if comment_mode =>
					comment_mode = false,
				None if comment_mode =>
					return Err(ParsingError::UnexpectedEndOfFileInComment),
				None if !comment_mode =>
					break,
				_ => (),
			}
			self.cs.disc();
		}
		Ok(())
	}

	fn parse_word_string(&mut self) -> String {
		std::assert!(self.cs.peek()
			.map_or(false, |ch| ch.is_ascii_alphabetic()));
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

	fn parse_integer_string(&mut self) -> String {
		std::assert!(self.cs.peek()
			.map_or(false, |ch| ch.is_ascii_digit()));
		let mut integer_string = String::new();
		while let Some(ch) = self.cs.peek() {
			if !ch.is_ascii_digit() {
				break;
			}
			integer_string.push(ch);
			self.cs.disc();
		}
		integer_string
	}

	fn parse_string_string(&mut self) -> String {
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

	fn parse_tok(&mut self) -> Result<(Tok, StringLoc), ParsingError> {
		self.disc_ws()?;
		let beg = self.cs.loc();
		match self.cs.peek() {
			Some(ch) if ch.is_ascii_alphabetic() => {
				let tok = Tok::Word(self.parse_word_string());
				let end = self.cs.loc();
				Ok((tok, StringLoc::new(beg, end)))
			},
			Some(ch) if ch.is_ascii_digit() => {
				let tok = Tok::IntegerLitteral(self.parse_integer_string());
				let end = self.cs.loc();
				Ok((tok, StringLoc::new(beg, end)))
			},
			Some(ch) if ch == '\"' => {
				let tok = Tok::StringLitteral(self.parse_string_string());
				let end = self.cs.loc();
				Ok((tok, StringLoc::new(beg, end)))
			},
			Some(ch) if ch == '<' => {
				self.cs.disc();
				let tok = Tok::OpLeft;
				let end = self.cs.loc();
				Ok((tok, StringLoc::new(beg, end)))
			},
			Some(ch) if ch == '+' => {
				self.cs.disc();
				let tok = Tok::OpPlus;
				let end = self.cs.loc();
				Ok((tok, StringLoc::new(beg, end)))
			},
			Some(ch) if ch == '-' => {
				self.cs.disc();
				let tok = Tok::OpMinus;
				let end = self.cs.loc();
				Ok((tok, StringLoc::new(beg, end)))
			},
			Some(ch) if ch == '(' => {
				self.cs.disc();
				let tok = Tok::ParenLeft;
				let end = self.cs.loc();
				Ok((tok, StringLoc::new(beg, end)))
			},
			Some(ch) if ch == ')' => {
				self.cs.disc();
				let tok = Tok::ParenRight;
				let end = self.cs.loc();
				Ok((tok, StringLoc::new(beg, end)))
			},
			None => {
				let tok = Tok::Void;
				let end = self.cs.loc();
				Ok((tok, StringLoc::new(beg, end)))
			},
			Some(ch) => {
				self.cs.disc();
				Err(ParsingError::UnexpectedCharacter(ch, beg))
			},
		}
	}
}

impl TokStream {
	fn peek(&mut self) -> Result<&(Tok, StringLoc), ParsingError> {
		if self.tok_loc_stack.is_empty() {
			let tok_loc = self.parse_tok()?;
			self.tok_loc_stack.push(tok_loc);
		}
		Ok(self.tok_loc_stack.last().unwrap())
	}

	fn disc(&mut self) -> Result<(), ParsingError> {
		match self.tok_loc_stack.pop() {
			None => {
				self.parse_tok()?;
				()
			},
			Some(_) => (),
		}
		Ok(())
	}

	fn pop(&mut self) -> Result<(Tok, StringLoc), ParsingError> {
		match self.tok_loc_stack.pop() {
			Some(tok_loc) => Ok(tok_loc),
			None => self.parse_tok(),
		}
	}
}

enum ExprEnd {
	None,
	Paren,
}

impl TokStream {
	fn parse_expr_left(&mut self) -> Result<(Expr, StringLoc), ParsingError> {
		match self.pop()? {
			(Tok::Word(word), loc) =>
				Ok((Expr::Var(word), loc)),
			(Tok::StringLitteral(string), loc) =>
				Ok((Expr::Const(Obj::String(string)), loc)),
			(Tok::IntegerLitteral(string), loc) =>
				Ok((Expr::Const(Obj::Integer(str::parse(&string)
					.expect("TODO: implement bigints"))), loc)),
			(Tok::ParenLeft, loc) => 
				self.parse_expr(&ExprEnd::Paren),
			(tok, loc) =>
				Err(ParsingError::UnexpectedToken(tok, loc)),
		}
	}

	fn parse_expr(&mut self, end: &ExprEnd) -> Result<
		(Expr, StringLoc), ParsingError
	> {
		let mut expr = self.parse_expr_left()?;
		while self.peek()?.0.is_bin_op() {
			let op = self.peek()?.to_owned();
			self.disc()?;
			match op {
				Tok::OpPlus =>
					expr = Expr::Plus(Box::new(expr),
						Box::new(self.parse_expr_left()?)),
				Tok::OpMinus =>
					expr = Expr::Minus(Box::new(expr),
						Box::new(self.parse_expr_left()?)),
				_ => unreachable!(),
			}
		}
		match end {
			ExprEnd::None => (),
			ExprEnd::Paren => match self.pop()? {
				Tok::ParenRight => (),
				tok => return Err(ParsingError::UnexpectedToken(tok)),
			},
		}
		Ok(expr)
	}

	fn parse_stmt(&mut self) -> Result<Stmt, ParsingError> {
		match self.pop()? {
			Tok::Word(word) if word == "nop" =>
				Ok(Stmt::Nop),
			Tok::Word(word) if word == "pr" =>
				Ok(Stmt::Print(self.parse_expr(&ExprEnd::None)?)),
			Tok::Word(word) => match self.pop()? {
				Tok::OpLeft =>
					Ok(Stmt::Assign(word, self.parse_expr(&ExprEnd::None)?)),
				tok => Err(ParsingError::UnexpectedToken(tok)),
			},
			tok => Err(ParsingError::UnexpectedToken(tok)),
		}
	}

	fn parse_prog(&mut self) -> Result<Prog, ParsingError> {
		let mut stmts: Vec<Stmt> = Vec::new();
		while *self.peek()? != Tok::Void {
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
	Integer(isize),
}

impl std::fmt::Display for Obj {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Obj::String(string) => write!(f, "{}", string),
			Obj::Integer(integer) => write!(f, "{}", integer),
		}
	}
}

impl Obj {
	fn op_plus(left: &Obj, right: &Obj) -> Obj {
		match (left, right) {
			(Obj::String(string_left), Obj::String(string_right)) =>
				Obj::String(string_left.to_owned() + string_right),
			(Obj::Integer(integer_left), Obj::Integer(integer_right)) =>
				Obj::Integer(integer_left + integer_right),
			_ => panic!("o shit"),
		}
	}

	fn op_minus(left: &Obj, right: &Obj) -> Obj {
		match (left, right) {
			(Obj::Integer(integer_left), Obj::Integer(integer_right)) =>
				Obj::Integer(integer_left - integer_right),
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
			Expr::Minus(left, right) =>
				Obj::op_minus(&self.eval_expr(left), &self.eval_expr(right)),
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

mod parser;

fn main() {
	let settings = Settings::from_args();

	let src = std::fs::read_to_string(&settings.src_filename)
		.expect(&format!("source file `{}` couldn't be read",
			&settings.src_filename));
	let prog = TokStream::from(Box::new(CharStreamString::new(&src)))
		.parse_prog()
		.expect("gura flat");

	if settings.debug_mode {
		println!("\x1b[32m{:?}\x1b[39m", prog);
	}

	let mut mem = Mem::new();
	mem.exec_prog(&prog);

	parser::uwu();
}
*/


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
			} else {
				panic!("unknown command line argument `{}`", arg);
			}
		}
		Settings {
			src_filename,
			debug_mode,
		}
	}
}

use std::rc::Rc;
mod parser;

fn main() -> Result<(), parser::ParsingError> {
	let settings = Settings::from_args();

	let scu = Rc::new(parser::SourceCodeUnit::from_filename(
		&settings.src_filename));
	if settings.debug_mode {
		dbg!(&scu);
	}

	let mut rh = parser::ReadingHead::from_scu(Rc::clone(&scu));
	loop {
		let (tok, _) = rh.read_cur_tok()?;
		if tok.is_void() {
			break;
		}
		dbg!(tok);
	}

	Ok(())
}
