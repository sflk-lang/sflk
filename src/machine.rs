
pub type Prog = Block;

#[derive(Debug)]
pub struct Block {
	stmts: Vec<Stmt>,
}

impl Block {
	pub fn new(stmts: Vec<Stmt>) -> Block {
		Block {
			stmts,
		}
	}
}

#[derive(Debug)]
pub enum Stmt {
	Print {expr: Expr},
	Assign {varname: String, expr: Expr},
	AssignIfFree {varname: String, expr: Expr},
	Do {expr: Expr},
	Redo,
	End,
	If {cond_expr: Expr, stmt: Box<Stmt>},
}

#[derive(Debug)]
pub enum Expr {
	Var {varname: String},
	Const {val: Obj},
	BinOp {op: Op, left: Box<Expr>, right: Box<Expr>},
}

#[derive(Debug)]
pub enum Op {
	Plus,
	Minus,
	Star,
	Slash,
}

use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Obj {
	Integer(isize),
	String(String),
	Block(Rc<Block>),
}

impl std::fmt::Display for Obj {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Obj::Integer(integer) => write!(f, "{}", integer),
			Obj::String(string) => write!(f, "{}", string),
			Obj::Block(_) => write!(f, "{}", "block"),
		}
	}
}

impl Obj {
	fn op_plus(&self, other: &Obj) -> Obj {
		match (self, other) {
			(Obj::Integer(left), Obj::Integer(right)) =>
				Obj::Integer(left + right),
			(Obj::String(left), Obj::String(right)) =>
				Obj::String(left.to_owned() + right),
			(obj_left, obj_right) => panic!("plus not yet supported between {} and {}",
				obj_left, obj_right),
		}
	}

	fn op_minus(&self, other: &Obj) -> Obj {
		match (self, other) {
			(Obj::Integer(left), Obj::Integer(right)) =>
				Obj::Integer(left - right),
			(obj_left, obj_right) => panic!("minus not yet supported between {} and {}",
				obj_left, obj_right),
		}
	}

	fn op_star(&self, other: &Obj) -> Obj {
		match (self, other) {
			(Obj::Integer(left), Obj::Integer(right)) =>
				Obj::Integer(left * right),
			(Obj::String(left), Obj::Integer(right)) =>
				Obj::String(left.repeat(*right as usize)),
			(obj_left, obj_right) => panic!("plus not yet supported between {} and {}",
				obj_left, obj_right),
		}
	}

	fn op_slash(&self, other: &Obj) -> Obj {
		match (self, other) {
			(Obj::Integer(left), Obj::Integer(right)) =>
				Obj::Integer(left / right),
			(Obj::String(left), Obj::String(right)) =>
				Obj::Integer(left.matches(right).count() as isize),
			(obj_left, obj_right) => panic!("plus not yet supported between {} and {}",
				obj_left, obj_right),
		}
	}
}

impl Obj {
	fn as_cond(&self) -> bool {
		match self {
			Obj::Integer(integer) => *integer != 0,
			Obj::String(string) => string.len() != 0,
			Obj::Block(_) => true,
		}
	}
}

use std::collections::HashMap;

struct Cx {
	varmap: HashMap<String, Obj>,
}

impl Cx {
	fn new() -> Cx {
		Cx {
			varmap: HashMap::new(),
		}
	}
}

struct ExCx {
	cx: Cx,
	i: usize,
	running: bool,
}

impl ExCx {
	fn new() -> ExCx {
		ExCx {
			cx: Cx::new(),
			i: 0,
			running: true,
		}
	}
}

pub struct Mem {
	excx_stack: Vec<ExCx>,
}

impl Mem {
	pub fn new() -> Mem {
		Mem {
			excx_stack: Vec::new(),
		}
	}
}

impl Mem {
	fn top_excx(&self) -> &ExCx {
		self.excx_stack.last().unwrap()
	}

	fn top_excx_mut(&mut self) -> &mut ExCx {
		self.excx_stack.last_mut().unwrap()
	}
}

impl Mem {
	fn varset(&mut self, varname: &str, val: Obj) {
		self.top_excx_mut().cx.varmap.insert(varname.to_string(), val);
		()
	}

	fn varset_if_free(&mut self, varname: &str, val: Obj) {
		if self.top_excx().cx.varmap.get(varname).is_none() {
			self.top_excx_mut().cx.varmap.insert(varname.to_string(), val);
		}
	}

	fn varget(&self, varname: &str) -> &Obj {
		self.top_excx().cx.varmap.get(varname).expect("change this")
	}
}

impl Mem {
	pub fn exec_prog(&mut self, prog: &Prog) {
		self.exec_block(prog as &Block);
	}

	fn exec_block(&mut self, block: &Block) {
		self.excx_stack.push(ExCx::new());
		while self.top_excx().running {
			self.exec_stmt(&block.stmts[self.top_excx().i]);
			if self.top_excx().i >= block.stmts.len() {
				self.top_excx_mut().running = false;
			}
		}
		self.excx_stack.pop();
	}

	fn exec_stmt(&mut self, stmt: &Stmt) {
		match stmt {
			Stmt::Print {expr} => {
				println!("{}", self.eval_expr(expr));
				self.top_excx_mut().i += 1;
			},
			Stmt::Assign {varname, expr} => {
				let val = self.eval_expr(expr);
				self.varset(varname, val);
				self.top_excx_mut().i += 1;
			},
			Stmt::AssignIfFree {varname, expr} => {
				let val = self.eval_expr(expr);
				self.varset_if_free(varname, val);
				self.top_excx_mut().i += 1;
			},
			Stmt::Do {expr} => {
				match self.eval_expr(expr) {
					Obj::Block(block) => self.exec_block(&block),
					obj => panic!("can't do {} for now", obj),
				};
				self.top_excx_mut().i += 1;
			},
			Stmt::Redo => {
				self.top_excx_mut().i = 0;
			},
			Stmt::End => {
				self.top_excx_mut().running = false;
			},
			Stmt::If {cond_expr, stmt} => {
				let cond_val = self.eval_expr(cond_expr);
				if cond_val.as_cond() {
					self.exec_stmt(stmt);
				} else {
					self.top_excx_mut().i += 1;
				}
			},
		}
	}

	fn eval_expr(&mut self, expr: &Expr) -> Obj {
		match expr {
			Expr::Var {varname} => self.varget(varname).clone(),
			Expr::Const {val} => val.clone(),
			Expr::BinOp {op, left, right} => match op {
				Op::Plus => Obj::op_plus(&self.eval_expr(left), &self.eval_expr(right)),
				Op::Minus => Obj::op_minus(&self.eval_expr(left), &self.eval_expr(right)),
				Op::Star => Obj::op_star(&self.eval_expr(left), &self.eval_expr(right)),
				Op::Slash => Obj::op_slash(&self.eval_expr(left), &self.eval_expr(right)),
			}
		}
	}
}
