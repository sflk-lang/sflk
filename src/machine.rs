
pub type Prog = Block;

#[derive(Debug)]
pub struct Block {
	pub stmts: Vec<Stmt>,
}

impl Block {
	pub fn new(stmts: Vec<Stmt>) -> Block {
		Block {
			stmts,
		}
	}
}

#[derive(Debug, Clone)]
pub enum Stmt {
	Print {expr: Expr},
	PrintNewline,
	Assign {varname: String, expr: Expr},
	AssignIfFree {varname: String, expr: Expr},
	Do {expr: Expr},
	Imp {expr: Expr},
	Exp {expr: Expr},
	Redo {expr: Expr},
	End {expr: Expr},
	If {cond_expr: Expr, stmt: Box<Stmt>},
}

#[derive(Debug, Clone)]
pub enum Expr {
	Var {varname: String},
	Const {val: Obj},
	BinOp {op: Op, left: Box<Expr>, right: Box<Expr>},
}

#[derive(Debug, Clone)]
pub enum Op {
	Plus,
	Minus,
	Star,
	Slash,
}

impl std::fmt::Display for Op {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Op::Plus => write!(f, "plus"),
			Op::Minus => write!(f, "minus"),
			Op::Star => write!(f, "star"),
			Op::Slash => write!(f, "slash"),
		}
	}
}

use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Obj {
	Integer(isize),
	String(String),
	Block(Rc<Block>),
	//Cx(Cx),
}

impl std::fmt::Display for Obj {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Obj::Integer(integer) => write!(f, "{}", integer),
			Obj::String(string) => write!(f, "{}", string),
			Obj::Block(_) => write!(f, "{}", "block"), // change this
			//Obj::Cx(cx) => write!(f, "{:?}", cx), // change this
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
			(Obj::Block(left), Obj::Block(right)) => 
				Obj::Block(Rc::new(Block::new(
					left.stmts.iter().chain(right.stmts.iter()).cloned().collect()))),
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
			//Obj::Cx(cx) => !cx.varmap.is_empty(),
		}
	}
}

use std::collections::HashMap;

#[derive(Debug, Clone)]
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

impl Cx {
	fn import(&mut self, other: Cx) {
		self.varmap.extend(other.varmap);
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
	fn excx(&self, i_from_top: usize) -> &ExCx {
		self.excx_stack.get(self.excx_stack.len() - 1 - i_from_top).unwrap()
	}

	fn excx_mut(&mut self, i_from_top: usize) -> &mut ExCx {
		let i_max = self.excx_stack.len() - 1;
		self.excx_stack.get_mut(i_max - i_from_top).unwrap()
	}
}

impl Mem {
	fn varset(&mut self, varname: &str, val: Obj) {
		self.excx_mut(0).cx.varmap.insert(varname.to_string(), val);
		()
	}

	fn varset_if_free(&mut self, varname: &str, val: Obj) {
		if self.excx(0).cx.varmap.get(varname).is_none() {
			self.excx_mut(0).cx.varmap.insert(varname.to_string(), val);
		}
	}

	fn varget(&self, varname: &str) -> &Obj {
		self.excx(0).cx.varmap.get(varname).expect("change this")
	}
}

impl Mem {
	pub fn exec_prog(&mut self, prog: &Prog) {
		self.exec_block(prog as &Block);
	}

	fn exec_block(&mut self, block: &Block) {
		self.excx_stack.push(ExCx::new());
		loop {
			if self.excx(0).i >= block.stmts.len() {
				self.excx_mut(0).running = false;
			}
			if !self.excx_mut(0).running {
				break;
			}
			self.exec_stmt(&block.stmts[self.excx(0).i]);
		}
		self.excx_stack.pop();
	}

	fn exec_stmt(&mut self, stmt: &Stmt) {
		match stmt {
			Stmt::Print {expr} => {
				print!("{}", self.eval_expr(expr));
				self.excx_mut(0).i += 1;
			},
			Stmt::PrintNewline => {
				println!("");
				self.excx_mut(0).i += 1;
			},
			Stmt::Assign {varname, expr} => {
				let val = self.eval_expr(expr);
				self.varset(varname, val);
				self.excx_mut(0).i += 1;
			},
			Stmt::AssignIfFree {varname, expr} => {
				let val = self.eval_expr(expr);
				self.varset_if_free(varname, val);
				self.excx_mut(0).i += 1;
			},
			Stmt::Do {expr} => {
				match self.eval_expr(expr) {
					Obj::Block(block) => self.exec_block(&block),
					obj => panic!("can't do {} for now", obj),
				};
				self.excx_mut(0).i += 1;
			},
			Stmt::Imp {expr} => {
				let val = self.eval_expr(expr);
				if let Obj::Integer(integer) = val {
					let cx_to_import = self.excx(integer as usize).cx.clone();
					self.excx_mut(0).cx.import(cx_to_import);
					self.excx_mut(0).i += 1;
				} else {
					panic!("imp expected integer but found {}", val)
				}
			},
			Stmt::Exp {expr} => {
				let val = self.eval_expr(expr);
				if let Obj::Integer(integer) = val {
					let cx_to_export = self.excx(0).cx.clone();
					self.excx_mut(integer as usize).cx.import(cx_to_export);
					self.excx_mut(0).i += 1;
				} else {
					panic!("exp expected integer but found {}", val)
				}
			},
			Stmt::Redo {expr} => {
				let val = self.eval_expr(expr);
				if let Obj::Integer(integer) = val {
					self.excx_mut(integer as usize).i = 0;
					if integer != 0 {
						self.excx_mut(0).i += 1;
					}
				} else {
					panic!("redo expected integer but found {}", val)
				}
			},
			Stmt::End {expr} => {
				let val = self.eval_expr(expr);
				if let Obj::Integer(integer) = val {
					self.excx_mut(integer as usize).running = false;
					if integer != 0 {
						self.excx_mut(0).i += 1;
					}
				} else {
					panic!("redo expected integer but found {}", val)
				}
			},
			Stmt::If {cond_expr, stmt} => {
				let cond_val = self.eval_expr(cond_expr);
				if cond_val.as_cond() {
					self.exec_stmt(stmt);
				} else {
					self.excx_mut(0).i += 1;
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
