
use std::collections::HashMap;
use crate::program::{Prog, Block, Stmt, Expr, ChOp, Op};
use crate::object::Obj;


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
	flow: Flow,
}

impl ExCx {
	fn new() -> ExCx {
		ExCx {
			cx: Cx::new(),
			i: 0,
			flow: Flow::Next,
		}
	}
}


#[derive(PartialEq, Eq)]
enum Flow {
	Next,
	Restart,
	End,
}


pub struct Mem {
	excx_stack: Vec<ExCx>,
	debug_mode: bool,
}

impl Mem {
	pub fn new() -> Mem {
		Mem {
			excx_stack: Vec::new(),
			debug_mode: false,
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

	fn exec_block_here(&mut self, block: &Block) {
		loop {
			if self.excx(0).i >= block.stmts.len() {
				self.excx_mut(0).flow = Flow::End;
			}
			if self.excx_mut(0).flow == Flow::End {
				break;
			}
			self.exec_stmt(&block.stmts[self.excx(0).i]);
			match self.excx_mut(0).flow {
				Flow::Next => self.excx_mut(0).i += 1,
				Flow::Restart => self.excx_mut(0).i = 0,
				Flow::End => (),
			}
		}
	}

	fn exec_block_excx(&mut self, block: &Block, excx: ExCx) -> ExCx {
		self.excx_stack.push(excx);
		self.exec_block_here(block);
		self.excx_stack.pop().unwrap()
	}

	fn exec_block(&mut self, block: &Block) {
		self.exec_block_excx(block, ExCx::new());
	}

	fn exec_stmt(&mut self, stmt: &Stmt) {
		match stmt {
			Stmt::Nop => (),
			Stmt::Print {expr} => 
				print!("{}", self.eval_expr(expr)),
			Stmt::PrintNewline => 
				println!(""),
			Stmt::Assign {varname, expr} => {
				let val = self.eval_expr(expr);
				self.varset(varname, val);
			},
			Stmt::AssignIfFree {varname, expr} => {
				let val = self.eval_expr(expr);
				self.varset_if_free(varname, val);
			},
			Stmt::Do {expr} =>
				match self.eval_expr(expr) {
					Obj::Block(block) => self.exec_block(&block),
					obj => panic!("can't do {} for now", obj),
				},
			Stmt::DoHere {expr} =>
				match self.eval_expr(expr) {
					Obj::Block(block) => self.exec_block_here(&block),
					obj => panic!("can't do {} for now", obj),
				},
			Stmt::Ev {expr} => {
				self.eval_expr(expr);
			}
			Stmt::Imp {expr} =>
				match self.eval_expr(expr) {
					Obj::Integer(integer) => {
						let cx_to_import = self.excx(integer as usize).cx.clone();
						self.excx_mut(0).cx.import(cx_to_import);
					},
					invalid_obj => panic!("imp expected integer but found {}", invalid_obj),
				},
			Stmt::Exp {expr} =>
				match self.eval_expr(expr) {
					Obj::Integer(integer) => {
						let cx_to_export = self.excx(0).cx.clone();
						self.excx_mut(integer as usize).cx.import(cx_to_export);
					},
					invalid_obj => panic!("exp expected integer but found {}", invalid_obj),
				},
			Stmt::Redo {expr} =>
				match self.eval_expr(expr) {
					Obj::Integer(integer) => {
						self.excx_mut(integer as usize).flow = Flow::Restart;
					},
					invalid_obj => panic!("redo expected integer but found {}", invalid_obj),
				},
			Stmt::End {expr} =>
				match self.eval_expr(expr) {
					Obj::Integer(integer) => {
						self.excx_mut(integer as usize).flow = Flow::End;
					},
					invalid_obj => panic!("end expected integer but found {}", invalid_obj),
				},
			Stmt::If {cond_expr, stmt} =>
				if self.eval_expr(cond_expr).as_cond() {
					self.exec_stmt(stmt)
				},
			Stmt::Group {stmts} =>
				for stmt in stmts {
					self.exec_stmt(stmt);
					if self.excx(0).flow != Flow::Next {
						break;
					}
				},
		}
	}

	fn eval_expr(&mut self, expr: &Expr) -> Obj {
		match expr {
			Expr::Var {varname} => self.varget(varname).clone(),
			Expr::Const {val} => val.clone(),
			Expr::Chain {init_expr, chops} => {
				let mut val = self.eval_expr(init_expr);
				for chop in chops {
					self.apply_chop(&mut val, chop)
				}
				val
			},
		}
	}

	fn apply_chop(&mut self, val: &mut Obj, chop: &ChOp) {
		let right = self.eval_expr(&chop.expr);
		match chop.op {
			Op::Plus => val.plus(right),
			Op::Minus => val.minus(right),
			Op::Star => val.star(right),
			Op::Slash => val.slash(right),
			Op::ToRight => {
				match right {
					Obj::Block(block) => {
						let mut excx = ExCx::new();
						excx.cx.varmap.insert("v".to_string(), val.clone());
						excx = self.exec_block_excx(&block, excx);
						if let Some(v_value) = excx.cx.varmap.get("v") {
							*val = v_value.to_owned();
						}
					},
					invalid_obj => panic!("can't do {} for now", invalid_obj),
				}
			},
		}
	}
}
