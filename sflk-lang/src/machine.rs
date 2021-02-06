
use std::collections::HashMap;
use crate::program::{Prog, Block, Stmt, Expr, ChOp, Op};
use crate::object::Obj;
use crate::stringrtlog::{StringRtlog, Style, style};


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
	i: Vec<usize>,
	flow: Flow,
}

impl ExCx {
	fn new() -> ExCx {
		ExCx {
			cx: Cx::new(),
			i: Vec::new(),
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
	pub debug_mode: Option<StringRtlog>,
}

impl Mem {
	pub fn new(is_debug_mode: bool) -> Mem {
		Mem {
			excx_stack: Vec::new(),
			debug_mode: if is_debug_mode {Some(StringRtlog::new())} else {None},
		}
	}
}

impl Mem {
	fn push_excx(&mut self, excx: ExCx) {
		self.excx_stack.push(excx);
	}

	fn pop_excx(&mut self) -> ExCx {
		self.excx_stack.pop().unwrap()
	}

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

	fn varset_if_free(&mut self, varname: &str, val: Obj) -> bool {
		if self.excx(0).cx.varmap.get(varname).is_none() {
			self.excx_mut(0).cx.varmap.insert(varname.to_string(), val);
			true
		} else {
			false
		}
	}

	fn varget(&self, varname: &str) -> &Obj {
		self.excx(0).cx.varmap.get(varname)
			.expect(format!("get variable {} but it doesn't exist", varname).as_str())
	}
}

impl Mem {
	fn rtlog_indent(&mut self, string: String, is_context: bool, style: Style) {
		if let Some(string_rtlog) = &mut self.debug_mode {
			string_rtlog.indent(string, is_context, style);
		}
	}

	fn rtlog_deindent(&mut self) {
		if let Some(string_rtlog) = &mut self.debug_mode {
			string_rtlog.deindent();
		}
	}

	fn rtlog_log(&mut self, string: String, style: Style) {
		if let Some(string_rtlog) = &mut self.debug_mode {
			string_rtlog.log(string, style);
		}
	}
}

impl Mem {
	pub fn exec_prog(&mut self, prog: &Prog) {
		self.exec_block(prog as &Block);
	}

	fn exec_stmts_here(&mut self, stmts: &Vec<Stmt>) {
		self.excx_mut(0).i.push(0);
		loop {
			if *self.excx(0).i.last().unwrap() >= stmts.len() {
				self.excx_mut(0).flow = Flow::End;
			}
			match self.excx_mut(0).flow {
				Flow::Next => (),
				Flow::Restart => self.excx_mut(0).flow = Flow::Next,
				Flow::End => break,
			}
			self.exec_stmt(&stmts[*self.excx(0).i.last().unwrap()]);
			match self.excx_mut(0).flow {
				Flow::Next => *self.excx_mut(0).i.last_mut().unwrap() += 1,
				Flow::Restart => *self.excx_mut(0).i.last_mut().unwrap() = 0,
				Flow::End => (),
			}
		}
		self.excx_mut(0).i.pop().expect("bug");
	}

	fn exec_block_excx(&mut self, block: &Block, excx: ExCx) -> ExCx {
		self.push_excx(excx);
		self.exec_stmts_here(&block.stmts);
		self.pop_excx()
	}

	fn exec_block(&mut self, block: &Block) {
		self.exec_block_excx(block, ExCx::new());
	}

	fn exec_stmt(&mut self, stmt: &Stmt) {
		match stmt {
			Stmt::Np => 
				if let Some(string_rtlog) = &mut self.debug_mode {
					string_rtlog.indent(String::from("np"), false, style::NORMAL);
					string_rtlog.deindent();
				},
			Stmt::Print {expr} => {
				self.rtlog_indent(String::from("pr"), false, style::NORMAL);
				let string = format!("{}", self.eval_expr(expr));
				if let Some(_) = self.debug_mode {
					self.rtlog_log(format!("output: {}", string), style::NORMAL);
				} else {
					print!("{}", string);
				}
				self.rtlog_deindent();
			},
			Stmt::PrintNewline => {
				self.rtlog_indent(String::from("nl"), false, style::NORMAL);
				if let Some(_) = self.debug_mode {
					self.rtlog_log(String::from("output newline"), style::NORMAL);
				} else {
					println!("");
				}
				self.rtlog_deindent();
			},
			Stmt::Assign {varname, expr} => {
				self.rtlog_indent(format!("assign to variable {}", varname), false, style::NORMAL);
				let val = self.eval_expr(expr);
				self.varset(varname, val);
				self.rtlog_log(format!("now variable {} is {}", varname, self.varget(varname)),
					style::NORMAL);
				self.rtlog_deindent();
			},
			Stmt::AssignIfFree {varname, expr} => {
				self.rtlog_indent(format!("assign if free to variable {}", varname), false,
					style::NORMAL);
				let val = self.eval_expr(expr);
				let was_free = self.varset_if_free(varname, val);
				if was_free {
					self.rtlog_log(format!("now variable {} is {}",
						varname, self.varget(varname)),
						style::NORMAL);
				} else {
					self.rtlog_log(format!("variable {} was already {}",
						varname, self.varget(varname)),
						style::NORMAL);
				}
				self.rtlog_deindent();
			},
			Stmt::Do {expr} =>
				match self.eval_expr(expr) {
					Obj::Block(block) => {
						self.rtlog_indent(String::from("do"), true, style::BOLD_LIGHT_RED);
						self.exec_block(&block);
						self.rtlog_deindent();
					},
					obj => panic!("can't do {} for now", obj),
				},
			Stmt::DoHere {expr} =>
				match self.eval_expr(expr) {
					Obj::Block(block) => {
						self.rtlog_indent(String::from("dh"), false, style::NORMAL);
						self.exec_stmts_here(&block.stmts);
						self.rtlog_deindent();
					},
					obj => panic!("can't do {} for now", obj),
				},
			Stmt::Ev {expr} => {
				self.rtlog_indent(String::from("ev"), false, style::NORMAL);
				self.eval_expr(expr);
				self.rtlog_deindent();
			},
			Stmt::Imp {expr} =>
				match self.eval_expr(expr) {
					Obj::Integer(integer) => {
						self.rtlog_indent(String::from("imp"), false, style::NORMAL);
						let cx_to_import = self.excx(integer as usize).cx.clone();
						self.rtlog_log(format!("import from excx {}", integer), style::NORMAL);
						self.excx_mut(0).cx.import(cx_to_import);
						self.rtlog_deindent();
					},
					invalid_obj => panic!("imp expected integer but found {}", invalid_obj),
				},
			Stmt::Exp {expr} =>
				match self.eval_expr(expr) {
					Obj::Integer(integer) => {
						self.rtlog_indent(String::from("exp"), false, style::NORMAL);
						let cx_to_export = self.excx(0).cx.clone();
						self.rtlog_log(format!("export to excx {}", integer), style::NORMAL);
						self.excx_mut(integer as usize).cx.import(cx_to_export);
						self.rtlog_deindent();
					},
					invalid_obj => panic!("exp expected integer but found {}", invalid_obj),
				},
			Stmt::Redo {expr} =>
				match self.eval_expr(expr) {
					Obj::Integer(integer) => {
						self.rtlog_indent(String::from("redo"), false, style::NORMAL);
						self.rtlog_log(format!("redo excx {}", integer), style::NORMAL);
						self.excx_mut(integer as usize).flow = Flow::Restart;
						self.rtlog_deindent();
					},
					invalid_obj => panic!("redo expected integer but found {}", invalid_obj),
				},
			Stmt::End {expr} =>
				match self.eval_expr(expr) {
					Obj::Integer(integer) => {
						self.rtlog_indent(String::from("end"), false, style::NORMAL);
						self.rtlog_log(format!("end excx {}", integer), style::NORMAL);
						self.excx_mut(integer as usize).flow = Flow::End;
						self.rtlog_deindent();
					},
					invalid_obj => panic!("end expected integer but found {}", invalid_obj),
				},
			Stmt::If {cond_expr, stmt} => {
				self.rtlog_indent(String::from("if"), false, style::NORMAL);
				if self.eval_expr(cond_expr).as_cond() {
					self.rtlog_log(String::from("condition is true"), style::NORMAL);
					self.exec_stmt(stmt)
				} else {
					self.rtlog_log(String::from("condition is false"), style::NORMAL);
				}
				self.rtlog_deindent();
			},
			Stmt::Group {stmts} => {
				self.rtlog_indent(String::from("group"), false, style::NORMAL);
				/*
				for stmt in stmts {
					self.exec_stmt(stmt);
					if self.excx(0).flow != Flow::Next {
						break;
					}
				}
				*/
				self.exec_stmts_here(stmts);
				self.rtlog_deindent();
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
