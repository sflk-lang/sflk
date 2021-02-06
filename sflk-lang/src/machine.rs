
use std::collections::HashMap;
use crate::program::{Prog, Block, Stmt, Expr, ChOp, Op};
use crate::object::Obj;
use crate::stringrtlog::StringRtlog;
use crate::utils::{Style, styles, escape_string};


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


#[derive(Debug, Clone, PartialEq, Eq)]
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
				break;
			}
			match self.excx_mut(0).flow {
				Flow::Next => (),
				Flow::Restart => self.excx_mut(0).flow = Flow::Next,
				Flow::End => break,
			}
			self.exec_stmt(&stmts[*self.excx(0).i.last().unwrap()]);
			match self.excx_mut(0).flow {
				Flow::Next => *self.excx_mut(0).i.last_mut().unwrap() += 1,
				Flow::Restart => {
					*self.excx_mut(0).i.last_mut().unwrap() = 0;
					self.rtlog_log(String::from("restart"), styles::NORMAL);
				},
				Flow::End => self.rtlog_log(String::from("end"), styles::NORMAL),
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
			Stmt::Np => {
				self.rtlog_indent(String::from("np"), false, styles::NORMAL);
				self.rtlog_deindent();
			},
			Stmt::Print {expr} => {
				self.rtlog_indent(String::from("pr"), false, styles::NORMAL);
				let val = self.eval_expr(expr);
				if let Some(_) = self.debug_mode {
					self.rtlog_log(format!("output {}", val),
						styles::NORMAL);
				} else {
					match val {
						Obj::Integer(integer) => print!("{}", integer),
						Obj::String(string) => print!("{}", string),
						invalid_object => panic!("can't print {} for now", invalid_object),
					}
				}
				self.rtlog_deindent();
			},
			Stmt::PrintNewline => {
				self.rtlog_indent(String::from("nl"), false, styles::NORMAL);
				if let Some(_) = self.debug_mode {
					self.rtlog_log(String::from("output newline"), styles::NORMAL);
				} else {
					println!("");
				}
				self.rtlog_deindent();
			},
			Stmt::Assign {varname, expr} => {
				self.rtlog_indent(String::from("assign"), false, styles::NORMAL);
				self.rtlog_log(format!("to variable {}", varname), styles::NORMAL);
				let val = self.eval_expr(expr);
				self.varset(varname, val);
				self.rtlog_log(format!("now variable {} is {}", varname, self.varget(varname)),
					styles::NORMAL);
				self.rtlog_deindent();
			},
			Stmt::AssignIfFree {varname, expr} => {
				self.rtlog_indent(String::from("assign if free"), false, styles::NORMAL);
				self.rtlog_log(format!("to variable {}", varname), styles::NORMAL);
				let val = self.eval_expr(expr);
				let was_free = self.varset_if_free(varname, val);
				if was_free {
					self.rtlog_log(format!("now variable {} is {}",
						varname, self.varget(varname)),
						styles::NORMAL);
				} else {
					self.rtlog_log(format!("variable {} was already {}",
						varname, self.varget(varname)),
						styles::NORMAL);
				}
				self.rtlog_deindent();
			},
			Stmt::Do {expr} => {
				self.rtlog_indent(String::from("do"), true, styles::BOLD_LIGHT_RED);
				match self.eval_expr(expr) {
					Obj::Block(block) => {
						self.exec_block(&block);
					},
					obj => panic!("can't do {} for now", obj),
				}
				self.rtlog_deindent();
			},
			Stmt::DoHere {expr} => {
				self.rtlog_indent(String::from("dh"), false, styles::YELLOW);
				match self.eval_expr(expr) {
					Obj::Block(block) => {
						self.exec_stmts_here(&block.stmts);
					},
					obj => panic!("can't dh {} for now", obj),
				}
				self.rtlog_deindent();
			},
			Stmt::FileDoHere {expr} => {
				self.rtlog_indent(String::from("fh"), false, styles::YELLOW);
				match self.eval_expr(expr) {
					Obj::String(filename) => {
						// TODO:
						// Rewrite this temporary code, divide it into cool functions, etc.
						// Try to reduce code duplication beteen here and in main

						use std::rc::Rc;
						use crate::tokenizer::{SourceCodeUnit, TokReadingHead};
						use crate::parser::ProgReadingHead;
						use crate::stringtree::StringTree;

						let scu = Rc::new(SourceCodeUnit::from_filename(&filename));

						let mut prh = ProgReadingHead::from(TokReadingHead::from_scu(scu));
						let prog = match prh.parse_prog() {
							Ok((prog, _)) => prog,
							Err(parsing_error) => {
								self.rtlog_log(
									format!("\x1b[91m\x1b[1mParsing error:\x1b[22m\x1b[39m {}",
										parsing_error), 
									styles::NORMAL);
								return;
							},
						};
						self.rtlog_log(String::from("\x1b[7mProgram tree\x1b[27m"),
							styles::NORMAL);
						if let Some(_) = self.debug_mode {
							StringTree::from(&prog).print(self.debug_mode.as_mut().unwrap());
						}

						self.rtlog_log(String::from("\x1b[7mProgram execution\x1b[27m"),
							styles::NORMAL);
						self.exec_stmts_here(&prog.stmts);
						self.rtlog_log(String::from("\x1b[7mProgram end\x1b[27m"),
							styles::NORMAL);
					},
					obj => panic!("can't fh {} for now", obj),
				}
				self.rtlog_deindent();
			},
			Stmt::Ev {expr} => {
				self.rtlog_indent(String::from("ev"), false, styles::NORMAL);
				self.eval_expr(expr);
				self.rtlog_deindent();
			},
			Stmt::Imp {expr} =>
				match self.eval_expr(expr) {
					Obj::Integer(integer) => {
						self.rtlog_indent(String::from("imp"), false, styles::NORMAL);
						let cx_to_import = self.excx(integer as usize).cx.clone();
						self.rtlog_log(format!("import from excx {}", integer), styles::NORMAL);
						self.excx_mut(0).cx.import(cx_to_import);
						self.rtlog_deindent();
					},
					invalid_obj => panic!("imp expected integer but found {}", invalid_obj),
				},
			Stmt::Exp {expr} =>
				match self.eval_expr(expr) {
					Obj::Integer(integer) => {
						self.rtlog_indent(String::from("exp"), false, styles::NORMAL);
						let cx_to_export = self.excx(0).cx.clone();
						self.rtlog_log(format!("export to excx {}", integer), styles::NORMAL);
						self.excx_mut(integer as usize).cx.import(cx_to_export);
						self.rtlog_deindent();
					},
					invalid_obj => panic!("exp expected integer but found {}", invalid_obj),
				},
			Stmt::Redo {expr} =>
				match self.eval_expr(expr) {
					Obj::Integer(integer) => {
						self.rtlog_indent(String::from("redo"), false, styles::NORMAL);
						self.rtlog_log(format!("redo excx {}", integer), styles::NORMAL);
						self.excx_mut(integer as usize).flow = Flow::Restart;
						self.rtlog_deindent();
					},
					invalid_obj => panic!("redo expected integer but found {}", invalid_obj),
				},
			Stmt::End {expr} =>
				match self.eval_expr(expr) {
					Obj::Integer(integer) => {
						self.rtlog_indent(String::from("end"), false, styles::NORMAL);
						self.rtlog_log(format!("end excx {}", integer), styles::NORMAL);
						self.excx_mut(integer as usize).flow = Flow::End;
						self.rtlog_deindent();
					},
					invalid_obj => panic!("end expected integer but found {}", invalid_obj),
				},
			Stmt::If {cond_expr, if_stmt, el_stmt} => {
				self.rtlog_indent(String::from("if"), false, styles::NORMAL);
				if self.eval_expr(cond_expr).as_cond() {
					self.rtlog_log(String::from("condition is true"), styles::NORMAL);
					self.exec_stmt(if_stmt)
				} else {
					self.rtlog_log(String::from("condition is false"), styles::NORMAL);
					match el_stmt {
						Some(stmt) => self.exec_stmt(stmt),
						None => (),
					}
				}
				self.rtlog_deindent();
			},
		}
	}

	fn eval_expr(&mut self, expr: &Expr) -> Obj {
		match expr {
			Expr::Var {varname} => {
				self.rtlog_indent(String::from("read"), false, styles::NORMAL);
				self.rtlog_log(format!("variable {}", varname), styles::NORMAL);
				let val = self.varget(varname).clone();
				self.rtlog_log(format!("value is {}", val), styles::NORMAL);
				self.rtlog_deindent();
				val
			},
			Expr::Const {val} => {
				self.rtlog_indent(String::from("constant"), false, styles::NORMAL);
				let val = val.clone();
				self.rtlog_log(format!("value is {}", val), styles::NORMAL);
				self.rtlog_deindent();
				val
			},
			Expr::Chain {init_expr, chops} => {
				self.rtlog_indent(String::from("chain"), false, styles::BLUE);
				let mut val = self.eval_expr(init_expr);
				self.rtlog_log(format!("initial value is {}", val), styles::NORMAL);
				for chop in chops {
					self.apply_chop(&mut val, chop);
					self.rtlog_log(format!("value now is {}", val), styles::NORMAL);
				}
				self.rtlog_deindent();
				val
			},
		}
	}

	fn apply_chop(&mut self, val: &mut Obj, chop: &ChOp) {
		let right = self.eval_expr(&chop.expr);
		self.rtlog_indent(String::from("chop"), false, styles::NORMAL);
		self.rtlog_log(format!("op {}", chop.op), styles::NORMAL);
		self.rtlog_log(format!("value {}", right), styles::NORMAL);
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
		self.rtlog_deindent();
	}
}
