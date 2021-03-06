use crate::log::IndentedLog;
use crate::object::Obj;
use crate::program::{Block, Chop, Expr, Prog, Stmt};
use crate::scu::Located;
use crate::utils::{styles, Style};
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

// TODO: remove pub
pub struct ExCx {
	cx: Cx,
	i: Vec<usize>,
	flow: Flow,
}

impl ExCx {
	// TODO: remove pub
	pub fn new() -> ExCx {
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
	pub debug_mode: Option<IndentedLog>,
}

impl Mem {
	pub fn new(is_debug_mode: bool) -> Mem {
		Mem {
			excx_stack: Vec::new(),
			debug_mode: if is_debug_mode {
				Some(IndentedLog::new())
			} else {
				None
			},
		}
	}
}

impl Mem {
	// TODO: remove pub
	pub fn push_excx(&mut self, excx: ExCx) {
		self.excx_stack.push(excx);
	}

	// TODO: remove pub
	pub fn pop_excx(&mut self) -> ExCx {
		self.excx_stack.pop().unwrap()
	}

	fn excx(&self, i_from_top: usize) -> &ExCx {
		self.excx_stack
			.get(self.excx_stack.len() - 1 - i_from_top)
			.unwrap()
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
		self.excx(0)
			.cx
			.varmap
			.get(varname)
			.expect(format!("get variable {} but it doesn't exist", varname).as_str())
	}
}

impl Mem {
	fn log_indent(&mut self, string: String, is_context: bool, style: Style) {
		if let Some(indented_log) = &mut self.debug_mode {
			indented_log.indent(string, is_context, style);
		}
	}

	fn log_deindent(&mut self) {
		if let Some(indented_log) = &mut self.debug_mode {
			indented_log.deindent();
		}
	}

	fn log_line(&mut self, string: String, style: Style) {
		if let Some(indented_log) = &mut self.debug_mode {
			indented_log.log_line(string, style);
		}
	}
}

impl Mem {
	pub fn exec_file(&mut self, filename: String) {
		self.push_excx(ExCx::new());
		self.exec_file_here(filename);
		self.pop_excx();
	}

	fn exec_file_here(&mut self, filename: String) {
		/*
		use crate::parser::ProgReadingHead;
		use crate::scu::SourceCodeUnit;
		use crate::stringtree::StringTree;
		use crate::tokenizer::TokReadingHead;
		use std::rc::Rc;

		let scu = Rc::new(SourceCodeUnit::from_filename(&filename));

		let mut prh = ProgReadingHead::from(TokReadingHead::from_scu(scu));
		let prog = match prh.parse_prog() {
			Ok(located_porg) => located_porg.unwrap(),
			Err(parsing_error) => {
				self.log_line(
					format!(
						"\x1b[91m\x1b[1mParsing error:\x1b[22m\x1b[39m {}",
						parsing_error
					),
					styles::NORMAL,
				);
				return;
			}
		};
		self.log_line(String::from("\x1b[7mProgram tree\x1b[27m"), styles::NORMAL);
		if let Some(_) = self.debug_mode {
			StringTree::from(&prog).print(self.debug_mode.as_mut().unwrap());
		}

		self.log_line(
			String::from("\x1b[7mProgram execution\x1b[27m"),
			styles::NORMAL,
		);
		self.exec_prog_here(&prog);
		self.log_line(String::from("\x1b[7mProgram end\x1b[27m"), styles::NORMAL);
		*/
		todo!()
	}

	fn exec_prog_here(&mut self, prog: &Prog) {
		self.exec_block_here(prog as &Block);
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
					self.log_line(String::from("restart"), styles::NORMAL);
				}
				Flow::End => self.log_line(String::from("end"), styles::NORMAL),
			}
		}
		self.excx_mut(0).i.pop().expect("bug");
	}

	fn exec_block_excx(&mut self, block: &Block, excx: ExCx) -> ExCx {
		self.push_excx(excx);
		self.exec_block_here(block);
		self.pop_excx()
	}

	fn exec_block(&mut self, block: &Block) {
		self.exec_block_excx(block, ExCx::new());
	}

	// TODO: remove pub
	pub fn exec_block_here(&mut self, block: &Block) {
		self.exec_stmts_here(&block.stmts);
	}

	fn exec_stmt(&mut self, stmt: &Stmt) {
		match stmt {
			Stmt::Nop => {
				self.log_indent(String::from("nop"), false, styles::NORMAL);
				self.log_deindent();
			}
			Stmt::Print { expr } => {
				self.log_indent(String::from("print"), false, styles::NORMAL);
				let val = self.eval_expr(expr);
				if let Some(_) = self.debug_mode {
					self.log_line(format!("output {}", val), styles::NORMAL);
				} else {
					match val {
						Obj::Integer(integer) => print!("{}", integer),
						Obj::String(string) => print!("{}", string),
						invalid_object => panic!("can't print {} for now", invalid_object),
					}
				}
				self.log_deindent();
			}
			Stmt::Newline => {
				self.log_indent(String::from("newline"), false, styles::NORMAL);
				if let Some(_) = self.debug_mode {
					self.log_line(String::from("output newline"), styles::NORMAL);
				} else {
					println!("");
				}
				self.log_deindent();
			}
			Stmt::Assign { varname, expr } => {
				self.log_indent(String::from("assign"), false, styles::NORMAL);
				self.log_line(format!("to variable {}", varname), styles::NORMAL);
				let val = self.eval_expr(expr);
				self.varset(varname, val);
				self.log_line(
					format!("now variable {} is {}", varname, self.varget(varname)),
					styles::NORMAL,
				);
				self.log_deindent();
			}
			/*
			Stmt::AssignIfFree { varname, expr } => {
				self.log_indent(String::from("assign if free"), false, styles::NORMAL);
				self.log_line(format!("to variable {}", varname), styles::NORMAL);
				let val = self.eval_expr(expr);
				let was_free = self.varset_if_free(varname, val);
				if was_free {
					self.log_line(
						format!("now variable {} is {}", varname, self.varget(varname)),
						styles::NORMAL,
					);
				} else {
					self.log_line(
						format!("variable {} was already {}", varname, self.varget(varname)),
						styles::NORMAL,
					);
				}
				self.log_deindent();
			}*/
			Stmt::Do { expr } => {
				self.log_indent(String::from("do"), true, styles::BOLD_LIGHT_RED);
				match self.eval_expr(expr) {
					Obj::Block(block) => self.exec_block(&block),
					obj => panic!("can't do {} for now", obj),
				}
				self.log_deindent();
			}
			Stmt::DoHere { expr } => {
				self.log_indent(String::from("do here"), false, styles::YELLOW);
				match self.eval_expr(expr) {
					Obj::Block(block) => self.exec_block_here(&block),
					obj => panic!("can't do here {} for now", obj),
				}
				self.log_deindent();
			}
			Stmt::DoFileHere { expr } => {
				self.log_indent(String::from("do file here"), false, styles::YELLOW);
				match self.eval_expr(expr) {
					Obj::String(filename) => self.exec_file_here(filename),
					obj => panic!("can't do file here {} for now", obj),
				}
				self.log_deindent();
			}
			Stmt::Evaluate { expr } => {
				self.log_indent(String::from("evaluate"), false, styles::NORMAL);
				self.eval_expr(expr);
				self.log_deindent();
			}
			/*
			Stmt::Imp { expr } => match self.eval_expr(expr) {
				Obj::Integer(integer) => {
					self.log_indent(String::from("imp"), false, styles::NORMAL);
					let cx_to_import = self.excx(integer as usize).cx.clone();
					self.log_line(format!("import from excx {}", integer), styles::NORMAL);
					self.excx_mut(0).cx.import(cx_to_import);
					self.log_deindent();
				}
				invalid_obj => panic!("imp expected integer but found {}", invalid_obj),
			},
			Stmt::Exp { expr } => match self.eval_expr(expr) {
				Obj::Integer(integer) => {
					self.log_indent(String::from("exp"), false, styles::NORMAL);
					let cx_to_export = self.excx(0).cx.clone();
					self.log_line(format!("export to excx {}", integer), styles::NORMAL);
					self.excx_mut(integer as usize).cx.import(cx_to_export);
					self.log_deindent();
				}
				invalid_obj => panic!("exp expected integer but found {}", invalid_obj),
			},
			Stmt::Redo { expr } => match self.eval_expr(expr) {
				Obj::Integer(integer) => {
					self.log_indent(String::from("redo"), false, styles::NORMAL);
					self.log_line(format!("redo excx {}", integer), styles::NORMAL);
					self.excx_mut(integer as usize).flow = Flow::Restart;
					self.log_deindent();
				}
				invalid_obj => panic!("redo expected integer but found {}", invalid_obj),
			},
			Stmt::End { expr } => match self.eval_expr(expr) {
				Obj::Integer(integer) => {
					self.log_indent(String::from("end"), false, styles::NORMAL);
					self.log_line(format!("end excx {}", integer), styles::NORMAL);
					self.excx_mut(integer as usize).flow = Flow::End;
					self.log_deindent();
				}
				invalid_obj => panic!("end expected integer but found {}", invalid_obj),
			},*/
			Stmt::If {
				cond_expr,
				th_stmt,
				el_stmt,
			} => {
				self.log_indent(String::from("if"), false, styles::NORMAL);
				if self.eval_expr(cond_expr).as_cond() {
					if let Some(stmt) = th_stmt {
						self.log_line(String::from("then branch"), styles::NORMAL);
						self.exec_stmt(stmt);
					} else {
						self.log_line(String::from("no then branch"), styles::NORMAL);
					}
				} else {
					if let Some(stmt) = el_stmt {
						self.log_line(String::from("else branch"), styles::NORMAL);
						self.exec_stmt(stmt);
					} else {
						self.log_line(String::from("no else branch"), styles::NORMAL);
					}
				}
				self.log_deindent();
			}
			Stmt::Invalid => println!("TODO: invalid stmt"),
		}
	}

	fn eval_expr(&mut self, expr: &Expr) -> Obj {
		match expr {
			Expr::Var { varname } => {
				self.log_indent(String::from("read"), false, styles::NORMAL);
				self.log_line(format!("variable {}", varname), styles::NORMAL);
				let val = self.varget(varname).clone();
				self.log_line(format!("value is {}", val), styles::NORMAL);
				self.log_deindent();
				val
			}
			Expr::Const { val } => {
				self.log_indent(String::from("constant"), false, styles::NORMAL);
				let val = val.clone();
				self.log_line(format!("value is {}", val), styles::NORMAL);
				self.log_deindent();
				val
			}
			Expr::Chain { init_expr, chops } => {
				self.log_indent(String::from("chain"), false, styles::BLUE);
				let mut val = self.eval_expr(init_expr);
				self.log_line(format!("initial value is {}", val), styles::NORMAL);
				for chop in chops {
					self.apply_chop(&mut val, chop);
					self.log_line(format!("value now is {}", val), styles::NORMAL);
				}
				self.log_deindent();
				val
			}
		}
	}

	fn apply_chop(&mut self, val: &mut Obj, chop: &Chop) {
		/*
		let right = self.eval_expr(&chop.expr);
		match &chop.op {
			Op::ToRight => {
				self.log_indent(String::from("chop to right"), true, styles::BOLD_LIGHT_RED);
				match right {
					Obj::Block(block) => {
						let mut excx = ExCx::new();
						excx.cx.varmap.insert("v".to_string(), val.clone());
						excx = self.exec_block_excx(&block, excx);
						if let Some(v_value) = excx.cx.varmap.get("v") {
							*val = v_value.to_owned();
						}
					}
					invalid_obj => panic!("can't do {} for now", invalid_obj),
				}
				self.log_deindent();
			}
			op => {
				self.log_indent(String::from("chop"), false, styles::NORMAL);
				self.log_line(format!("op {}", chop.op), styles::NORMAL);
				self.log_line(format!("value {}", right), styles::NORMAL);
				match op {
					Op::Plus => val.plus(right),
					Op::Minus => val.minus(right),
					Op::Star => val.star(right),
					Op::Slash => val.slash(right),
					_ => unreachable!(),
				}
				self.log_deindent();
			}
		}
		*/
		match chop {
			Chop::Plus(expr) => {
				self.log_indent(String::from("chop plus"), false, styles::NORMAL);
				let right = self.eval_expr(expr);
				self.log_line(format!("value {}", right), styles::NORMAL);
				val.apply_plus(right);
				self.log_deindent();
			}
			Chop::Minus(expr) => {
				self.log_indent(String::from("chop minus"), false, styles::NORMAL);
				let right = self.eval_expr(expr);
				self.log_line(format!("value {}", right), styles::NORMAL);
				val.apply_minus(right);
				self.log_deindent();
			}
			Chop::Star(expr) => {
				self.log_indent(String::from("chop star"), false, styles::NORMAL);
				let right = self.eval_expr(expr);
				self.log_line(format!("value {}", right), styles::NORMAL);
				val.apply_star(right);
				self.log_deindent();
			}
			Chop::Slash(expr) => {
				self.log_indent(String::from("chop slash"), false, styles::NORMAL);
				let right = self.eval_expr(expr);
				self.log_line(format!("value {}", right), styles::NORMAL);
				val.apply_slash(right);
				self.log_deindent();
			}
			Chop::ToRight(expr) => {
				self.log_indent(String::from("chop to right"), true, styles::BOLD_LIGHT_RED);
				let right = self.eval_expr(expr);
				match right {
					Obj::Block(block) => {
						let mut excx = ExCx::new();
						excx.cx.varmap.insert("v".to_string(), val.clone());
						excx = self.exec_block_excx(&block, excx);
						if let Some(v_value) = excx.cx.varmap.get("v") {
							*val = v_value.to_owned();
						}
					}
					invalid_obj => panic!("can't do {} for now", invalid_obj),
				}
				self.log_deindent();
			}
		}
	}
}
