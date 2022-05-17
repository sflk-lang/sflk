use crate::log::IndentedLog;
use crate::object::Obj;
use crate::program::{Block, Chain, Chop, Expr, Stmt};
use crate::utils::{styles, Style};
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct Cx {
	varmap: HashMap<String, Obj>,
}

impl Cx {
	fn new() -> Cx {
		Cx { varmap: HashMap::new() }
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
		ExCx { cx: Cx::new(), i: Vec::new(), flow: Flow::Next }
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Flow {
	Next,
	Restart,
	End,
}

pub struct DebugMem {
	pub log: IndentedLog,
}

impl DebugMem {
	fn new() -> DebugMem {
		DebugMem { log: IndentedLog::new() }
	}
}

pub struct Mem {
	excx_stack: Vec<ExCx>,
	pub debug_mem_opt: Option<DebugMem>,
}

impl Mem {
	pub fn new(is_debug_mode: bool) -> Mem {
		Mem {
			excx_stack: Vec::new(),
			debug_mem_opt: if is_debug_mode {
				Some(DebugMem::new())
			} else {
				None
			},
		}
	}
}

impl DebugMem {
	fn log_indent(&mut self, string: String, is_context: bool, style: Style) {
		self.log.indent(string, is_context, style);
	}

	fn log_deindent(&mut self) {
		self.log.deindent();
	}

	fn log_line(&mut self, string: String, style: Style) {
		self.log.log_line(string, style);
	}
}

impl Mem {
	fn log_indent(&mut self, string: String, is_context: bool, style: Style) {
		if let Some(debug_mem) = &mut self.debug_mem_opt {
			debug_mem.log_indent(string, is_context, style);
		}
	}

	fn log_deindent(&mut self) {
		if let Some(debug_mem) = &mut self.debug_mem_opt {
			debug_mem.log_deindent();
		}
	}

	fn log_line(&mut self, string: String, style: Style) {
		if let Some(debug_mem) = &mut self.debug_mem_opt {
			debug_mem.log_line(string, style);
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
			.unwrap_or_else(|| panic!("get variable {} but it doesn't exist", varname))
	}
}

impl Mem {
	pub fn exec_file(&mut self, filename: String) {
		self.push_excx(ExCx::new());
		self.exec_file_here(filename);
		self.pop_excx();
	}

	fn exec_file_here(&mut self, filename: String) {
		let scu = std::rc::Rc::new(crate::scu::SourceCodeUnit::from_filename(&filename));
		let mut tfr =
			crate::parser::TokBuffer::from(crate::tokenizer::CharReadingHead::from_scu(scu));

		let mut parser = crate::parser::Parser::new();
		let ast = parser.parse_program(&mut tfr);
		if let Some(debug_mem) = &mut self.debug_mem_opt {
			debug_mem
				.log
				.log_line(String::from("Program tree"), styles::NEGATIVE);
			crate::stringtree::StringTree::from(&ast).print(&mut debug_mem.log);
		}
		let block_program = ast.unwrap_ref().to_machine_block();

		self.log_line(String::from("Program execution"), styles::NEGATIVE);
		self.exec_block_here(&block_program);
		self.log_line(String::from("Program end"), styles::NEGATIVE);
	}

	fn exec_stmts_here(&mut self, stmts: &[Stmt]) {
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
				},
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
			},
			Stmt::Print { expr } => {
				self.log_indent(String::from("print"), false, styles::NORMAL);
				let val = self.eval_expr(expr);
				if self.debug_mem_opt.is_some() {
					self.log_line(format!("output {}", val), styles::NORMAL);
				} else {
					match val {
						Obj::Integer(integer) => print!("{}", integer),
						Obj::String(string) => print!("{}", string),
						invalid_object => panic!("can't print {} for now", invalid_object),
					}
				}
				self.log_deindent();
			},
			Stmt::Newline => {
				self.log_indent(String::from("newline"), false, styles::NORMAL);
				if self.debug_mem_opt.is_some() {
					self.log_line(String::from("output newline"), styles::NORMAL);
				} else {
					println!();
				}
				self.log_deindent();
			},
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
			},
			Stmt::Do { expr } => {
				self.log_indent(String::from("do"), true, styles::BOLD_LIGHT_RED);
				match self.eval_expr(expr) {
					Obj::Block(block) => self.exec_block(&block),
					obj => panic!("can't do {} for now", obj),
				}
				self.log_deindent();
			},
			Stmt::DoHere { expr } => {
				self.log_indent(String::from("do here"), false, styles::YELLOW);
				match self.eval_expr(expr) {
					Obj::Block(block) => self.exec_block_here(&block),
					obj => panic!("can't do here {} for now", obj),
				}
				self.log_deindent();
			},
			Stmt::DoFileHere { expr } => {
				self.log_indent(String::from("do file here"), false, styles::YELLOW);
				match self.eval_expr(expr) {
					Obj::String(filename) => self.exec_file_here(filename),
					obj => panic!("can't do file here {} for now", obj),
				}
				self.log_deindent();
			},
			Stmt::Evaluate { expr } => {
				self.log_indent(String::from("evaluate"), false, styles::NORMAL);
				self.eval_expr(expr);
				self.log_deindent();
			},
			Stmt::If { cond_expr, th_stmt, el_stmt } => {
				self.log_indent(String::from("if"), false, styles::NORMAL);
				if self.eval_expr(cond_expr).as_cond() {
					if let Some(stmt) = th_stmt {
						self.log_line(String::from("then branch"), styles::NORMAL);
						self.exec_stmt(stmt);
					} else {
						self.log_line(String::from("no then branch"), styles::NORMAL);
					}
				} else if let Some(stmt) = el_stmt {
					self.log_line(String::from("else branch"), styles::NORMAL);
					self.exec_stmt(stmt);
				} else {
					self.log_line(String::from("no else branch"), styles::NORMAL);
				}
				self.log_deindent();
			},
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
			},
			Expr::Const { val } => {
				self.log_indent(String::from("constant"), false, styles::NORMAL);
				let val = val.clone();
				self.log_line(format!("value is {}", val), styles::NORMAL);
				self.log_deindent();
				val
			},
			Expr::Chain(Chain { init_expr, chops }) => {
				self.log_indent(String::from("chain"), false, styles::BLUE);
				let mut val = self.eval_expr(init_expr);
				self.log_line(format!("initial value is {}", val), styles::NORMAL);
				for chop in chops {
					self.apply_chop(&mut val, chop);
					self.log_line(format!("value now is {}", val), styles::NORMAL);
				}
				self.log_deindent();
				val
			},
		}
	}

	fn apply_chop(&mut self, val: &mut Obj, chop: &Chop) {
		match chop {
			Chop::Plus(expr) => {
				self.log_indent(String::from("chop plus"), false, styles::NORMAL);
				let right = self.eval_expr(expr);
				self.log_line(format!("value {}", right), styles::NORMAL);
				val.apply_plus(right);
				self.log_deindent();
			},
			Chop::Minus(expr) => {
				self.log_indent(String::from("chop minus"), false, styles::NORMAL);
				let right = self.eval_expr(expr);
				self.log_line(format!("value {}", right), styles::NORMAL);
				val.apply_minus(right);
				self.log_deindent();
			},
			Chop::Star(expr) => {
				self.log_indent(String::from("chop star"), false, styles::NORMAL);
				let right = self.eval_expr(expr);
				self.log_line(format!("value {}", right), styles::NORMAL);
				val.apply_star(right);
				self.log_deindent();
			},
			Chop::Slash(expr) => {
				self.log_indent(String::from("chop slash"), false, styles::NORMAL);
				let right = self.eval_expr(expr);
				self.log_line(format!("value {}", right), styles::NORMAL);
				val.apply_slash(right);
				self.log_deindent();
			},
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
					},
					invalid_obj => panic!("can't do {} for now", invalid_obj),
				}
				self.log_deindent();
			},
		}
	}
}
