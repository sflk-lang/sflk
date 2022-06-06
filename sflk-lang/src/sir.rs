use crate::{
	ast::{Chop, Expr, Program, Stmt, TargetExpr, Unop},
	parser::{Parser},
	scu::SourceCodeUnit,
	tokenizer::{CharReadingHead, TokBuffer},
};
use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Clone)]
enum SirInstr {
	// Essentials
	Nop,                            // ( -- )
	PushConstant { value: Object }, // ( -- value)

	// Stack operations
	Duplicate, // (a -- a a)
	Discard,   // (a -- )

	// Variable operations
	PopToVar { var_name: String },  // (value -- )
	VarToPush { var_name: String }, // ( -- value)

	// Control flow operations
	RelativeJump { offset: isize },   // ( -- )
	RelativeJumpIf { offset: isize }, // (cond -- )

	// Signal operations
	RegisterInterceptor, // (block -- )
	EmitSignal,          // (sig -- res)
	IntoPrintSignal,     // (value -- sig)
	PushNewlineSignal,   // ( -- sig)
	PushInputSignal,     // ( -- sig)
	IntoReadFileSignal,  // (value -- sig)

	// Logical operators
	LogicalNot, // (a -- not_a)
	LogicalAnd, // (a b -- (a && b))

	// Binary operators
	Plus,        // (l r -- (l + r))
	Minus,       // (l r -- (l - r))
	Star,        // (l r -- (l * r))
	Slash,       // (l r -- (l / r))
	Comma,       // (l r -- (l , r))
	DoubleComma, // (l r -- (l ,, r))
	Dot,         // (l r -- (l . r))
	ToRight,     // (l r -- (l > r))

	// Other operations
	Do,     // (block -- )
	DoHere, // (block -- )
}

#[derive(Debug, Clone)]
pub struct SirBlock {
	instrs: Vec<SirInstr>,
}

impl SirBlock {
	fn concat(self, right: SirBlock) -> SirBlock {
		SirBlock {
			instrs: self.instrs.into_iter().chain(right.instrs).collect(),
		}
	}
}

// Holds the data used during the execution of one sir block.
#[derive(Debug)]
struct Frame {
	sir_block: SirBlock,
	/// Refers to the next instruction in the `sir_block`.
	instr_index: usize,
	object_stack: Vec<Object>,
	cx_id: ContextId,
	/// If true, then the value of the v variable will be pushed
	/// on the oject stack of the frame below when this one is popped.
	// TODO: Find a more elegant solution.
	push_v: bool,
	/// If some, then the contained value is what the v variable evaluates to
	/// if it is not defined in the context.
	// TODO: Find a more elegant solution.
	signal: Option<Object>,
}

impl Frame {
	fn for_sir_block(sir_block: SirBlock, cx_id: ContextId) -> Frame {
		Frame {
			sir_block,
			instr_index: 0,
			object_stack: Vec::new(),
			cx_id,
			push_v: false,
			signal: None,
		}
	}

	fn is_done(&self) -> bool {
		self.instr_index >= self.sir_block.instrs.len()
	}

	fn get_next_sir_instr(&self) -> SirInstr {
		self.sir_block.instrs[self.instr_index].clone()
	}
}

#[derive(Debug)]
struct Execution {
	frame_stack: Vec<Frame>,
}

impl Execution {
	fn new() -> Execution {
		Execution { frame_stack: Vec::new() }
	}

	fn is_done(&self) -> bool {
		self.frame_stack.is_empty()
	}
}

#[derive(Debug, Clone)]
struct Block {
	sir_block: SirBlock,
}

impl Block {
	fn concat(self, right: Block) -> Block {
		Block { sir_block: self.sir_block.concat(right.sir_block) }
	}
}

#[derive(Debug, Clone)]
enum Object {
	Nothing,
	Integer(i64),
	String(String),
	Block(Block),
	List(Vec<Object>),
}

type ContextId = usize;

#[derive(Debug)]
struct Context {
	var_table: HashMap<String, Object>,
	parent_context: Option<ContextId>,
	interceptor: Option<Object>,
}

impl Context {
	fn child_of(parent_context: Option<ContextId>) -> Context {
		Context {
			var_table: HashMap::new(),
			parent_context,
			interceptor: None,
		}
	}

	fn set_var(&mut self, var_name: String, value: Object) {
		self.var_table.insert(var_name, value);
	}

	fn var_value_cloned(&self, var_name: &str) -> Option<Object> {
		self.var_table.get(var_name).cloned()
	}

	fn is_var_defined(&self, var_name: &str) -> bool {
		self.var_table.contains_key(var_name)
	}

	fn undefine_var(&mut self, var_name: &str) {
		self.var_table.remove(var_name);
	}
}

#[derive(Debug)]
struct ContextTable {
	table: HashMap<ContextId, Context>,
	next_id: ContextId,
}

impl ContextTable {
	fn new() -> ContextTable {
		ContextTable { table: HashMap::new(), next_id: 0 }
	}

	fn create_context(&mut self, parent_context: Option<ContextId>) -> ContextId {
		let new_id = self.next_id;
		self.next_id += 1;
		let new_cx = Context::child_of(parent_context);
		self.table.insert(new_id, new_cx);
		new_id
	}

	fn get(&self, id: ContextId) -> Option<&Context> {
		self.table.get(&id)
	}

	fn get_mut(&mut self, id: ContextId) -> Option<&mut Context> {
		self.table.get_mut(&id)
	}
}

#[derive(Debug)]
struct Machine {
	context_table: ContextTable,
	executions: Vec<Execution>,
}

impl Machine {
	fn new() -> Machine {
		Machine {
			context_table: ContextTable::new(),
			executions: Vec::new(),
		}
	}

	fn run(&mut self) {
		while !self.executions.is_empty() {
			for execution in self.executions.iter_mut() {
				execution.perform_one_step(&mut self.context_table);
			}
			self.executions.retain(|execution| !execution.is_done());
		}
	}
}

fn string_to_sir(string: String, name: String) -> SirBlock {
	let scu = Rc::new(SourceCodeUnit::from_str(string, name));
	let mut tfr = TokBuffer::from(CharReadingHead::from_scu(scu));
	let mut parser = Parser::new();
	let ast = parser.parse_program(&mut tfr);
	let sir_block = program_to_sir_block(ast.unwrap_ref());
	sir_block
}

impl Execution {
	fn push_obj(&mut self, value: Object) {
		self.frame_stack
			.last_mut()
			.unwrap()
			.object_stack
			.push(value);
	}

	fn pop_obj(&mut self) -> Object {
		self.frame_stack
			.last_mut()
			.unwrap()
			.object_stack
			.pop()
			.unwrap()
	}

	fn instr_index_mut(&mut self) -> &mut usize {
		&mut self.frame_stack.last_mut().unwrap().instr_index
	}

	fn advance_instr_index(&mut self) {
		self.frame_stack.last_mut().unwrap().instr_index += 1;
	}

	fn add_to_instr_index(&mut self, offset: isize) {
		if offset >= 0 {
			*self.instr_index_mut() += offset as usize;
		} else {
			*self.instr_index_mut() -= (-offset) as usize;
		}
	}

	fn cx_id(&self) -> ContextId {
		self.frame_stack.last().unwrap().cx_id
	}

	fn discard_top_frame(&mut self, context_table: &mut ContextTable) {
		let popped_frame = self.frame_stack.pop().unwrap();
		let cx_id = popped_frame.cx_id;
		if popped_frame.push_v {
			if let Some(v_value) = context_table.get(cx_id).unwrap().var_value_cloned("v") {
				// Is undefining v really necessary? Or even a good idea?
				context_table.get_mut(cx_id).unwrap().undefine_var("v");
				self.push_obj(v_value);
			} else {
				self.push_obj(Object::Nothing);
			}
		}
	}

	fn perform_one_step(&mut self, context_table: &mut ContextTable) {
		let top_frame_is_done = self.frame_stack.last().unwrap().is_done();
		if top_frame_is_done {
			self.discard_top_frame(context_table);
			// Is it good to consider that droping an already finished frame is "one step"?
			return;
		}
		let sir_instr = self.frame_stack.last().unwrap().get_next_sir_instr();
		//dbg!(&sir_instr);
		match sir_instr {
			SirInstr::Nop => {
				self.advance_instr_index();
			},
			SirInstr::PushConstant { value } => {
				self.push_obj(value);
				self.advance_instr_index();
			},
			SirInstr::Discard => {
				self.pop_obj();
				self.advance_instr_index();
			},
			SirInstr::Duplicate => {
				let a = self.pop_obj();
				self.push_obj(a.clone());
				self.push_obj(a);
				self.advance_instr_index();
			},
			SirInstr::PopToVar { var_name } => {
				let value = self.pop_obj();
				context_table
					.table
					.get_mut(&self.cx_id())
					.unwrap()
					.set_var(var_name, value);
				self.advance_instr_index();
			},
			SirInstr::VarToPush { var_name } => {
				let value = if var_name == "v"
					&& !context_table.get(self.cx_id()).unwrap().is_var_defined("v")
				{
					let mut frame_index = self.frame_stack.len() - 1;
					loop {
						if let Some(frame) = self.frame_stack.get(frame_index) {
							if frame.cx_id != self.cx_id() {
								break None;
							} else if let Some(sig) = &frame.signal {
								break Some(sig.clone());
							}
							frame_index -= 1;
						} else {
							break None;
						}
					}
				} else {
					None
				};
				let value = value.unwrap_or_else(|| {
					context_table
						.get(self.cx_id())
						.unwrap()
						.var_value_cloned(&var_name)
						.unwrap()
				});
				self.push_obj(value);
				self.advance_instr_index();
			},
			SirInstr::RelativeJump { offset } => {
				self.add_to_instr_index(offset);
			},
			SirInstr::RelativeJumpIf { offset } => {
				let cond = self.pop_obj();
				let do_the_jump = matches!(cond, Object::Integer(value) if value != 0);
				if do_the_jump {
					self.add_to_instr_index(offset);
				} else {
					self.advance_instr_index();
				}
			},
			SirInstr::LogicalNot => {
				let right = self.pop_obj();
				match right {
					Object::Integer(value) => {
						self.push_obj(Object::Integer(if value == 0 { 1 } else { 0 }));
					},
					_ => unimplemented!(),
				}
				self.advance_instr_index();
			},
			SirInstr::LogicalAnd => {
				let left = self.pop_obj();
				let right = self.pop_obj();
				match (left, right) {
					(Object::Integer(left_value), Object::Integer(right_value)) => {
						let value = if left_value != 0 && right_value != 0 {
							1
						} else {
							0
						};
						self.push_obj(Object::Integer(value));
					},
					_ => unimplemented!(),
				}
				self.advance_instr_index();
			},
			SirInstr::Plus => {
				let right = self.pop_obj();
				let left = self.pop_obj();
				match (left, right) {
					(Object::Integer(left_value), Object::Integer(right_value)) => {
						self.push_obj(Object::Integer(left_value + right_value));
					},
					(Object::String(left_string), Object::String(right_string)) => {
						self.push_obj(Object::String(left_string + &right_string));
					},
					(Object::Block(left_block), Object::Block(right_block)) => {
						self.push_obj(Object::Block(left_block.concat(right_block)))
					},
					_ => unimplemented!(),
				}
				self.advance_instr_index();
			},
			SirInstr::Minus => {
				let right = self.pop_obj();
				let left = self.pop_obj();
				match (left, right) {
					(Object::Integer(left_value), Object::Integer(right_value)) => {
						self.push_obj(Object::Integer(left_value - right_value));
					},
					(Object::String(left_string), Object::String(right_string)) => {
						let value = if left_string == right_string { 0 } else { 1 };
						self.push_obj(Object::Integer(value));
					},
					_ => unimplemented!(),
				}
				self.advance_instr_index();
			},
			SirInstr::Star => {
				let right = self.pop_obj();
				let left = self.pop_obj();
				match (left, right) {
					(Object::Integer(left_value), Object::Integer(right_value)) => {
						self.push_obj(Object::Integer(left_value * right_value));
					},
					(Object::String(left_string), Object::Integer(right_value)) => {
						self.push_obj(Object::String(left_string.repeat(right_value as usize)));
					},
					(Object::Block(left_block), Object::Integer(right_value)) => {
						let mut block = left_block.clone();
						for _ in 0..right_value {
							block = block.concat(left_block.clone());
						}
						self.push_obj(Object::Block(block));
					},
					_ => unimplemented!(),
				}
				self.advance_instr_index();
			},
			SirInstr::Slash => {
				let right = self.pop_obj();
				let left = self.pop_obj();
				match (left, right) {
					(Object::Integer(left_value), Object::Integer(right_value)) => {
						self.push_obj(Object::Integer(left_value / right_value));
					},
					(Object::String(left_string), Object::String(right_string)) => {
						self.push_obj(Object::Integer(
							left_string.matches(right_string.as_str()).count() as i64,
						));
					},
					_ => unimplemented!(),
				}
				self.advance_instr_index();
			},
			SirInstr::Comma => {
				let right = self.pop_obj();
				let left = self.pop_obj();
				match (left, right) {
					(Object::Nothing, right) => {
						self.push_obj(Object::List(vec![right]));
					},
					(Object::List(mut vec), right) => {
						vec.push(right);
						self.push_obj(Object::List(vec));
					},
					_ => unimplemented!(),
				}
				self.advance_instr_index();
			},
			SirInstr::DoubleComma => {
				let right = self.pop_obj();
				let left = self.pop_obj();
				self.push_obj(Object::List(vec![left, right]));
				self.advance_instr_index();
			},
			SirInstr::Dot => {
				let right = self.pop_obj();
				let left = self.pop_obj();
				match (left, right) {
					(Object::List(vec), Object::Integer(index)) => {
						self.push_obj(vec.get(index as usize).unwrap().clone());
					},
					_ => unimplemented!(),
				}
				self.advance_instr_index();
			},
			SirInstr::ToRight => {
				let right = self.pop_obj();
				let left = self.pop_obj();
				match right {
					Object::Block(block) => {
						let sub_context = context_table.create_context(Some(self.cx_id()));
						context_table
							.get_mut(sub_context)
							.unwrap()
							.set_var("v".to_string(), left);
						let mut sub_frame = Frame::for_sir_block(block.sir_block, sub_context);
						sub_frame.push_v = true; // Will push v.
						self.advance_instr_index();
						self.frame_stack.push(sub_frame);
					},
					Object::String(string) => {
						let sub_context = context_table.create_context(Some(self.cx_id()));
						context_table
							.get_mut(sub_context)
							.unwrap()
							.set_var("v".to_string(), left);
						let sir_block = string_to_sir(string, "some string".to_string());
						let mut sub_frame = Frame::for_sir_block(sir_block, sub_context);
						sub_frame.push_v = true; // Will push v.
						self.advance_instr_index();
						self.frame_stack.push(sub_frame);
					},
					_ => unimplemented!(),
				}
			},
			SirInstr::DoHere => {
				let obj = self.pop_obj();
				match obj {
					Object::Block(block) => {
						self.advance_instr_index();
						self.frame_stack
							.push(Frame::for_sir_block(block.sir_block, self.cx_id()));
					},
					Object::String(string) => {
						self.advance_instr_index();
						let sir_block = string_to_sir(string, "some string".to_string());
						self.frame_stack
							.push(Frame::for_sir_block(sir_block, self.cx_id()));
					},
					_ => unimplemented!(),
				}
			},
			SirInstr::Do => {
				let obj = self.pop_obj();
				match obj {
					Object::Block(block) => {
						self.advance_instr_index();
						self.frame_stack.push(Frame::for_sir_block(
							block.sir_block,
							context_table.create_context(Some(self.cx_id())),
						));
					},
					Object::String(string) => {
						self.advance_instr_index();
						let sir_block = string_to_sir(string, "some string".to_string());
						self.frame_stack.push(Frame::for_sir_block(
							sir_block,
							context_table.create_context(Some(self.cx_id())),
						));
					},
					_ => unimplemented!(),
				}
			},
			SirInstr::RegisterInterceptor => {
				let obj = self.pop_obj();
				match obj {
					Object::Block(_) => {
						context_table.get_mut(self.cx_id()).unwrap().interceptor = Some(obj);
					},
					Object::String(_) => {
						context_table.get_mut(self.cx_id()).unwrap().interceptor = Some(obj);
					},
					Object::Nothing => {
						context_table.get_mut(self.cx_id()).unwrap().interceptor = None;
					},
					_ => unimplemented!(),
				}
				self.advance_instr_index();
			},
			SirInstr::EmitSignal => {
				let signal = self.pop_obj();
				let mut cx_id = self.cx_id();
				loop {
					let parent_context = context_table.get(cx_id).unwrap().parent_context;
					match parent_context {
						None => {
							let result = peform_signal(signal);
							self.push_obj(result);
							self.advance_instr_index();
							break;
						},
						Some(parent_context) => {
							let interceptor = context_table
								.get(parent_context)
								.unwrap()
								.interceptor
								.clone();
							match interceptor {
								None => {
									cx_id = parent_context;
								},
								Some(Object::Block(block)) => {
									self.advance_instr_index();
									let mut sub_frame =
										Frame::for_sir_block(block.sir_block, parent_context);
									sub_frame.signal = Some(signal);
									sub_frame.push_v = true; // Will push the result of the signal.
									self.frame_stack.push(sub_frame);
									break;
								},
								Some(Object::String(string)) => {
									self.advance_instr_index();
									let sir_block =
										string_to_sir(string, "some string".to_string());
									let mut sub_frame =
										Frame::for_sir_block(sir_block, parent_context);
									sub_frame.signal = Some(signal);
									sub_frame.push_v = true; // Will push the result of the signal.
									self.frame_stack.push(sub_frame);
									break;
								},
								Some(_) => unimplemented!(),
							}
						},
					}
				}
			},
			SirInstr::IntoPrintSignal => {
				let obj = self.pop_obj();
				self.push_obj(Object::List(vec![Object::String("print".to_string()), obj]));
				self.advance_instr_index();
			},
			SirInstr::PushNewlineSignal => {
				self.push_obj(Object::List(vec![Object::String("newline".to_string())]));
				self.advance_instr_index();
			},
			SirInstr::PushInputSignal => {
				self.push_obj(Object::List(vec![Object::String("input".to_string())]));
				self.advance_instr_index();
			},
			SirInstr::IntoReadFileSignal => {
				let obj = self.pop_obj();
				self.push_obj(Object::List(vec![
					Object::String("readfile".to_string()),
					obj,
				]));
				self.advance_instr_index();
			},
		}
	}
}

fn peform_signal(signal: Object) -> Object {
	match signal {
		Object::List(vec) => match vec.get(0) {
			Some(Object::String(sig_name)) if sig_name == "print" => match vec.get(1) {
				Some(Object::Integer(value)) => {
					print!("{}", value);
					Object::Nothing
				},
				Some(Object::String(string)) => {
					print!("{}", string);
					Object::Nothing
				},
				_ => unimplemented!(),
			},
			Some(Object::String(sig_name)) if sig_name == "newline" => {
				println!();
				Object::Nothing
			},
			Some(Object::String(sig_name)) if sig_name == "input" => {
				use std::io::Write;
				std::io::stdout().flush().ok();
				let mut input = String::new();
				std::io::stdin().read_line(&mut input).expect("h");
				Object::String(input)
			},
			Some(Object::String(sig_name)) if sig_name == "readfile" => match vec.get(1) {
				Some(Object::String(filename)) => {
					let file_content = std::fs::read_to_string(filename).unwrap();
					Object::String(file_content)
				},
				_ => unimplemented!(),
			},
			_ => unimplemented!(),
		},
		_ => unimplemented!(),
	}
}

pub fn exec_sir_block(sir_block: SirBlock) {
	let mut machine = Machine::new();
	let root_cx_id = machine.context_table.create_context(None);
	let first_frame = Frame::for_sir_block(sir_block, root_cx_id);
	let mut execuion = Execution::new();
	execuion.frame_stack.push(first_frame);
	machine.executions.push(execuion);
	machine.run();
	//dbg!(machine);
}

pub fn program_to_sir_block(program: &Program) -> SirBlock {
	let mut sir_instrs = Vec::new();
	for stmt in program.stmts.iter() {
		stmt_to_sir_instrs(stmt.unwrap_ref(), &mut sir_instrs);
	}
	SirBlock { instrs: sir_instrs }
}

fn stmt_to_sir_instrs(stmt: &Stmt, sir_instrs: &mut Vec<SirInstr>) {
	match stmt {
		Stmt::Nop => {
			sir_instrs.push(SirInstr::Nop);
		},
		Stmt::Evaluate { expr } => {
			expr_to_sir_instrs(expr.unwrap_ref(), sir_instrs);
			sir_instrs.push(SirInstr::Discard);
		},
		Stmt::Assign { target, expr } => {
			expr_to_sir_instrs(expr.unwrap_ref(), sir_instrs);
			match target.unwrap_ref() {
				TargetExpr::VariableName(var_name) => {
					sir_instrs.push(SirInstr::PopToVar { var_name: var_name.clone() });
				},
				_ => unimplemented!(),
			}
		},
		Stmt::If { cond_expr, th_stmts, el_stmts } => {
			expr_to_sir_instrs(cond_expr.unwrap_ref(), sir_instrs);
			sir_instrs.push(SirInstr::LogicalNot);
			let mut th_sir = Vec::new();
			for stmt in th_stmts {
				stmt_to_sir_instrs(stmt.unwrap_ref(), &mut th_sir);
			}
			let el_jump_sir_len = 1;
			sir_instrs.push(SirInstr::RelativeJumpIf {
				offset: th_sir.len() as isize + el_jump_sir_len + 1,
			});
			sir_instrs.extend(th_sir);
			let mut el_sir = Vec::new();
			for stmt in el_stmts {
				stmt_to_sir_instrs(stmt.unwrap_ref(), &mut el_sir);
			}
			sir_instrs.push(SirInstr::RelativeJump { offset: el_sir.len() as isize + 1 });
			sir_instrs.extend(el_sir);
		},
		Stmt::Loop { wh_exprs, bd_stmts, sp_stmts, ao_flag } => {
			// TODO: Make this more readable and less error-prone by introducing
			// labels and gotos that get resolved into sir here.
			// Maybe do it by introducing BcInstrUnresolved or something
			// (it ould have the variants: non-jump SirInstr, Label,
			// and the conditiona jumps to labels).
			let has_loop_counter = !sp_stmts.is_empty();
			if has_loop_counter {
				// Loop counter for the separator.
				sir_instrs.push(SirInstr::PushConstant { value: Object::Integer(0) });
			}
			let mut sp_sir = Vec::new();
			if !sp_stmts.is_empty() {
				let mut sub_sp_sir = Vec::new();
				for stmt in sp_stmts {
					stmt_to_sir_instrs(stmt.unwrap_ref(), &mut sub_sp_sir);
				}
				// We skip the separation if the loop counter is 0.
				sp_sir.push(SirInstr::Duplicate);
				sp_sir.push(SirInstr::LogicalNot);
				sp_sir.push(SirInstr::RelativeJumpIf { offset: sub_sp_sir.len() as isize + 1 });
				sp_sir.extend(sub_sp_sir);
			}
			let mut bd_sir = Vec::new();
			if !bd_stmts.is_empty() {
				for stmt in bd_stmts {
					stmt_to_sir_instrs(stmt.unwrap_ref(), &mut bd_sir);
				}
				if has_loop_counter {
					// Increment the loop counter.
					bd_sir.push(SirInstr::PushConstant { value: Object::Integer(1) });
					bd_sir.push(SirInstr::Plus);
				}
			}
			let loop_back_sir_len = 1;
			let mut wh_sir = Vec::new();
			if let Some((last, exprs)) = wh_exprs.split_last() {
				for expr in exprs {
					expr_to_sir_instrs(expr.unwrap_ref(), &mut wh_sir);
					bd_sir.push(SirInstr::LogicalAnd);
				}
				expr_to_sir_instrs(last.unwrap_ref(), &mut wh_sir);
				wh_sir.push(SirInstr::LogicalNot);
				wh_sir.push(SirInstr::RelativeJumpIf {
					offset: sp_sir.len() as isize + bd_sir.len() as isize + loop_back_sir_len + 1,
				});
			}
			let loop_back_sir = vec![SirInstr::RelativeJump {
				offset: -(wh_sir.len() as isize + sp_sir.len() as isize + bd_sir.len() as isize),
			}];
			debug_assert_eq!(loop_back_sir_len, loop_back_sir.len() as isize);
			if ao_flag.is_some() {
				sir_instrs.push(SirInstr::RelativeJump {
					offset: wh_sir.len() as isize + sp_sir.len() as isize + 1,
				});
			}
			sir_instrs.extend(wh_sir);
			sir_instrs.extend(sp_sir);
			sir_instrs.extend(bd_sir);
			sir_instrs.extend(loop_back_sir);
			if has_loop_counter {
				// Didn't forget the loop counter.
				sir_instrs.push(SirInstr::Discard);
			}
		},
		Stmt::DoHere { expr } => {
			expr_to_sir_instrs(expr.unwrap_ref(), sir_instrs);
			sir_instrs.push(SirInstr::DoHere);
		},
		Stmt::Do { expr } => {
			expr_to_sir_instrs(expr.unwrap_ref(), sir_instrs);
			sir_instrs.push(SirInstr::Do);
		},
		Stmt::Newline => {
			sir_instrs.push(SirInstr::PushNewlineSignal);
			sir_instrs.push(SirInstr::EmitSignal);
			sir_instrs.push(SirInstr::Discard);
		},
		Stmt::Print { expr } => {
			expr_to_sir_instrs(expr.unwrap_ref(), sir_instrs);
			sir_instrs.push(SirInstr::IntoPrintSignal);
			sir_instrs.push(SirInstr::EmitSignal);
			sir_instrs.push(SirInstr::Discard);
		},
		Stmt::RegisterInterceptor { expr } => {
			expr_to_sir_instrs(expr.unwrap_ref(), sir_instrs);
			sir_instrs.push(SirInstr::RegisterInterceptor);
		},
		Stmt::Emit { expr, target } => {
			expr_to_sir_instrs(expr.unwrap_ref(), sir_instrs);
			sir_instrs.push(SirInstr::EmitSignal);
			if let Some(target_expr) = target {
				match target_expr.unwrap_ref() {
					TargetExpr::VariableName(var_name) => {
						sir_instrs.push(SirInstr::PopToVar { var_name: var_name.clone() });
					},
					_ => unimplemented!(),
				}
			} else {
				sir_instrs.push(SirInstr::Discard);
			}
		},
		h => unimplemented!("{:?}", h),
	}
}

fn expr_to_sir_instrs(expr: &Expr, sir_instrs: &mut Vec<SirInstr>) {
	match expr {
		Expr::VariableName(var_name) => {
			sir_instrs.push(SirInstr::VarToPush { var_name: var_name.clone() });
		},
		Expr::NothingLiteral => sir_instrs.push(SirInstr::PushConstant { value: Object::Nothing }),
		Expr::IntegerLiteral(integer_string) => sir_instrs.push(SirInstr::PushConstant {
			value: Object::Integer(str::parse(integer_string).expect("TODO: bigints")),
		}),
		Expr::StringLiteral(string_string) => {
			sir_instrs.push(SirInstr::PushConstant { value: Object::String(string_string.clone()) })
		},
		Expr::BlockLiteral(stmts) => {
			let mut sub_sir_instrs = Vec::new();
			for stmt in stmts.iter() {
				stmt_to_sir_instrs(stmt.unwrap_ref(), &mut sub_sir_instrs);
			}
			sir_instrs.push(SirInstr::PushConstant {
				value: Object::Block(Block { sir_block: SirBlock { instrs: sub_sir_instrs } }),
			});
		},
		Expr::Input => {
			sir_instrs.push(SirInstr::PushInputSignal);
			sir_instrs.push(SirInstr::EmitSignal);
		},
		Expr::Unop(unop) => match unop {
			Unop::ReadFile(expr) => {
				expr_to_sir_instrs(expr.unwrap_ref(), sir_instrs);
				sir_instrs.push(SirInstr::IntoReadFileSignal);
				sir_instrs.push(SirInstr::EmitSignal);
			},
			Unop::Negate(expr) => {
				expr_to_sir_instrs(expr.unwrap_ref(), sir_instrs);
				sir_instrs.push(SirInstr::PushConstant { value: Object::Integer(-1) });
				sir_instrs.push(SirInstr::Star);
			},
		},
		Expr::Chain { init, chops } => {
			expr_to_sir_instrs(init.unwrap_ref(), sir_instrs);
			for chop in chops {
				match chop.unwrap_ref() {
					Chop::Plus(right) => {
						expr_to_sir_instrs(right.unwrap_ref(), sir_instrs);
						sir_instrs.push(SirInstr::Plus);
					},
					Chop::Minus(right) => {
						expr_to_sir_instrs(right.unwrap_ref(), sir_instrs);
						sir_instrs.push(SirInstr::Minus);
					},
					Chop::Star(right) => {
						expr_to_sir_instrs(right.unwrap_ref(), sir_instrs);
						sir_instrs.push(SirInstr::Star);
					},
					Chop::Slash(right) => {
						expr_to_sir_instrs(right.unwrap_ref(), sir_instrs);
						sir_instrs.push(SirInstr::Slash);
					},
					Chop::Comma(right) => {
						expr_to_sir_instrs(right.unwrap_ref(), sir_instrs);
						sir_instrs.push(SirInstr::Comma);
					},
					Chop::DoubleComma(right) => {
						expr_to_sir_instrs(right.unwrap_ref(), sir_instrs);
						sir_instrs.push(SirInstr::DoubleComma);
					},
					Chop::Index(right) => {
						expr_to_sir_instrs(right.unwrap_ref(), sir_instrs);
						sir_instrs.push(SirInstr::Dot);
					},
					Chop::ToRight(right) => {
						expr_to_sir_instrs(right.unwrap_ref(), sir_instrs);
						sir_instrs.push(SirInstr::ToRight);
					},
					_ => unimplemented!(),
				}
			}
		},
		_ => unimplemented!(),
	}
}
