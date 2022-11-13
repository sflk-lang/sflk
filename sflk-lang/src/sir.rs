//! SIR stands for Sequential Instruction Representation.
//! Code represented in SIR is composed of a sequence of
//! instructions that can be executed sequentially, as opposed
//! to, say, a tree that would be executed recursively.
//!
//! An adventage of executing code in SIR is that there is
//! an instruction pointer that can be saved to pause execution
//! and resume it later, easily.
//!
//! Temporary data is handled via a stack of SFLK objects.
//!
//!	Contexts hold data such as the declared variables and their value.
//! The contexts are arranged in a tree, as a simple stack cannot handle
//! the case of an interceptor creating a new context (its parent is the
//! same context as an other context from which the intercepted signal comes
//! from, so we need a stack in which a parent can have multiple children,
//! hence the tree).
//!
//! Frames hold data such as the stack of temporary values used for the
//! execution of a sequence of instructions.
//! The frames are arranged in a stack (no need for a tree here, there
//! must be a top frame that is the one in which the execution is taking
//! place at the moment).

use crate::{
	ast::{Chop, Expr, Program, Stmt, TargetExpr, Unop},
	bignums::big_frac::BigFrac,
	object::Object,
	parser::Parser,
	scu::SourceCodeUnit,
	tokenizer::{CharReadingHead, TokBuffer},
	ParserDebuggingLogger,
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
	DeclVar { var_name: String },   // ( -- )
	PopToVar { var_name: String },  // (value -- )
	VarToPush { var_name: String }, // ( -- value)

	// Context operations
	PushContextVars, // ( -- cx)
	DeployContext,   // (cx -- )

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

	// Unary operations
	IsOrdered,         // (list -- bool)
	IsOrderedStrictly, // (list -- bool)
	Length,            // (thing -- length)

	// Binary operators
	Plus,        // (l r -- (l + r))
	Minus,       // (l r -- (l - r))
	Star,        // (l r -- (l * r))
	Slash,       // (l r -- (l / r))
	Comma,       // (l r -- (l , r))
	DoubleComma, // (l r -- (l ,, r))
	Index,       // (l r -- (l . r))
	ToRight,     // (l r -- (l > r))

	// Other operations
	Do,             // (block interceptor -- )
	DoHere,         // (block -- )
	GenericFeature, // (name (arg list) -- res)
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

/// Holds the data used during the execution of a `SirBlock`.
/// A `Frame` is to be pushed on a stack when the execution of a `SirBlock` begins,
/// and be popped when the execution of that block ends.
#[derive(Debug)]
struct Frame {
	sir_block: SirBlock,
	/// Refers to the next instruction in the `sir_block` that is to be executed at7
	/// the next call to `perform_one_step`. Such step shall set `instr_index` to
	/// the instruction that has to be executed in the next step, eventually chosing
	/// an other instruction that the one that follows (when executing a jumping
	/// instruction). If set to an invalid index, then it means that there is no
	/// instruction to execute next, and the next step will pop this frame.
	instr_index: usize,
	/// Stack of temporary values.
	object_stack: Vec<Object>,
	cx_id: ContextId,
	/// If true, then the value of the v variable will be pushed
	/// on the object stack of the frame below when this one is popped.
	// TODO: Find a more elegant solution.
	push_v: bool,
}

impl Frame {
	fn for_sir_block(sir_block: SirBlock, cx_id: ContextId) -> Frame {
		Frame {
			sir_block,
			instr_index: 0,
			object_stack: Vec::new(),
			cx_id,
			push_v: false,
		}
	}

	fn is_done(&self) -> bool {
		self.instr_index >= self.sir_block.instrs.len()
	}

	fn get_next_sir_instr(&self) -> SirInstr {
		self.sir_block.instrs[self.instr_index].clone()
	}
}

/// Holds the data unique to one logical thread of execution.
// TODO: Allow for multiple executions to exist in parallel.
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
pub struct Block {
	sir_block: SirBlock,
}

impl Block {
	pub fn concat(self, right: Block) -> Block {
		Block { sir_block: self.sir_block.concat(right.sir_block) }
	}
}

type ContextId = usize;

/// Holds data such as variable names mapped to their values.
/// Contexts are often reffered to via their `ContextId` by
/// asking the `ContextTable` that contains them.
#[derive(Debug)]
struct Context {
	var_table: HashMap<String, Object>,
	/// No parent means that this context is the root of the context tree.
	parent_context: Option<ContextId>,
	/// Intercepts signals coming from *this* context and its subtree,
	/// when it runs, it does so in the parent of this context.
	///
	/// ```sflk
	/// # Code here runs in context A. #
	/// do {
	/// 	# Code here runs in context B
	/// 	| (a context that was created just for
	/// 	| the execution of this block of code). #
	/// } wi {
	/// 	# Code here runs in context A, despite
	/// 	| the object representing this interceptor
	/// 	| being stored in the context B. #
	/// }
	/// ```
	///
	/// The reason why interceptors are stored in the concept
	/// they intercept signals form and not the context they
	/// are executed in is that they exist as an interceptor
	/// during the same time as the intercepted context, and
	/// this way there is no need to store potentially mutiple
	/// interceptors in the same context. It is simpler this way.
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

	fn decl_var(&mut self, var_name: String) {
		self.var_table.insert(var_name, Object::Nothing);
	}

	fn set_var(&mut self, var_name: String, value: Object) {
		if !self.is_var_decl(&var_name) {
			panic!("Setting undeclared variable");
		}
		self.var_table.insert(var_name, value);
	}

	fn decl_var_and_set(&mut self, var_name: String, value: Object) {
		self.var_table.insert(var_name, value);
	}

	fn var_value_cloned(&self, var_name: &str) -> Option<Object> {
		self.var_table.get(var_name).cloned()
	}

	fn is_var_decl(&self, var_name: &str) -> bool {
		self.var_table.contains_key(var_name)
	}

	fn undecl_var(&mut self, var_name: &str) {
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

	fn create_context_with_interceptor(
		&mut self,
		parent_context: Option<ContextId>,
		interceptor: Object,
	) -> ContextId {
		let new_id = self.next_id;
		self.next_id += 1;
		let mut new_cx = Context::child_of(parent_context);
		new_cx.interceptor = Some(interceptor);
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
	let tfr = TokBuffer::from(CharReadingHead::from_scu(scu));

	// This temporary solution is using a `ParserDebuggingLogger` that does not
	// care about the settings and without logging.
	// TODO: Get the settings parsed from the command line in there somehow.
	let mut parser = Parser::new(
		tfr,
		ParserDebuggingLogger {
			logger: None,
			log_lines: false,
			log_actions: false,
			last_line: 0,
		},
	);

	let ast = parser.parse_program();
	let sir_block = program_to_sir_block(ast.unwrap_ref());
	sir_block
}

impl Execution {
	/// Pushes an object on the stack of temporary values of the top frame.
	fn push_obj(&mut self, value: Object) {
		self.frame_stack
			.last_mut()
			.unwrap()
			.object_stack
			.push(value);
	}

	/// Pops an object from the stack of temporary values of the top frame.
	fn pop_obj(&mut self) -> Object {
		self.frame_stack
			.last_mut()
			.unwrap()
			.object_stack
			.pop()
			.unwrap()
	}

	/// Refers to the SIR code instruction index of the top frame.
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
				context_table.get_mut(cx_id).unwrap().undecl_var("v");
				self.push_obj(v_value);
			} else {
				self.push_obj(Object::Nothing);
			}
		}
	}

	/// This is the core of the whole SIR machine.
	///
	/// TODO: Document what goes on in there.
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
			SirInstr::DeclVar { var_name } => {
				context_table
					.table
					.get_mut(&self.cx_id())
					.unwrap()
					.decl_var(var_name);
				self.advance_instr_index();
			},
			SirInstr::PopToVar { var_name } => {
				let value = self.pop_obj();
				if context_table
					.get(self.cx_id())
					.unwrap()
					.is_var_decl(&var_name)
				{
					context_table
						.table
						.get_mut(&self.cx_id())
						.unwrap()
						.set_var(var_name, value);
					self.advance_instr_index();
				} else {
					let mut var_table = HashMap::new();
					var_table.insert("name".to_string(), Object::String("writevar".to_string()));
					var_table.insert("varname".to_string(), Object::String(var_name));
					var_table.insert("value".to_string(), value);
					let signal = Object::Context(var_table);
					self.emit_signal_and_advance(context_table, signal, false);
				}
			},
			SirInstr::VarToPush { var_name } => {
				if context_table
					.get(self.cx_id())
					.unwrap()
					.is_var_decl(&var_name)
				{
					let value = context_table
						.get(self.cx_id())
						.unwrap()
						.var_value_cloned(&var_name)
						.unwrap();
					self.push_obj(value);
					self.advance_instr_index();
				} else {
					let mut var_table = HashMap::new();
					var_table.insert("name".to_string(), Object::String("readvar".to_string()));
					var_table.insert("varname".to_string(), Object::String(var_name));
					let signal = Object::Context(var_table);
					self.emit_signal_and_advance(context_table, signal, true);
				}
			},
			SirInstr::PushContextVars => {
				let var_table = context_table.get(self.cx_id()).unwrap().var_table.clone();
				self.push_obj(Object::Context(var_table));
				self.advance_instr_index();
			},
			SirInstr::DeployContext => {
				let obj = self.pop_obj();
				match obj {
					Object::Context(new_vars) => {
						for (var_name, value) in new_vars {
							context_table
								.table
								.get_mut(&self.cx_id())
								.unwrap()
								.decl_var_and_set(var_name, value);
						}
					},
					obj => unimplemented!(
						"Deploy context operation on object of type {}",
						obj.type_name()
					),
				}
				self.advance_instr_index();
			},
			SirInstr::RelativeJump { offset } => {
				self.add_to_instr_index(offset);
			},
			SirInstr::RelativeJumpIf { offset } => {
				let cond = self.pop_obj();
				let do_the_jump = matches!(cond, Object::Number(value) if !value.is_zero());
				if do_the_jump {
					self.add_to_instr_index(offset);
				} else {
					self.advance_instr_index();
				}
			},
			SirInstr::LogicalNot => {
				let obj = self.pop_obj();
				let res = obj.logical_not().unwrap();
				self.push_obj(res);
				self.advance_instr_index();
			},
			SirInstr::LogicalAnd => {
				let left = self.pop_obj();
				let right = self.pop_obj();
				let res = left.logical_and(&right).unwrap();
				self.push_obj(res);
				self.advance_instr_index();
			},
			instr @ (SirInstr::IsOrdered | SirInstr::IsOrderedStrictly) => {
				let strictly = matches!(instr, SirInstr::IsOrderedStrictly);
				let obj = self.pop_obj();
				let res = obj.is_ordered(strictly).unwrap();
				self.push_obj(res);
				self.advance_instr_index();
			},
			SirInstr::Length => {
				let obj = self.pop_obj();
				let res = obj.length().unwrap();
				self.push_obj(res);
				self.advance_instr_index();
			},
			SirInstr::Plus => {
				let right = self.pop_obj();
				let left = self.pop_obj();
				let res = left.plus(right).unwrap();
				self.push_obj(res);
				self.advance_instr_index();
			},
			SirInstr::Minus => {
				let right = self.pop_obj();
				let left = self.pop_obj();
				let res = left.minus(right).unwrap();
				self.push_obj(res);
				self.advance_instr_index();
			},
			SirInstr::Star => {
				let right = self.pop_obj();
				let left = self.pop_obj();
				let res = left.star(right).unwrap();
				self.push_obj(res);
				self.advance_instr_index();
			},
			SirInstr::Slash => {
				let right = self.pop_obj();
				let left = self.pop_obj();
				let res = left.slash(right).unwrap();
				self.push_obj(res);
				self.advance_instr_index();
			},
			SirInstr::Comma => {
				let right = self.pop_obj();
				let left = self.pop_obj();
				let res = left.comma(right).unwrap();
				self.push_obj(res);
				self.advance_instr_index();
			},
			SirInstr::DoubleComma => {
				let right = self.pop_obj();
				let left = self.pop_obj();
				let res = left.double_comma(right);
				self.push_obj(res);
				self.advance_instr_index();
			},
			SirInstr::Index => {
				let right = self.pop_obj();
				let left = self.pop_obj();
				let res = left.index(right).unwrap();
				self.push_obj(res);
				self.advance_instr_index();
			},
			SirInstr::ToRight => {
				let right = self.pop_obj();
				let left = self.pop_obj();
				match (left, right) {
					(left, Object::Block(block)) => {
						let sub_context = context_table.create_context(Some(self.cx_id()));
						context_table
							.get_mut(sub_context)
							.unwrap()
							.decl_var("v".to_string());
						context_table
							.get_mut(sub_context)
							.unwrap()
							.set_var("v".to_string(), left);
						let mut sub_frame = Frame::for_sir_block(block.sir_block, sub_context);
						sub_frame.push_v = true; // Will push v.
						self.advance_instr_index();
						self.frame_stack.push(sub_frame);
					},
					(left, Object::String(string)) => {
						let sub_context = context_table.create_context(Some(self.cx_id()));
						context_table
							.get_mut(sub_context)
							.unwrap()
							.decl_var("v".to_string());
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
					(index @ Object::Number(_), list @ Object::List(_)) => {
						let res = list.index(index).unwrap();
						self.push_obj(res);
						self.advance_instr_index();
					},
					(string_index @ Object::String(_), var_table @ Object::Context(_)) => {
						let res = var_table.index(string_index).unwrap();
						self.push_obj(res);
						self.advance_instr_index();
					},
					(left, right) => unimplemented!(
						"To right operation on objects of type {} and {}",
						left.type_name(),
						right.type_name()
					),
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
					obj => {
						unimplemented!("Do here operation on an object of type {}", obj.type_name())
					},
				}
			},
			SirInstr::Do => {
				let interceptor = self.pop_obj();
				let obj = self.pop_obj();
				match obj {
					Object::Block(block) => {
						self.advance_instr_index();
						self.frame_stack.push(Frame::for_sir_block(
							block.sir_block,
							context_table
								.create_context_with_interceptor(Some(self.cx_id()), interceptor),
						));
					},
					Object::String(string) => {
						self.advance_instr_index();
						let sir_block = string_to_sir(string, "some string".to_string());
						self.frame_stack.push(Frame::for_sir_block(
							sir_block,
							context_table
								.create_context_with_interceptor(Some(self.cx_id()), interceptor),
						));
					},
					obj => unimplemented!("Do operation on an object of type {}", obj.type_name()),
				}
			},
			SirInstr::RegisterInterceptor => {
				panic!("Use of removed register interceptor operation");
			},
			SirInstr::EmitSignal => {
				let signal = self.pop_obj();
				self.emit_signal_and_advance(context_table, signal, true);
			},
			SirInstr::IntoPrintSignal => {
				let obj = self.pop_obj();
				let mut var_table = HashMap::new();
				var_table.insert("name".to_string(), Object::String("print".to_string()));
				var_table.insert("value".to_string(), obj);
				self.push_obj(Object::Context(var_table));
				self.advance_instr_index();
			},
			SirInstr::PushNewlineSignal => {
				let mut var_table = HashMap::new();
				var_table.insert("name".to_string(), Object::String("newline".to_string()));
				self.push_obj(Object::Context(var_table));
				self.advance_instr_index();
			},
			SirInstr::PushInputSignal => {
				let mut var_table = HashMap::new();
				var_table.insert("name".to_string(), Object::String("input".to_string()));
				self.push_obj(Object::Context(var_table));
				self.advance_instr_index();
			},
			SirInstr::IntoReadFileSignal => {
				let obj = self.pop_obj();
				let mut var_table = HashMap::new();
				var_table.insert("name".to_string(), Object::String("readfile".to_string()));
				var_table.insert("value".to_string(), obj);
				self.push_obj(Object::Context(var_table));
				self.advance_instr_index();
			},
			SirInstr::GenericFeature => {
				let arg_list = self.pop_obj();
				let feature_name = self.pop_obj();

				let arg_vec = match arg_list {
					Object::Nothing => Vec::new(),
					Object::List(vec) => vec,
					_ => panic!(),
				};

				// Test.
				match feature_name {
					Object::String(feature_name) if feature_name == "testgs" => {
						let mut res_string = String::new();
						res_string += "test generic syntax";
						for arg in arg_vec {
							match arg {
								Object::String(string) => {
									res_string += " ";
									res_string += &string;
								},
								_ => unimplemented!(),
							}
						}
						self.push_obj(Object::String(res_string));
					},
					Object::String(feature_name) => unimplemented!(
						"Generic syntax operation with feature name \"{}\"",
						feature_name
					),
					obj => unimplemented!(
						"Generic syntax operation with feature name object of type {}",
						obj.type_name()
					),
				}
				self.advance_instr_index();
			},
		}
	}

	fn emit_signal_and_advance(
		&mut self,
		context_table: &mut ContextTable,
		signal: Object,
		push_result: bool,
	) {
		let initial_cx_id = Some(self.cx_id());
		let mut opt_cx_id = initial_cx_id;
		loop {
			match opt_cx_id {
				None => {
					let result = perform_signal_past_root(signal);
					if push_result {
						self.push_obj(result);
					}
					self.advance_instr_index();
					break;
				},
				Some(cx_id) => {
					let interceptor = context_table.get_mut(cx_id).unwrap().interceptor.clone();
					let parent_cx_id = context_table.get_mut(cx_id).unwrap().parent_context;
					match interceptor {
						None | Some(Object::Nothing) => {
							if opt_cx_id != initial_cx_id {
								match perform_signal_passing_context(
									&signal,
									context_table.get_mut(cx_id).unwrap(),
								) {
									SignalPassingResult::KeepGoing => (),
									SignalPassingResult::Result(result) => {
										if push_result {
											self.push_obj(result);
										}
										self.advance_instr_index();
										break;
									},
								}
							}
							opt_cx_id = parent_cx_id;
						},
						Some(Object::Block(block)) => {
							self.advance_instr_index();
							let sub_context = context_table.create_context(parent_cx_id);
							context_table
								.get_mut(sub_context)
								.unwrap()
								.decl_var("v".to_string());
							context_table
								.get_mut(sub_context)
								.unwrap()
								.set_var("v".to_string(), signal);
							let mut sub_frame = Frame::for_sir_block(block.sir_block, sub_context);
							sub_frame.push_v = push_result;
							self.frame_stack.push(sub_frame);
							break;
						},
						Some(Object::String(string)) => {
							self.advance_instr_index();
							let sub_context = context_table.create_context(parent_cx_id);
							context_table
								.get_mut(sub_context)
								.unwrap()
								.decl_var("v".to_string());
							context_table
								.get_mut(sub_context)
								.unwrap()
								.set_var("v".to_string(), signal);
							let sir_block = string_to_sir(string, "some string".to_string());
							let mut sub_frame = Frame::for_sir_block(sir_block, sub_context);
							sub_frame.push_v = push_result;
							self.frame_stack.push(sub_frame);
							break;
						},
						Some(interceptor) => unimplemented!(
							"Interception with an interceptor of type {}",
							interceptor.type_name()
						),
					}
				},
			}
		}
	}
}

enum SignalPassingResult {
	KeepGoing,
	Result(Object),
}

fn perform_signal_passing_context(signal: &Object, context: &mut Context) -> SignalPassingResult {
	match signal {
		Object::Context(var_table) => match var_table.get("name") {
			Some(Object::String(sig_name)) if sig_name == "readvar" => match var_table
				.get("varname")
			{
				Some(Object::String(var_name)) => {
					if context.is_var_decl(var_name) {
						SignalPassingResult::Result(context.var_value_cloned(var_name).unwrap())
					} else {
						SignalPassingResult::KeepGoing
					}
				},
				Some(obj) => unimplemented!(
					"Read variable signal on object of type {} passing context",
					obj.type_name()
				),
				None => {
					unimplemented!("Read variable signal but without any object passing context")
				},
			},
			Some(Object::String(sig_name)) if sig_name == "writevar" => match var_table
				.get("varname")
			{
				Some(Object::String(var_name)) => {
					if context.is_var_decl(var_name) {
						context.set_var(
							var_name.to_string(),
							var_table.get("value").unwrap().clone(),
						);
						SignalPassingResult::Result(Object::Nothing)
					} else {
						SignalPassingResult::KeepGoing
					}
				},
				Some(obj) => unimplemented!(
					"Write variable signal on object of type {} passing context",
					obj.type_name()
				),
				None => {
					unimplemented!("Write variable signal but without any object passing context")
				},
			},
			Some(_) | None => SignalPassingResult::KeepGoing,
		},
		_ => SignalPassingResult::KeepGoing,
	}
}

fn perform_signal_past_root(signal: Object) -> Object {
	match signal {
		Object::Context(var_table) => match var_table.get("name") {
			Some(Object::String(sig_name)) if sig_name == "print" => match var_table.get("value") {
				Some(Object::Number(value)) => {
					use crate::bignums::{big_frac, big_sint, big_uint};
					let value_as_string = value.clone().to_string(
						big_frac::string_conversion::ToStringFormat::Slash {
							num_sint_format: big_sint::string_conversion::ToStringFormat {
								uint_format: big_uint::string_conversion::ToStringFormat {
									base: 10,
									upper_case: false,
								},
								plus_sign: false,
							},
							den_sint_format: big_sint::string_conversion::ToStringFormat {
								uint_format: big_uint::string_conversion::ToStringFormat {
									base: 10,
									upper_case: false,
								},
								plus_sign: false,
							},
							shash_even_for_integer: false,
						},
					);
					print!("{value_as_string}");
					Object::Nothing
				},
				Some(Object::String(string)) => {
					print!("{}", string);
					Object::Nothing
				},
				Some(Object::Nothing) => Object::Nothing,
				Some(obj) => unimplemented!(
					"Print signal on object of type {} past root",
					obj.type_name()
				),
				None => unimplemented!("Print signal without any value past root"),
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
			Some(Object::String(sig_name)) if sig_name == "readfile" => {
				match var_table.get("value") {
					Some(Object::String(filename)) => {
						let file_content = std::fs::read_to_string(filename).unwrap();
						Object::String(file_content)
					},
					Some(obj) => {
						unimplemented!(
							"Read file signal on object of type {} past root",
							obj.type_name()
						)
					},
					None => unimplemented!("Read file signal but without any value past root"),
				}
			},
			Some(Object::String(sig_name)) if sig_name == "readvar" => {
				match var_table.get("varname") {
					Some(Object::String(var_name)) => {
						unimplemented!("Read variable signal on name {} past root", var_name)
					},
					Some(_) | None => unimplemented!("Read variable signal past root"),
				}
			},
			Some(Object::String(sig_name)) if sig_name == "writevar" => {
				match var_table.get("varname") {
					Some(Object::String(var_name)) => {
						unimplemented!("Wrire variable signal on name {} past root", var_name)
					},
					Some(_) | None => unimplemented!("Write variable signal past root"),
				}
			},
			Some(Object::String(sig_name)) => {
				unimplemented!("Signal named \"{}\" past root", sig_name)
			},
			Some(obj) => unimplemented!(
				"Signal named by an object of type {} past root",
				obj.type_name()
			),
			None => unimplemented!(
				"Signal described by a context not containing a name variable past root"
			),
		},
		obj => unimplemented!(
			"Signal described by an object of type {} past root",
			obj.type_name()
		),
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

// The code that follows is dedicated to transform programs represented
// as pieces of legacy AST into SIR code.
//
// TODO: Document.

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
				TargetExpr::DeclVariableName(var_name) => {
					sir_instrs.push(SirInstr::DeclVar { var_name: var_name.clone() });
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
				sir_instrs.push(SirInstr::PushConstant { value: Object::Number(BigFrac::zero()) });
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
					bd_sir.push(SirInstr::PushConstant { value: Object::Number(BigFrac::one()) });
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
		Stmt::Do { expr, wi_expr } => {
			expr_to_sir_instrs(expr.unwrap_ref(), sir_instrs);
			if let Some(wi_expr) = wi_expr {
				expr_to_sir_instrs(wi_expr.unwrap_ref(), sir_instrs);
			} else {
				sir_instrs.push(SirInstr::PushConstant { value: Object::Nothing });
			}
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
		Stmt::DeployContext { expr } => {
			expr_to_sir_instrs(expr.unwrap_ref(), sir_instrs);
			sir_instrs.push(SirInstr::DeployContext);
		},
		Stmt::GenericSyntax { expr, ar_exprs, target } => {
			// Feature name.
			expr_to_sir_instrs(expr.unwrap_ref(), sir_instrs);

			// Argument list.
			sir_instrs.push(SirInstr::PushConstant { value: Object::Nothing });
			for ar_expr in ar_exprs {
				expr_to_sir_instrs(ar_expr.unwrap_ref(), sir_instrs);
				sir_instrs.push(SirInstr::Comma);
			}

			sir_instrs.push(SirInstr::GenericFeature);

			// Assign to target if any.
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
		Stmt::Invalid { error_expr } => {
			expr_to_sir_instrs(error_expr.unwrap_ref(), sir_instrs);
			sir_instrs.push(SirInstr::IntoPrintSignal);
			sir_instrs.push(SirInstr::EmitSignal);
			sir_instrs.push(SirInstr::Discard);
			sir_instrs.push(SirInstr::PushNewlineSignal);
			sir_instrs.push(SirInstr::EmitSignal);
			sir_instrs.push(SirInstr::Discard);
		},
	}
}

fn expr_to_sir_instrs(expr: &Expr, sir_instrs: &mut Vec<SirInstr>) {
	match expr {
		Expr::VariableName(var_name) => {
			sir_instrs.push(SirInstr::VarToPush { var_name: var_name.clone() });
		},
		Expr::NothingLiteral => sir_instrs.push(SirInstr::PushConstant { value: Object::Nothing }),
		Expr::IntegerLiteral(integer_string) => {
			use crate::bignums::{big_sint, big_uint};
			sir_instrs.push(SirInstr::PushConstant {
				value: Object::Number(
					BigFrac::from_integer_string(
						integer_string,
						big_sint::string_conversion::FromStringFormat {
							uint_format: big_uint::string_conversion::FromStringFormat {
								base: 10,
								allow_underscores: false,
							},
							allow_sign: false,
						},
					)
					.unwrap(),
				),
			})
		},
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
		Expr::Context => {
			sir_instrs.push(SirInstr::PushContextVars);
		},
		Expr::Unop(unop) => match unop {
			Unop::Negate(expr) => {
				expr_to_sir_instrs(expr.unwrap_ref(), sir_instrs);
				sir_instrs
					.push(SirInstr::PushConstant { value: Object::Number(BigFrac::from(-1i64)) });
				sir_instrs.push(SirInstr::Star);
			},
			Unop::ReadFile(expr) => {
				expr_to_sir_instrs(expr.unwrap_ref(), sir_instrs);
				sir_instrs.push(SirInstr::IntoReadFileSignal);
				sir_instrs.push(SirInstr::EmitSignal);
			},
			Unop::Ordered(expr) => {
				expr_to_sir_instrs(expr.unwrap_ref(), sir_instrs);
				sir_instrs.push(SirInstr::IsOrdered);
			},
			Unop::OrderedStrictly(expr) => {
				expr_to_sir_instrs(expr.unwrap_ref(), sir_instrs);
				sir_instrs.push(SirInstr::IsOrderedStrictly);
			},
			Unop::Length(expr) => {
				expr_to_sir_instrs(expr.unwrap_ref(), sir_instrs);
				sir_instrs.push(SirInstr::Length);
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
						sir_instrs.push(SirInstr::Index);
					},
					Chop::ToRight(right) => {
						expr_to_sir_instrs(right.unwrap_ref(), sir_instrs);
						sir_instrs.push(SirInstr::ToRight);
					},
					_ => unimplemented!(),
				}
			}
		},
		Expr::Invalid { error_expr } => {
			expr_to_sir_instrs(error_expr.unwrap_ref(), sir_instrs);
			sir_instrs.push(SirInstr::IntoPrintSignal);
			sir_instrs.push(SirInstr::EmitSignal);
			sir_instrs.push(SirInstr::Discard);
			sir_instrs.push(SirInstr::PushNewlineSignal);
			sir_instrs.push(SirInstr::EmitSignal);
			sir_instrs.push(SirInstr::Discard);
			sir_instrs.push(SirInstr::PushConstant { value: Object::Nothing });
		},
	}
}
