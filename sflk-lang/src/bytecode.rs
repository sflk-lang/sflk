use crate::ast::{Chop, Expr, Program, Stmt, TargetExpr};
use std::collections::HashMap;

#[derive(Debug, Clone)]
enum UnaryOperator {
	LogicalNot,
}

#[derive(Debug, Clone)]
enum BinaryOperator {
	Plus,
	Minus,
	Star,
	Slash,
	ToRight,
}

#[derive(Debug, Clone)]
enum BcInstr {
	// Essentials
	Nop,                      // ( -- )
	Bruh,                     // ( -- )
	PushConst { value: Obj }, // ( -- value)

	// Stack operations
	Swap, // (a b -- b a)
	Over, // (a b -- a b a)
	Dup,  // (a -- a a)
	Drop, // (a -- )

	// Variable operations
	PopToVar { var_name: String },  // (value -- )
	VarToPush { var_name: String }, // ( -- value)

	// Control flow operations
	RelativeJump { offset: isize },     // ( -- )
	RelativeJumpCond { offset: isize }, // (cond -- )

	// Other operations
	UnOp { un_op: UnaryOperator },    // (a -- (op a))
	BinOp { bin_op: BinaryOperator }, // (a b -- (a op b))
	Sig,                              // (sig -- res)
	Do,                               // (block -- )
	DoHere,                           // (block -- )
	DoFileHere,                       // (filepath -- )
}

#[derive(Debug, Clone)]
pub struct BcBlock {
	instrs: Vec<BcInstr>,
}

impl BcBlock {
	pub fn debug_new() -> BcBlock {
		BcBlock { instrs: vec![BcInstr::Bruh, BcInstr::Bruh] }
	}
}

#[derive(Debug)]
struct Frame {
	bc_block: BcBlock,
	pos: usize,
	stack: Vec<Obj>,
	cx_id: CxId,
}

impl Frame {
	fn for_bc_block(bc_block: BcBlock, cx_id: CxId) -> Frame {
		Frame { bc_block, pos: 0, stack: Vec::new(), cx_id }
	}

	fn is_done(&self) -> bool {
		self.pos >= self.bc_block.instrs.len()
	}
}

#[derive(Debug)]
struct Ip {
	stack: Vec<Frame>,
}

impl Ip {
	fn new() -> Ip {
		Ip { stack: Vec::new() }
	}
}

#[derive(Debug, Clone)]
struct Block {
	bc: BcBlock,
}

#[derive(Debug, Clone)]
enum Sig {
	Print(Obj),
	Newline,
}

#[derive(Debug, Clone)]
enum Obj {
	Integer(i64),
	String(String),
	Block(Block),
	Sig(Box<Sig>),
}

type CxId = usize;

#[derive(Debug)]
struct Cx {
	vars: HashMap<String, Obj>,
}

impl Cx {
	fn new() -> Cx {
		Cx { vars: HashMap::new() }
	}

	fn set_var(&mut self, var_name: String, value: Obj) {
		self.vars.insert(var_name, value);
	}

	fn get_var(&self, var_name: &str) -> &Obj {
		&self.vars[var_name]
	}
}

#[derive(Debug)]
struct Cxs {
	cx_table: HashMap<CxId, Cx>,
	next_cx_id: CxId,
}

impl Cxs {
	fn create_cx(&mut self) -> CxId {
		let new_cx_id = self.next_cx_id;
		self.next_cx_id += 1;
		let new_cx = Cx::new();
		self.cx_table.insert(new_cx_id, new_cx);
		new_cx_id
	}
}

#[derive(Debug)]
struct Vm {
	cxs: Cxs,
	ips: Vec<Ip>,
}

impl Vm {
	fn new() -> Vm {
		Vm {
			cxs: Cxs { cx_table: HashMap::new(), next_cx_id: 0 },
			ips: Vec::new(),
		}
	}

	fn run(&mut self) {
		while !self.ips.is_empty() {
			for ip in self.ips.iter_mut() {
				ip.perform_one_step(&mut self.cxs);
			}
			self.ips.retain(|ip| !ip.stack.is_empty());
		}
	}
}

impl Ip {
	fn push_value(&mut self, value: Obj) {
		self.stack.last_mut().unwrap().stack.push(value);
	}

	fn pop_value(&mut self) -> Obj {
		self.stack.last_mut().unwrap().stack.pop().unwrap()
	}

	fn advance_pos(&mut self) {
		self.stack.last_mut().unwrap().pos += 1;
	}

	fn get_cx_id(&self) -> CxId {
		self.stack.last().unwrap().cx_id
	}

	fn perform_one_step(&mut self, cxs: &mut Cxs) {
		if self.stack.last().unwrap().is_done() {
			self.stack.pop();
			return;
		}
		let frame = self.stack.last().expect("bug?");
		let bc_instr = frame.bc_block.instrs[frame.pos].clone();
		//dbg!(&bc_instr);
		match bc_instr {
			BcInstr::Nop => {
				self.advance_pos();
			},
			BcInstr::Bruh => {
				println!("bruh");
				self.advance_pos();
			},
			BcInstr::PushConst { value } => {
				self.push_value(value);
				self.advance_pos();
			},
			BcInstr::Drop => {
				self.pop_value();
				self.advance_pos();
			},
			BcInstr::Dup => {
				let a = self.pop_value();
				self.push_value(a.clone());
				self.push_value(a);
				self.advance_pos();
			},
			BcInstr::Over => {
				let b = self.pop_value();
				let a = self.pop_value();
				self.push_value(a.clone());
				self.push_value(b);
				self.push_value(a);
				self.advance_pos();
			},
			BcInstr::PopToVar { var_name } => {
				let value = self.pop_value();
				let cx_id = self.get_cx_id();
				cxs.cx_table
					.get_mut(&cx_id)
					.unwrap()
					.set_var(var_name, value);
				self.advance_pos();
			},
			BcInstr::VarToPush { var_name } => {
				let cx_id = self.get_cx_id();
				let value = cxs.cx_table.get(&cx_id).unwrap().get_var(&var_name).clone();
				self.push_value(value);
				self.advance_pos();
			},
			BcInstr::RelativeJump { offset } => {
				let pos = self.stack.last().unwrap().pos as isize;
				self.stack.last_mut().unwrap().pos = (pos + offset) as usize;
			},
			BcInstr::RelativeJumpCond { offset } => {
				let cond = self.pop_value();
				let do_the_jump = matches!(cond, Obj::Integer(value) if value != 0);
				if do_the_jump {
					let pos = self.stack.last().unwrap().pos as isize;
					self.stack.last_mut().unwrap().pos = (pos + offset) as usize;
				} else {
					self.advance_pos();
				}
			},
			BcInstr::UnOp { un_op } => {
				match un_op {
					UnaryOperator::LogicalNot => {
						let right = self.pop_value();
						match right {
							Obj::Integer(value) => {
								self.push_value(Obj::Integer(if value == 0 { 1 } else { 0 }));
							},
							_ => unimplemented!(),
						}
					},
				}
				self.advance_pos();
			},
			BcInstr::BinOp { bin_op } => {
				match bin_op {
					BinaryOperator::Plus => {
						let right = self.pop_value();
						let left = self.pop_value();
						match (left, right) {
							(Obj::Integer(left_value), Obj::Integer(right_value)) => {
								self.push_value(Obj::Integer(left_value + right_value));
							},
							_ => unimplemented!(),
						}
					},
					BinaryOperator::Minus => {
						let right = self.pop_value();
						let left = self.pop_value();
						match (left, right) {
							(Obj::Integer(left_value), Obj::Integer(right_value)) => {
								self.push_value(Obj::Integer(left_value - right_value));
							},
							_ => unimplemented!(),
						}
					},
					BinaryOperator::Star => {
						let right = self.pop_value();
						let left = self.pop_value();
						match (left, right) {
							(Obj::Integer(left_value), Obj::Integer(right_value)) => {
								self.push_value(Obj::Integer(left_value * right_value));
							},
							_ => unimplemented!(),
						}
					},
					BinaryOperator::Slash => {
						let right = self.pop_value();
						let left = self.pop_value();
						match (left, right) {
							(Obj::Integer(left_value), Obj::Integer(right_value)) => {
								self.push_value(Obj::Integer(left_value / right_value));
							},
							_ => unimplemented!(),
						}
					},
					_ => unimplemented!(),
				}
				self.advance_pos();
			},
			BcInstr::DoHere => {
				let obj = self.pop_value();
				match obj {
					Obj::Block(block) => {
						self.advance_pos();
						self.stack
							.push(Frame::for_bc_block(block.bc, self.get_cx_id()));
					},
					_ => unimplemented!(),
				}
			},
			BcInstr::Do => {
				let obj = self.pop_value();
				match obj {
					Obj::Block(block) => {
						self.advance_pos();
						self.stack
							.push(Frame::for_bc_block(block.bc, cxs.create_cx()));
					},
					_ => unimplemented!(),
				}
			},
			_ => unimplemented!(),
		}
	}
}

pub fn exec_bc_block(bc_block: BcBlock) {
	let mut vm = Vm::new();
	let root_cx_id = vm.cxs.create_cx();
	let root_frame = Frame::for_bc_block(bc_block, root_cx_id);
	let mut ip = Ip::new();
	ip.stack.push(root_frame);
	vm.ips.push(ip);
	vm.run();
	dbg!(vm);
}

pub fn program_to_bc_block(program: &Program) -> BcBlock {
	let mut bc_instrs = Vec::new();
	for stmt in program.stmts.iter() {
		stmt_to_bc_instrs(stmt.unwrap_ref(), &mut bc_instrs);
	}
	BcBlock { instrs: bc_instrs }
}

fn stmt_to_bc_instrs(stmt: &Stmt, bc_instrs: &mut Vec<BcInstr>) {
	match stmt {
		Stmt::Nop => {
			bc_instrs.push(BcInstr::Nop);
		},
		Stmt::Newline => {
			// Placeholder for debugging.
			bc_instrs.push(BcInstr::Bruh);
		},
		Stmt::Evaluate { expr } => {
			expr_to_bc_instrs(expr.unwrap_ref(), bc_instrs);
			bc_instrs.push(BcInstr::Drop);
		},
		Stmt::Assign { target, expr } => {
			expr_to_bc_instrs(expr.unwrap_ref(), bc_instrs);
			match target.unwrap_ref() {
				TargetExpr::VariableName(var_name) => {
					bc_instrs.push(BcInstr::PopToVar { var_name: var_name.clone() });
				},
				_ => unimplemented!(),
			}
		},
		Stmt::If { cond_expr, th_stmt, el_stmt } => {
			expr_to_bc_instrs(cond_expr.unwrap_ref(), bc_instrs);
			bc_instrs.push(BcInstr::UnOp { un_op: UnaryOperator::LogicalNot });
			let mut th_bc = Vec::new();
			if let Some(stmt) = th_stmt {
				stmt_to_bc_instrs(stmt.unwrap_ref(), &mut th_bc);
			}
			let el_jump_bc_len = 1;
			bc_instrs.push(BcInstr::RelativeJumpCond {
				offset: th_bc.len() as isize + el_jump_bc_len + 1,
			});
			bc_instrs.extend(th_bc);
			let mut el_bc = Vec::new();
			if let Some(stmt) = el_stmt {
				stmt_to_bc_instrs(stmt.unwrap_ref(), &mut el_bc);
			}
			bc_instrs.push(BcInstr::RelativeJump { offset: el_bc.len() as isize + 1 });
			bc_instrs.extend(el_bc);
		},
		Stmt::Loop { wh_expr, bd_stmt, sp_stmt } => {
			// TODO: Make this more readable and less error-prone by introducing
			// labels and gotos that get resolved into bytecode here.
			// Maybe do it by introducing BcInstrUnresolved or something
			// (it ould have the variants: non-jump BcInstr, Label,
			// and the conditiona jumps to labels).
			if sp_stmt.is_some() {
				// Loop counter for the separator.
				bc_instrs.push(BcInstr::PushConst { value: Obj::Integer(0) });
			}
			let mut sp_bc = Vec::new();
			if let Some(stmt) = sp_stmt {
				let mut sub_sp_bc = Vec::new();
				stmt_to_bc_instrs(stmt.unwrap_ref(), &mut sub_sp_bc);
				// We skip the separation if the loop counter is 0.
				sp_bc.push(BcInstr::Dup);
				sp_bc.push(BcInstr::UnOp { un_op: UnaryOperator::LogicalNot });
				sp_bc.push(BcInstr::RelativeJumpCond { offset: sub_sp_bc.len() as isize + 1 });
				sp_bc.extend(sub_sp_bc);
			}
			let mut bd_bc = Vec::new();
			if let Some(stmt) = bd_stmt {
				stmt_to_bc_instrs(stmt.unwrap_ref(), &mut bd_bc);
				if sp_stmt.is_some() {
					// Increment the loop counter.
					bd_bc.push(BcInstr::PushConst { value: Obj::Integer(1) });
					bd_bc.push(BcInstr::BinOp { bin_op: BinaryOperator::Plus });
				}
			}
			let loop_back_bc_len = 1;
			let mut wh_bc = Vec::new();
			if let Some(expr) = wh_expr {
				expr_to_bc_instrs(expr.unwrap_ref(), &mut wh_bc);
				wh_bc.push(BcInstr::UnOp { un_op: UnaryOperator::LogicalNot });
				wh_bc.push(BcInstr::RelativeJumpCond {
					offset: sp_bc.len() as isize + bd_bc.len() as isize + loop_back_bc_len + 1,
				});
			}
			let loop_back_bc = vec![BcInstr::RelativeJump {
				offset: -(wh_bc.len() as isize + sp_bc.len() as isize + bd_bc.len() as isize),
			}];
			debug_assert_eq!(loop_back_bc_len, loop_back_bc.len() as isize);
			bc_instrs.extend(wh_bc);
			bc_instrs.extend(sp_bc);
			bc_instrs.extend(bd_bc);
			bc_instrs.extend(loop_back_bc);
			if sp_stmt.is_some() {
				// Didn't forget the loop counter.
				bc_instrs.push(BcInstr::Drop);
			}
		},
		Stmt::DoHere { expr } => {
			expr_to_bc_instrs(expr.unwrap_ref(), bc_instrs);
			bc_instrs.push(BcInstr::DoHere);
		},
		Stmt::Do { expr } => {
			expr_to_bc_instrs(expr.unwrap_ref(), bc_instrs);
			bc_instrs.push(BcInstr::Do);
		},
		_ => unimplemented!(),
	}
}

fn expr_to_bc_instrs(expr: &Expr, bc_instrs: &mut Vec<BcInstr>) {
	match expr {
		Expr::IntegerLiteral(integer_string) => bc_instrs.push(BcInstr::PushConst {
			value: Obj::Integer(str::parse(integer_string).expect("TODO: bigints")),
		}),
		Expr::VariableName(var_name) => {
			bc_instrs.push(BcInstr::VarToPush { var_name: var_name.clone() });
		},
		Expr::BlockLiteral(stmts) => {
			let mut sub_bc_instrs = Vec::new();
			for stmt in stmts.iter() {
				stmt_to_bc_instrs(stmt.unwrap_ref(), &mut sub_bc_instrs);
			}
			bc_instrs.push(BcInstr::PushConst {
				value: Obj::Block(Block { bc: BcBlock { instrs: sub_bc_instrs } }),
			});
		},
		Expr::Chain { init, chops } => {
			expr_to_bc_instrs(init.unwrap_ref(), bc_instrs);
			for chop in chops {
				match chop.unwrap_ref() {
					Chop::Plus(right) => {
						expr_to_bc_instrs(right.unwrap_ref(), bc_instrs);
						bc_instrs.push(BcInstr::BinOp { bin_op: BinaryOperator::Plus });
					},
					Chop::Minus(right) => {
						expr_to_bc_instrs(right.unwrap_ref(), bc_instrs);
						bc_instrs.push(BcInstr::BinOp { bin_op: BinaryOperator::Minus });
					},
					Chop::Star(right) => {
						expr_to_bc_instrs(right.unwrap_ref(), bc_instrs);
						bc_instrs.push(BcInstr::BinOp { bin_op: BinaryOperator::Star });
					},
					Chop::Slash(right) => {
						expr_to_bc_instrs(right.unwrap_ref(), bc_instrs);
						bc_instrs.push(BcInstr::BinOp { bin_op: BinaryOperator::Slash });
					},
					_ => unimplemented!(),
				}
			}
		},
		_ => unimplemented!(),
	}
}
