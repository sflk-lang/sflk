use crate::ast::{Chop, Expr, Program, Stmt, TargetExpr};
use std::collections::HashMap;

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
	Dup,  // (a -- a a)
	Drop, // (a -- )

	// Variable operations
	PopToVar { var_name: String },  // (value -- )
	VarToPush { var_name: String }, // ( -- value)

	// Control flow operations
	RelativeJump { offset: isize },     // ( -- )
	RelativeJumpCond { offset: isize }, // (cond -- )

	// Other operations
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
}

#[derive(Debug)]
struct Vm {
	cxs: HashMap<CxId, Cx>,
	next_cx_id: CxId,
	ips: Vec<Ip>,
}

impl Vm {
	fn new() -> Vm {
		Vm {
			cxs: HashMap::new(),
			next_cx_id: 0,
			ips: Vec::new(),
		}
	}

	fn create_cx(&mut self) -> CxId {
		let new_cx_id = self.next_cx_id;
		self.next_cx_id += 1;
		let new_cx = Cx::new();
		self.cxs.insert(new_cx_id, new_cx);
		new_cx_id
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
	fn perform_one_step(&mut self, cxs: &mut HashMap<CxId, Cx>) {
		let frame = self.stack.last().expect("bug?");
		let bc_instr = frame.bc_block.instrs[frame.pos].clone();
		match bc_instr {
			BcInstr::Nop => {
				self.stack.last_mut().unwrap().pos += 1;
			},
			BcInstr::Bruh => {
				println!("bruh");
				self.stack.last_mut().unwrap().pos += 1;
			},
			BcInstr::PushConst { value } => {
				self.stack.last_mut().unwrap().stack.push(value);
				self.stack.last_mut().unwrap().pos += 1;
			},
			BcInstr::Drop => {
				self.stack.last_mut().unwrap().stack.pop();
				self.stack.last_mut().unwrap().pos += 1;
			},
			BcInstr::PopToVar { var_name } => {
				let value = self.stack.last_mut().unwrap().stack.pop().unwrap();
				let cx_id = self.stack.last().unwrap().cx_id;
				cxs.get_mut(&cx_id).unwrap().vars.insert(var_name, value);
				self.stack.last_mut().unwrap().pos += 1;
			},
			BcInstr::VarToPush { var_name } => {
				let cx_id = self.stack.last().unwrap().cx_id;
				let value = cxs[&cx_id].vars[&var_name].clone();
				self.stack.last_mut().unwrap().stack.push(value);
				self.stack.last_mut().unwrap().pos += 1;
			},
			BcInstr::BinOp { bin_op } => {
				match bin_op {
					BinaryOperator::Plus => {
						let right = self.stack.last_mut().unwrap().stack.pop().unwrap();
						let left = self.stack.last_mut().unwrap().stack.pop().unwrap();
						match (left, right) {
							(Obj::Integer(left_value), Obj::Integer(right_value)) => {
								self.stack
									.last_mut()
									.unwrap()
									.stack
									.push(Obj::Integer(left_value + right_value));
							},
							_ => unimplemented!(),
						}
					},
					_ => unimplemented!(),
				}
				self.stack.last_mut().unwrap().pos += 1;
			},
			_ => unimplemented!(),
		}
		if self.stack.last().unwrap().is_done() {
			self.stack.pop();
		}
	}
}

pub fn exec_bc_block(bc_block: BcBlock) {
	let mut vm = Vm::new();
	let root_cx_id = vm.create_cx();
	let root_frame = Frame::for_bc_block(bc_block, root_cx_id);
	let mut ip = Ip::new();
	ip.stack.push(root_frame);
	vm.ips.push(ip);
	vm.run();
	dbg!(vm);
}

pub fn program_to_bc_block(program: &Program) -> BcBlock {
	let mut bc_instrs = Vec::new();
	for stmt in program.stmts.iter().map(|stmt_node| stmt_node.unwrap_ref()) {
		match stmt {
			Stmt::Nop => {
				bc_instrs.push(BcInstr::Nop);
			},
			Stmt::Newline => {
				// Placeholder for debugging.
				bc_instrs.push(BcInstr::Bruh);
			},
			Stmt::Evaluate { expr } => {
				expr_to_bc_instrs(expr.unwrap_ref(), &mut bc_instrs);
				bc_instrs.push(BcInstr::Drop);
			},
			Stmt::Assign { target, expr } => {
				expr_to_bc_instrs(expr.unwrap_ref(), &mut bc_instrs);
				match target.unwrap_ref() {
					TargetExpr::VariableName(var_name) => {
						bc_instrs.push(BcInstr::PopToVar { var_name: var_name.clone() })
					},
					_ => unimplemented!(),
				}
			},
			_ => unimplemented!(),
		}
	}
	BcBlock { instrs: bc_instrs }
}

fn expr_to_bc_instrs(expr: &Expr, bc_instrs: &mut Vec<BcInstr>) {
	match expr {
		Expr::IntegerLiteral(integer_string) => bc_instrs.push(BcInstr::PushConst {
			value: Obj::Integer(str::parse(integer_string).expect("TODO: bigints")),
		}),
		Expr::VariableName(var_name) => {
			bc_instrs.push(BcInstr::VarToPush { var_name: var_name.clone() })
		},
		Expr::Chain { init, chops } => {
			expr_to_bc_instrs(init.unwrap_ref(), bc_instrs);
			for chop in chops {
				match chop.unwrap_ref() {
					Chop::Plus(right) => {
						expr_to_bc_instrs(right.unwrap_ref(), bc_instrs);
						bc_instrs.push(BcInstr::BinOp { bin_op: BinaryOperator::Plus });
					},
					_ => unimplemented!(),
				}
			}
		},
		_ => unimplemented!(),
	}
}
