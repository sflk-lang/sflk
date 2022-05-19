use std::collections::HashMap;

enum BinaryOperator {
	Plus,
	Minus,
	Star,
	Slash,
	ToRight,
}

enum BcInstr {
	// Essentials
	Nop,                      // ( -- )
	Debug,                    // ( -- )
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

pub struct BcBlock {
	instrs: Vec<BcInstr>,
}

impl BcBlock {
	pub fn debug_new() -> BcBlock {
		BcBlock {
			instrs: vec![BcInstr::Debug, BcInstr::Debug],
		}
	}
}

struct Frame {
	bc_block: BcBlock,
	pos: usize,
	cx_id: CxId,
}

impl Frame {
	fn for_bc_block(bc_block: BcBlock, cx_id: CxId) -> Frame {
		Frame {
			bc_block,
			pos: 0,
			cx_id,
		}
	}

	fn is_done(&self) -> bool {
		self.pos >= self.bc_block.instrs.len()
	}
}

struct Ip {
	stack: Vec<Frame>,
}

impl Ip {
	fn new() -> Ip {
		Ip {
			stack: Vec::new(),
		}
	}
}

struct Block {
	bc: BcBlock,
}

enum Sig {
	Print(Obj),
	Newline,
}

enum Obj {
	Integer(i64),
	String(String),
	Block(Block),
	Sig(Box<Sig>),
}

type CxId = usize;

struct Cx {
	vars: HashMap<String, Obj>,
}

impl Cx {
	fn new() -> Cx {
		Cx {
			vars: HashMap::new(),
		}
	}
}

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
				ip.perform_one_step()
			}
			self.ips.retain(|ip| !ip.stack.is_empty());
		}
	}
}

impl Ip {
	fn perform_one_step(&mut self) {
		let frame = self.stack.last().expect("bug?");
		let bc_instr = &frame.bc_block.instrs[frame.pos];
		match bc_instr {
			BcInstr::Nop => {
				self.stack.last_mut().unwrap().pos += 1;
			},
			BcInstr::Debug => {
				println!("debug");
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
}
