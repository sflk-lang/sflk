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
	PushConst { value: Obj }, // ( -- value)

	// Stack operations
	Swap, // (a b -- b a)
	Dup,  // (a -- a a)
	Drop, // (a -- )

	// Variable operations
	PopToVar { var_name: String },  // (value -- )
	VarToPush { var_name: String }, // ( -- value)

	// Other operations
	BinOp { bin_op: BinaryOperator }, // (a b -- (a op b))
	Sig,                              // (sig -- res)
	Do,                               // (block -- )
	DoHere,                           // (block -- )
	DoFileHere,                       // (filepath -- )
}

type CxId = usize;

struct BcBlock {
	instrs: Vec<BcInstr>,
}

struct Frame {
	bc_block: BcBlock,
	pos: usize,
	cx_id: CxId,
}

struct Ip {
	stack: Vec<Frame>,
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

struct Cx {
	vars: HashMap<String, Obj>,
}

struct Vm {
	cxs: HashMap<CxId, Cx>,
	ip: Ip,
}
