
#[derive(Debug)]
pub struct Block {
	stmts: Vec<Stmt>,
}

impl Block {
	pub fn new(stmts: Vec<Stmt>) -> Block {
		Block {
			stmts,
		}
	}
}

#[derive(Debug)]
pub enum Stmt {
	Print {expr: Expr},
	//Assign {varname: String, expr: Expr},
}

#[derive(Debug)]
pub enum Expr {
	//Var {varname: String},
	Const {val: Obj},
	BinOp {op: Op, left: Box<Expr>, right: Box<Expr>},
}

#[derive(Debug)]
pub enum Op {
	Plus,
	Minus,
	//Star,
	//Slash,
}

#[derive(Debug)]
pub enum Obj {
	//String(String),
	Integer(isize),
}
