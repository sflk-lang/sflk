
pub type Prog = Block;

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

#[derive(Debug, Clone)]
pub enum Obj {
	//String(String),
	Integer(isize),
}

impl std::fmt::Display for Obj {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Obj::Integer(integer) => write!(f, "{}", integer),
		}
	}
}

impl Obj {
	fn op_plus(&self, other: &Obj) -> Obj {
		match (self, other) {
			(Obj::Integer(left), Obj::Integer(right)) => Obj::Integer(left + right),
		}
	}

	fn op_minus(&self, other: &Obj) -> Obj {
		match (self, other) {
			(Obj::Integer(left), Obj::Integer(right)) => Obj::Integer(left - right),
		}
	}
}

struct Cx {
}

struct ExCx {
	cx: Cx,
	i: usize,
}

pub struct Mem {
	excx_stack: Vec<ExCx>,
}

impl Mem {
	pub fn new() -> Mem {
		Mem {
			excx_stack: Vec::new(),
		}
	}
}

impl Mem {
	pub fn exec_prog(&mut self, prog: &Prog) {
		self.exec_block(prog as &Block);
	}

	fn exec_block(&mut self, block: &Block) {
		self.excx_stack.push(ExCx {cx: Cx {}, i: 0});
		while self.excx_stack.last().unwrap().i < block.stmts.len() {
			self.exec_stmt(&block.stmts[self.excx_stack.last().unwrap().i]);
			self.excx_stack.last_mut().unwrap().i += 1;
		}
	}

	fn exec_stmt(&mut self, stmt: &Stmt) {
		match stmt {
			Stmt::Print {expr} => println!("{}", self.eval_expr(expr)),
		}
	}

	fn eval_expr(&mut self, expr: &Expr) -> Obj {
		match expr {
			Expr::Const {val} => val.clone(),
			Expr::BinOp {op, left, right} => match op {
				Op::Plus => Obj::op_plus(&self.eval_expr(left), &self.eval_expr(right)),
				Op::Minus => Obj::op_minus(&self.eval_expr(left), &self.eval_expr(right)),
			}
		}
	}
}
