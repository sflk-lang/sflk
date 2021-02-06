
use crate::stringtree::{StringTree};
use crate::object::Obj;
use crate::utils::styles;


pub type Prog = Block;

#[derive(Debug, Clone)]
pub struct Block {
	pub stmts: Vec<Stmt>,
}

impl From<&Block> for StringTree {
	fn from(block: &Block) -> StringTree {
		StringTree::new_node("block".to_owned(), styles::CYAN, 
			block.stmts.iter()
				.map(|stmt| StringTree::from(stmt))
				.collect()
		)
	}
}

impl Block {
	pub fn new(stmts: Vec<Stmt>) -> Block {
		Block {
			stmts,
		}
	}
}

impl Block {
	pub fn clone_multiply(&self, n: usize) -> Block {
		let mut stmts = Vec::new();
		for _ in 0..n {
			for stmt in &self.stmts {
				stmts.push(stmt.clone());
			}
		}
		Block {stmts}
	}
}


#[derive(Debug, Clone)]
pub enum Stmt {
	Np,
	Print {expr: Expr},
	PrintNewline,
	Assign {varname: String, expr: Expr},
	AssignIfFree {varname: String, expr: Expr},
	Do {expr: Expr},
	DoHere {expr: Expr},
	Ev {expr: Expr},
	Imp {expr: Expr},
	Exp {expr: Expr},
	Redo {expr: Expr},
	End {expr: Expr},
	If {cond_expr: Expr, stmt: Box<Stmt>},
}

impl From<&Stmt> for StringTree {
	fn from(stmt: &Stmt) -> StringTree {
		match stmt {
			Stmt::Np => StringTree::new_leaf(
				String::from("nop"), styles::NORMAL),
			Stmt::Print {expr} => StringTree::new_node(
				String::from("pr"), styles::NORMAL,
				vec![StringTree::from(expr)]),
			Stmt::PrintNewline => StringTree::new_leaf(
				String::from("nl"), styles::NORMAL),
			Stmt::Assign {varname, expr} => StringTree::new_node(
				format!("assign to variable {}", varname), styles::NORMAL,
				vec![StringTree::from(expr)]),
			Stmt::AssignIfFree {varname, expr} => StringTree::new_node(
				format!("assign if free to variable {}", varname), styles::NORMAL,
				vec![StringTree::from(expr)]),
			Stmt::Do {expr} => StringTree::new_node(
				String::from("do"), styles::NORMAL,
				vec![StringTree::from(expr)]),
			Stmt::DoHere {expr} => StringTree::new_node(
				String::from("dh"), styles::NORMAL,
				vec![StringTree::from(expr)]),
			Stmt::Ev {expr} => StringTree::new_node(
				String::from("ev"), styles::NORMAL,
				vec![StringTree::from(expr)]),
			Stmt::Imp {expr} => StringTree::new_node(
				String::from("imp"), styles::NORMAL,
				vec![StringTree::from(expr)]),
			Stmt::Exp {expr} => StringTree::new_node(
				String::from("exp"), styles::NORMAL,
				vec![StringTree::from(expr)]),
			Stmt::Redo {expr} => StringTree::new_node(
				String::from("redo"), styles::NORMAL,
				vec![StringTree::from(expr)]),
			Stmt::End {expr} => StringTree::new_node(
				String::from("end"), styles::NORMAL,
				vec![StringTree::from(expr)]),
			Stmt::If {cond_expr, stmt} => StringTree::new_node(
				String::from("if"), styles::NORMAL,
				vec![StringTree::from(cond_expr), StringTree::from(&**stmt)]),
		}
	}
}


#[derive(Debug, Clone)]
pub enum Expr {
	Var {varname: String},
	Const {val: Obj},
	Chain {init_expr: Box<Expr>, chops: Vec<ChOp>},
}

impl From<&Expr> for StringTree {
	fn from(expr: &Expr) -> StringTree {
		match expr {
			Expr::Var {varname} => StringTree::new_leaf(
				format!("variable {}", varname), styles::NORMAL),
			Expr::Const {val} => StringTree::from(val),
			Expr::Chain {init_expr, chops} => StringTree::new_node(
				"chain".to_string(), styles::NORMAL,
				std::iter::once(StringTree::from(&**init_expr)).chain(
					chops.iter().map(|chop| StringTree::from(chop))).collect()),
		}
	}
}


#[derive(Debug, Clone)]
pub struct ChOp {
	pub op: Op,
	pub expr: Expr,
}

impl From<&ChOp> for StringTree {
	fn from(chop: &ChOp) -> StringTree {
		StringTree::new_node(
			format!("chop {}", chop.op), styles::NORMAL,
			vec![StringTree::from(&chop.expr)])
	}
}

#[derive(Debug, Clone)]
pub enum Op {
	Plus,
	Minus,
	Star,
	Slash,
	ToRight,
}

impl std::fmt::Display for Op {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Op::Plus => write!(f, "plus"),
			Op::Minus => write!(f, "minus"),
			Op::Star => write!(f, "star"),
			Op::Slash => write!(f, "slash"),
			Op::ToRight => write!(f, "to right"),
		}
	}
}
