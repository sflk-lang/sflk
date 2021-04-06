use crate::object::Obj;
use crate::stringtree::StringTree;
use crate::utils::styles;

#[derive(Debug, Clone)]
pub struct Block {
	pub stmts: Vec<Stmt>,
}

impl From<&Block> for StringTree {
	fn from(block: &Block) -> StringTree {
		StringTree::new_node(
			"block".to_owned(),
			styles::CYAN,
			block
				.stmts
				.iter()
				.map(|stmt| StringTree::from(stmt))
				.collect(),
		)
	}
}

impl Block {
	pub fn new(stmts: Vec<Stmt>) -> Block {
		Block { stmts }
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
		Block { stmts }
	}
}

#[derive(Debug, Clone)]
pub enum Stmt {
	Nop,
	Print {
		expr: Expr,
	},
	Newline,
	Assign {
		varname: String,
		expr: Expr,
	},
	Do {
		expr: Expr,
	},
	DoHere {
		expr: Expr,
	},
	DoFileHere {
		expr: Expr,
	},
	Evaluate {
		expr: Expr,
	},
	If {
		cond_expr: Expr,
		th_stmt: Option<Box<Stmt>>,
		el_stmt: Option<Box<Stmt>>,
	},
	Invalid, // TODO
}

impl From<&Stmt> for StringTree {
	fn from(stmt: &Stmt) -> StringTree {
		match stmt {
			Stmt::Nop => StringTree::new_leaf(String::from("nop"), styles::NORMAL),
			Stmt::Print { expr } => StringTree::new_node(
				String::from("print"),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Stmt::Newline => StringTree::new_leaf(String::from("newline"), styles::NORMAL),
			Stmt::Assign { varname, expr } => StringTree::new_node(
				format!("assign to variable {}", varname),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Stmt::Do { expr } => StringTree::new_node(
				String::from("do"),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Stmt::DoHere { expr } => StringTree::new_node(
				String::from("do here"),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Stmt::DoFileHere { expr } => StringTree::new_node(
				String::from("do file here"),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Stmt::Evaluate { expr } => StringTree::new_node(
				String::from("evaluate"),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Stmt::If {
				cond_expr,
				th_stmt,
				el_stmt,
			} => StringTree::new_node(String::from("if"), styles::NORMAL, {
				let mut vec: Vec<StringTree> = Vec::with_capacity(3);
				vec.push(StringTree::from(cond_expr));
				if let Some(stmt) = th_stmt {
					vec.push(StringTree::from(&**stmt));
				}
				if let Some(stmt) = el_stmt {
					vec.push(StringTree::from(&**stmt));
				}
				vec
			}),
			Stmt::Invalid => StringTree::new_leaf(format!("invalid"), styles::BOLD_LIGHT_RED), // TODO
		}
	}
}

#[derive(Debug, Clone)]
pub enum Expr {
	Var { varname: String },
	Const { val: Obj },
	Chain(Chain),
}

#[derive(Debug, Clone)]
pub struct Chain {
	pub init_expr: Box<Expr>,
	pub chops: Vec<Chop>,
}

impl From<&Expr> for StringTree {
	fn from(expr: &Expr) -> StringTree {
		match expr {
			Expr::Var { varname } => {
				StringTree::new_leaf(format!("variable {}", varname), styles::NORMAL)
			}
			Expr::Const { val } => StringTree::from(val),
			Expr::Chain(Chain { init_expr, chops }) => StringTree::new_node(
				"chain".to_string(),
				styles::BLUE,
				std::iter::once(StringTree::from(&**init_expr))
					.chain(chops.iter().map(|chop| StringTree::from(chop)))
					.collect(),
			),
		}
	}
}

#[derive(Debug, Clone)]
pub enum Chop {
	Plus(Expr),
	Minus(Expr),
	Star(Expr),
	Slash(Expr),
	ToRight(Expr),
}

impl From<&Chop> for StringTree {
	fn from(chop: &Chop) -> StringTree {
		match chop {
			Chop::Plus(expr) => StringTree::new_node(
				format!("chop plus"),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Chop::Minus(expr) => StringTree::new_node(
				format!("chop minus"),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Chop::Star(expr) => StringTree::new_node(
				format!("chop star"),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Chop::Slash(expr) => StringTree::new_node(
				format!("chop slash"),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Chop::ToRight(expr) => StringTree::new_node(
				format!("chop to right"),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
		}
	}
}
