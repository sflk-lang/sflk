use crate::parser2::ParsingWarning;
use crate::scu::Loc;
use crate::stringtree::StringTree;
use crate::utils::{escape_string, styles};

// TODO:
// - move Loc here
// - delete Located
// - parser->program must become parser->ast->program

pub struct Node<T> {
	content: T,
	loc: Loc,
	comments: Comments,
	warnings: Vec<ParsingWarning>,
}

impl<T> Node<T> {
	pub fn from(content: T, loc: Loc) -> Node<T> {
		Node {
			content,
			loc,
			comments: Comments::new(),
			warnings: Vec::new(),
		}
	}

	pub fn unwrap(self) -> T {
		self.content
	}
}

struct Comments {
	left_comments: Vec<String>,
	right_comments: Vec<String>,
	internal_comments: Vec<String>,
}

impl Comments {
	fn new() -> Comments {
		Comments {
			left_comments: Vec::new(),
			right_comments: Vec::new(),
			internal_comments: Vec::new(),
		}
	}
}

impl<T> Node<T> {
	pub fn loc(&self) -> &Loc {
		&self.loc
	}

	pub fn add_loc(mut self, loc: Loc) -> Node<T> {
		// TODO:
		// Change the + impl for Loc so that this looks better
		// like seriously wtf is even that
		self.loc = &loc + &self.loc;
		self.loc = self.loc + loc;
		self
	}
}

impl<T> Node<T> {
	pub fn map<U>(self, func: impl FnOnce(T) -> U) -> Node<U> {
		Node {
			content: func(self.content),
			loc: self.loc,
			comments: self.comments,
			warnings: self.warnings,
		}
	}
}

pub struct Program {
	pub stmts: Vec<Node<Stmt>>,
}

pub enum Stmt {
	Nop,
	Print {
		expr: Node<Expr>,
	},
	Newline,
	Assign {
		target: Node<TargetExpr>,
		expr: Node<Expr>,
	},
	Evaluate {
		expr: Node<Expr>,
	},
	Do {
		expr: Node<Expr>,
	},
	DoHere {
		expr: Node<Expr>,
	},
	DoFileHere {
		expr: Node<Expr>,
	},
	If {
		cond_expr: Node<Expr>,
		th_stmt: Option<Box<Node<Stmt>>>,
		el_stmt: Option<Box<Node<Stmt>>>,
	},
	Invalid, // TODO: Add error details
}

pub enum TargetExpr {
	VariableName(String),
	Invalid, // TODO: Add error details
}

pub enum Expr {
	VariableName(String),
	IntegerLiteral(String),
	StringLiteral(String),
	BlockLiteral(Vec<Node<Stmt>>),
	Chain {
		init: Box<Node<Expr>>,
		chops: Vec<Node<Chop>>,
	},
	Invalid, // TODO: Add error details
}

pub enum Chop {
	Plus(Node<Expr>),
	Minus(Node<Expr>),
	Star(Node<Expr>),
	Slash(Node<Expr>),
	ToRight(Node<Expr>),
	Invalid, // TODO: Add error details
}

pub trait Treeable {
	fn tree(&self, loc: &Loc) -> StringTree;
}

impl<T> From<&Node<T>> for StringTree
where
	T: Treeable,
{
	fn from(node: &Node<T>) -> StringTree {
		node.content.tree(node.loc())
	}
}

impl Treeable for Chop {
	fn tree(&self, loc: &Loc) -> StringTree {
		match self {
			Chop::Plus(expr_node) => StringTree::new_node(
				format!("chop plus"),
				styles::NORMAL,
				vec![StringTree::from(expr_node)],
			),
			Chop::Minus(expr_node) => StringTree::new_node(
				format!("chop minus"),
				styles::NORMAL,
				vec![StringTree::from(expr_node)],
			),
			Chop::Star(expr_node) => StringTree::new_node(
				format!("chop star"),
				styles::NORMAL,
				vec![StringTree::from(expr_node)],
			),
			Chop::Slash(expr_node) => StringTree::new_node(
				format!("chop slash"),
				styles::NORMAL,
				vec![StringTree::from(expr_node)],
			),
			Chop::ToRight(expr_node) => StringTree::new_node(
				format!("chop to_right"),
				styles::NORMAL,
				vec![StringTree::from(expr_node)],
			),
			Chop::Invalid => todo!("TODO"),
		}
	}
}

impl Treeable for Expr {
	fn tree(&self, loc: &Loc) -> StringTree {
		match self {
			Expr::VariableName(name) => {
				StringTree::new_leaf(format!("variable {}", name), styles::NORMAL)
			}
			Expr::IntegerLiteral(integer) => {
				StringTree::new_leaf(format!("integer {}", integer), styles::NORMAL)
			}
			Expr::StringLiteral(string) => StringTree::new_leaf(
				format!("string \"{}\"", escape_string(string, &styles::UNDERLINE)),
				styles::NORMAL,
			),
			Expr::BlockLiteral(stmts) => StringTree::new_node(
				format!("block"),
				styles::CYAN,
				stmts
					.iter()
					.map(|stmt_node| StringTree::from(stmt_node))
					.collect(),
			),
			Expr::Chain { init, chops } => StringTree::new_node(
				format!("chain"),
				styles::BLUE,
				std::iter::once(StringTree::from(&**init))
					.chain(chops.iter().map(|chop_node| StringTree::from(chop_node)))
					.collect(),
			),
			Expr::Invalid => todo!("TODO"),
		}
	}
}

impl Treeable for TargetExpr {
	fn tree(&self, loc: &Loc) -> StringTree {
		match self {
			TargetExpr::VariableName(name) => {
				StringTree::new_leaf(format!("target variable {}", name), styles::NORMAL)
			}
			TargetExpr::Invalid => todo!("TODO"),
		}
	}
}

impl Treeable for Stmt {
	fn tree(&self, loc: &Loc) -> StringTree {
		match self {
			Stmt::Nop => StringTree::new_leaf(format!("nop"), styles::NORMAL),
			Stmt::Print { expr } => StringTree::new_node(
				format!("print"),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Stmt::Newline => StringTree::new_leaf(format!("newline"), styles::NORMAL),
			Stmt::Assign { target, expr } => StringTree::new_node(
				format!("assign"),
				styles::NORMAL,
				vec![StringTree::from(target), StringTree::from(expr)],
			),
			Stmt::Evaluate { expr } => StringTree::new_node(
				format!("evaluate"),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Stmt::Do { expr } => {
				StringTree::new_node(format!("do"), styles::NORMAL, vec![StringTree::from(expr)])
			}
			Stmt::DoHere { expr } => StringTree::new_node(
				format!("do here"),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Stmt::DoFileHere { expr } => StringTree::new_node(
				format!("do file here"),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Stmt::If {
				cond_expr,
				th_stmt,
				el_stmt,
			} => StringTree::new_node(format!("if"), styles::NORMAL, {
				let mut vec: Vec<StringTree> = Vec::with_capacity(3);
				vec.push(StringTree::from(cond_expr));
				if let Some(stmt) = th_stmt {
					vec.push(StringTree::from(&**stmt));
				} else {
					vec.push(StringTree::new_leaf(
						format!("no then branch"),
						styles::NORMAL,
					));
				}
				if let Some(stmt) = el_stmt {
					vec.push(StringTree::from(&**stmt));
				} else {
					vec.push(StringTree::new_leaf(
						format!("no else branch"),
						styles::NORMAL,
					));
				}
				vec
			}),
			Stmt::Invalid => todo!("TODO"),
		}
	}
}

impl Treeable for Program {
	fn tree(&self, loc: &Loc) -> StringTree {
		StringTree::new_node(
			format!("program"),
			styles::CYAN,
			self.stmts
				.iter()
				.map(|stmt_node| StringTree::from(stmt_node))
				.collect(),
		)
	}
}
