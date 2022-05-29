use crate::object::Obj;
use crate::parser::ParsingWarning;
use crate::program;
use crate::scu::Loc;
use crate::stringtree::StringTree;
use crate::utils::{escape_string, styles};

// TODO:
// - move Loc here, or not ?
// - delete Located
// - parser->program must become parser->ast->program

#[derive(Debug)]
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

	pub fn unwrap_ref(&self) -> &T {
		&self.content
	}

	pub fn add_left_comments(&mut self, comments: Vec<Node<Comment>>) {
		self.comments.left_comments.extend(
			comments
				.into_iter()
				.map(|node_comment| node_comment.unwrap()),
		);
	}

	pub fn add_internal_comments(&mut self, comments: Vec<Node<Comment>>) {
		self.comments.internal_comments.extend(
			comments
				.into_iter()
				.map(|node_comment| node_comment.unwrap()),
		);
	}
}

#[derive(Debug)]
pub struct Comment {
	content: String,
	delimitation_thickness: usize,
}

impl Comment {
	pub fn new(content: String, delimitation_thickness: usize) -> Comment {
		Comment { content, delimitation_thickness }
	}
}

#[derive(Debug)]
struct Comments {
	left_comments: Vec<Comment>,
	internal_comments: Vec<Comment>,
}

impl Comments {
	fn new() -> Comments {
		Comments {
			left_comments: Vec::new(),
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
		self.loc += loc;
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

#[derive(Debug)]
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
		th_stmts: Vec<Node<Stmt>>,
		el_stmts: Vec<Node<Stmt>>,
	},
	Loop {
		wh_exprs: Vec<Node<Expr>>,
		bd_stmts: Vec<Node<Stmt>>,
		sp_stmts: Vec<Node<Stmt>>,
	},
	RegisterInterceptor {
		expr: Node<Expr>,
	},
	Emit {
		expr: Node<Expr>,
		target: Option<Node<TargetExpr>>,
	},
	Invalid, // TODO: Add error details
}

#[derive(Debug)]
pub enum TargetExpr {
	VariableName(String),
	Invalid, // TODO: Add error details
}

#[derive(Debug)]
pub enum Expr {
	VariableName(String),
	NothingLiteral,
	IntegerLiteral(String),
	StringLiteral(String),
	BlockLiteral(Vec<Node<Stmt>>),
	Input,
	Unop(Unop),
	Chain { init: Box<Node<Expr>>, chops: Vec<Node<Chop>> },
	Invalid, // TODO: Add error details
}

#[derive(Debug)]
pub enum Unop {
	ReadFile(Box<Node<Expr>>),
}

#[derive(Debug)]
pub enum Chop {
	Plus(Node<Expr>),
	Minus(Node<Expr>),
	Star(Node<Expr>),
	Slash(Node<Expr>),
	Comma(Node<Expr>),
	DoubleComma(Node<Expr>),
	Dot(Node<Expr>),
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
				"chop plus".to_string(),
				styles::NORMAL,
				vec![StringTree::from(expr_node)],
			),
			Chop::Minus(expr_node) => StringTree::new_node(
				"chop minus".to_string(),
				styles::NORMAL,
				vec![StringTree::from(expr_node)],
			),
			Chop::Star(expr_node) => StringTree::new_node(
				"chop star".to_string(),
				styles::NORMAL,
				vec![StringTree::from(expr_node)],
			),
			Chop::Slash(expr_node) => StringTree::new_node(
				"chop slash".to_string(),
				styles::NORMAL,
				vec![StringTree::from(expr_node)],
			),
			Chop::ToRight(expr_node) => StringTree::new_node(
				"chop to_right".to_string(),
				styles::NORMAL,
				vec![StringTree::from(expr_node)],
			),
			Chop::Comma(expr_node) => StringTree::new_node(
				"chop comma".to_string(),
				styles::NORMAL,
				vec![StringTree::from(expr_node)],
			),
			Chop::DoubleComma(expr_node) => StringTree::new_node(
				"chop double comma".to_string(),
				styles::NORMAL,
				vec![StringTree::from(expr_node)],
			),
			Chop::Dot(expr_node) => StringTree::new_node(
				"chop dot".to_string(),
				styles::NORMAL,
				vec![StringTree::from(expr_node)],
			),
			Chop::Invalid => StringTree::new_leaf("invalid".to_string(), styles::BOLD_LIGHT_RED), // TODO
		}
	}
}

impl Treeable for Expr {
	fn tree(&self, loc: &Loc) -> StringTree {
		match self {
			Expr::VariableName(name) => {
				StringTree::new_leaf(format!("variable {}", name), styles::NORMAL)
			},
			Expr::NothingLiteral => StringTree::new_leaf("nothing".to_string(), styles::NORMAL),
			Expr::IntegerLiteral(integer) => {
				StringTree::new_leaf(format!("integer {}", integer), styles::NORMAL)
			},
			Expr::StringLiteral(string) => StringTree::new_leaf(
				format!("string \"{}\"", escape_string(string, &styles::UNDERLINE)),
				styles::NORMAL,
			),
			Expr::BlockLiteral(stmts) => StringTree::new_node(
				"block".to_string(),
				styles::CYAN,
				stmts.iter().map(StringTree::from).collect(),
			),
			Expr::Input => unimplemented!(),
			Expr::Unop(unop) => unimplemented!(),
			Expr::Chain { init, chops } => StringTree::new_node(
				"chain".to_string(),
				styles::BLUE,
				std::iter::once(StringTree::from(&**init))
					.chain(chops.iter().map(StringTree::from))
					.collect(),
			),
			Expr::Invalid => StringTree::new_leaf("invalid".to_string(), styles::BOLD_LIGHT_RED), // TODO
		}
	}
}

impl Treeable for TargetExpr {
	fn tree(&self, loc: &Loc) -> StringTree {
		match self {
			TargetExpr::VariableName(name) => {
				StringTree::new_leaf(format!("target variable {}", name), styles::NORMAL)
			},
			TargetExpr::Invalid => {
				StringTree::new_leaf("invalid".to_string(), styles::BOLD_LIGHT_RED)
			}, // TODO
		}
	}
}

impl Treeable for Stmt {
	fn tree(&self, loc: &Loc) -> StringTree {
		match self {
			Stmt::Nop => StringTree::new_leaf("nop".to_string(), styles::NORMAL),
			Stmt::Print { expr } => StringTree::new_node(
				"print".to_string(),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Stmt::Newline => StringTree::new_leaf("newline".to_string(), styles::NORMAL),
			Stmt::Assign { target, expr } => StringTree::new_node(
				"assign".to_string(),
				styles::NORMAL,
				vec![StringTree::from(target), StringTree::from(expr)],
			),
			Stmt::Evaluate { expr } => StringTree::new_node(
				"evaluate".to_string(),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Stmt::Do { expr } => StringTree::new_node(
				"do".to_string(),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Stmt::DoHere { expr } => StringTree::new_node(
				"do here".to_string(),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Stmt::DoFileHere { expr } => StringTree::new_node(
				"do file here".to_string(),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Stmt::If { .. } => {
				/*
				StringTree::new_node("if".to_string(), styles::NORMAL, {
					let mut vec: Vec<StringTree> = Vec::with_capacity(3);
					vec.push(StringTree::from(cond_expr));
					if let Some(stmt) = th_stmt {
						vec.push(StringTree::from(&**stmt));
					} else {
						vec.push(StringTree::new_leaf(
							"no then branch".to_string(),
							styles::NORMAL,
						));
					}
					if let Some(stmt) = el_stmt {
						vec.push(StringTree::from(&**stmt));
					} else {
						vec.push(StringTree::new_leaf(
							"no else branch".to_string(),
							styles::NORMAL,
						));
					}
					vec
				})
				*/
				unimplemented!()
			},
			Stmt::Loop { .. } => {
				/*
				StringTree::new_node("loop".to_string(), styles::NORMAL, {
					let mut vec: Vec<StringTree> = Vec::with_capacity(3);
					if let Some(expr) = wh_expr {
						vec.push(StringTree::from(expr));
					} else {
						vec.push(StringTree::new_leaf(
							"no while condition".to_string(),
							styles::NORMAL,
						));
					}
					if let Some(stmt) = bd_stmt {
						vec.push(StringTree::from(&**stmt));
					} else {
						vec.push(StringTree::new_leaf("no body".to_string(), styles::NORMAL));
					}
					if let Some(stmt) = sp_stmt {
						vec.push(StringTree::from(&**stmt));
					} else {
						vec.push(StringTree::new_leaf(
							"no separator".to_string(),
							styles::NORMAL,
						));
					}
					vec
				})
				*/
				unimplemented!()
			},
			Stmt::RegisterInterceptor { expr } => StringTree::new_node(
				"register interceptor".to_string(),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Stmt::Emit { expr, target } => {
				StringTree::new_node("if".to_string(), styles::NORMAL, {
					let mut vec: Vec<StringTree> = Vec::with_capacity(2);
					vec.push(StringTree::from(expr));
					if let Some(target_expr) = target {
						vec.push(StringTree::from(target_expr));
					} else {
						vec.push(StringTree::new_leaf(
							"no target".to_string(),
							styles::NORMAL,
						));
					}
					vec
				})
			},
			Stmt::Invalid => StringTree::new_leaf("invalid".to_string(), styles::BOLD_LIGHT_RED), // TODO
		}
	}
}

impl Treeable for Program {
	fn tree(&self, loc: &Loc) -> StringTree {
		StringTree::new_node(
			"program".to_string(),
			styles::CYAN,
			self.stmts.iter().map(StringTree::from).collect(),
		)
	}
}

impl Program {
	pub fn to_machine_block(&self) -> program::Block {
		program::Block {
			stmts: self
				.stmts
				.iter()
				.map(|stmt_node| stmt_node.content.to_machine_stmt())
				.collect(),
		}
	}
}

impl Stmt {
	fn is_invalid(&self) -> bool {
		match self {
			Stmt::Nop => false,
			Stmt::Print { expr } => expr.content.is_invalid(),
			Stmt::Newline => false,
			Stmt::Assign { target, expr } => {
				target.content.is_invalid() || expr.content.is_invalid()
			},
			Stmt::Evaluate { expr } => expr.content.is_invalid(),
			Stmt::Do { expr } => expr.content.is_invalid(),
			Stmt::DoHere { expr } => expr.content.is_invalid(),
			Stmt::DoFileHere { expr } => expr.content.is_invalid(),
			#[rustfmt::skip]
			Stmt::If { cond_expr, th_stmts, el_stmts } => {
				cond_expr.content.is_invalid()
				|| th_stmts
					.iter()
					.fold(false, |acc, stmt| acc || (*stmt).content.is_invalid())
				|| el_stmts
					.iter()
					.fold(false, |acc, stmt| acc || (*stmt).content.is_invalid())
			},
			#[rustfmt::skip]
			Stmt::Loop { wh_exprs, bd_stmts, sp_stmts } => {
				wh_exprs
					.iter()
					.fold(false, |acc, expr| acc || (*expr).content.is_invalid())
				|| bd_stmts
					.iter()
					.fold(false, |acc, stmt| acc || (*stmt).content.is_invalid())
				|| sp_stmts
					.iter()
					.fold(false, |acc, stmt| acc || (*stmt).content.is_invalid())
			},
			Stmt::RegisterInterceptor { expr } => expr.content.is_invalid(),
			#[rustfmt::skip]
			Stmt::Emit { expr, target } => {
				expr
					.content
					.is_invalid()
				|| target
					.as_ref()
					.map(|target_expr| (*target_expr).content.is_invalid())
					.unwrap_or(false)
			},
			Stmt::Invalid => true,
		}
	}

	fn to_machine_stmt(&self) -> program::Stmt {
		match self {
			Stmt::Nop => program::Stmt::Nop,
			Stmt::Print { expr } => program::Stmt::Print { expr: expr.content.to_machine_expr() },
			Stmt::Newline => program::Stmt::Newline,
			Stmt::Assign { target, expr } => program::Stmt::Assign {
				varname: match &target.content {
					TargetExpr::VariableName(varname) => varname.to_string(),
					TargetExpr::Invalid => todo!(),
				},
				expr: expr.content.to_machine_expr(),
			},
			Stmt::Evaluate { expr } => {
				program::Stmt::Evaluate { expr: expr.content.to_machine_expr() }
			},
			Stmt::Do { expr } => program::Stmt::Do { expr: expr.content.to_machine_expr() },
			Stmt::DoHere { expr } => program::Stmt::DoHere { expr: expr.content.to_machine_expr() },
			Stmt::DoFileHere { expr } => {
				program::Stmt::DoFileHere { expr: expr.content.to_machine_expr() }
			},
			Stmt::If { .. } => unimplemented!(), /* program::Stmt::If {
			cond_expr: cond_expr.content.to_machine_expr(),
			th_stmt: th_stmt
			.as_ref()
			.map(|stmt| Box::new((*stmt).content.to_machine_stmt())),
			el_stmt: el_stmt
			.as_ref()
			.map(|stmt| Box::new((*stmt).content.to_machine_stmt())),
			}, */
			Stmt::Loop { .. } => unimplemented!(), /* program::Stmt::Loop {
				wh_expr: wh_expr
					.as_ref()
					.map(|expr| ((*expr).content.to_machine_expr())),
				bd_stmt: bd_stmt
					.as_ref()
					.map(|stmt| Box::new((*stmt).content.to_machine_stmt())),
				sp_stmt: sp_stmt
					.as_ref()
					.map(|stmt| Box::new((*stmt).content.to_machine_stmt())),
			}, */
			Stmt::RegisterInterceptor { .. } => unimplemented!(),
			Stmt::Emit { .. } => unimplemented!(),
			Stmt::Invalid => program::Stmt::Invalid,
		}
	}
}

impl TargetExpr {
	fn is_invalid(&self) -> bool {
		match self {
			TargetExpr::VariableName(_) => false,
			TargetExpr::Invalid => true,
		}
	}
}

impl Expr {
	fn is_invalid(&self) -> bool {
		match self {
			Expr::VariableName(_varname) => false,
			Expr::NothingLiteral => false,
			Expr::IntegerLiteral(_integer_string) => false,
			Expr::StringLiteral(_string_string) => false,
			Expr::BlockLiteral(_stmts) => false,
			Expr::Input => false,
			Expr::Unop(_stmts) => false,
			Expr::Chain { init, chops } => {
				(*init).content.is_invalid()
					|| chops.iter().any(|chop| (*chop).content.is_invalid())
			},
			Expr::Invalid => true,
		}
	}

	fn to_machine_expr(&self) -> program::Expr {
		match self {
			Expr::VariableName(varname) => program::Expr::Var { varname: varname.to_string() },
			Expr::NothingLiteral => unimplemented!(),
			Expr::IntegerLiteral(integer_string) => program::Expr::Const {
				val: Obj::Integer(str::parse(integer_string).expect("TODO: bigints")),
			},
			Expr::StringLiteral(string_string) => {
				program::Expr::Const { val: Obj::String(string_string.clone()) }
			},
			Expr::BlockLiteral(stmts) => program::Expr::Const {
				val: Obj::Block(program::Block {
					stmts: stmts
						.iter()
						.map(|stmt_node| stmt_node.content.to_machine_stmt())
						.collect(),
				}),
			},
			Expr::Input => unimplemented!(),
			Expr::Unop(_) => unimplemented!(),
			Expr::Chain { init, chops } => program::Expr::Chain(program::Chain {
				init_expr: Box::new(init.content.to_machine_expr()),
				chops: chops
					.iter()
					.map(|chop_node| chop_node.content.to_machine_chop())
					.collect(),
			}),
			Expr::Invalid => unreachable!(),
		}
	}
}

impl Chop {
	fn is_invalid(&self) -> bool {
		match self {
			Chop::Plus(expr) => expr.content.is_invalid(),
			Chop::Minus(expr) => expr.content.is_invalid(),
			Chop::Star(expr) => expr.content.is_invalid(),
			Chop::Slash(expr) => expr.content.is_invalid(),
			Chop::ToRight(expr) => expr.content.is_invalid(),
			Chop::Comma(expr) => expr.content.is_invalid(),
			Chop::DoubleComma(expr) => expr.content.is_invalid(),
			Chop::Dot(expr) => expr.content.is_invalid(),
			Chop::Invalid => true,
		}
	}

	fn to_machine_chop(&self) -> program::Chop {
		match self {
			Chop::Plus(expr) => program::Chop::Plus(expr.content.to_machine_expr()),
			Chop::Minus(expr) => program::Chop::Minus(expr.content.to_machine_expr()),
			Chop::Star(expr) => program::Chop::Star(expr.content.to_machine_expr()),
			Chop::Slash(expr) => program::Chop::Slash(expr.content.to_machine_expr()),
			Chop::ToRight(expr) => program::Chop::ToRight(expr.content.to_machine_expr()),
			Chop::Comma(expr) => unimplemented!(),
			Chop::DoubleComma(expr) => unimplemented!(),
			Chop::Dot(expr) => unimplemented!(),
			Chop::Invalid => unreachable!(),
		}
	}
}
