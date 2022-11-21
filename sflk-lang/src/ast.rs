use crate::{
	log::IndentedLog,
	parser::ParsingWarning,
	scu::Loc,
	stringtree::StringTree,
	utils::{escape_string, styles},
};

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
}

#[derive(Debug)]
pub struct Comment {
	_content: String,
	_delimitation_thickness: usize,
}

#[derive(Debug)]
struct Comments {
	_left_comments: Vec<Comment>,
	_internal_comments: Vec<Comment>,
}

impl Comments {
	fn new() -> Comments {
		Comments {
			_left_comments: Vec::new(),
			_internal_comments: Vec::new(),
		}
	}
}

impl<T> Node<T> {
	pub fn loc(&self) -> &Loc {
		&self.loc
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
		wi_expr: Option<Node<Expr>>,
	},
	DoHere {
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
		ao_flag: Option<Node<()>>,
	},
	RegisterInterceptor {
		expr: Node<Expr>,
	},
	Emit {
		expr: Node<Expr>,
		target: Option<Node<TargetExpr>>,
	},
	DeployContext {
		expr: Node<Expr>,
	},
	GenericSyntax {
		expr: Node<Expr>,
		ar_exprs: Vec<Node<Expr>>,
		target: Option<Node<TargetExpr>>,
	},
	Invalid {
		error_expr: Node<Expr>,
	},
}

#[derive(Debug)]
pub enum TargetExpr {
	VariableName(String),
	DeclVariableName(String),
	Invalid, // TODO: Add error details
}

pub enum Expr {
	VariableName(String),
	NothingLiteral,
	IntegerLiteral(String),
	StringLiteral(String),
	BlockLiteral(Vec<Node<Stmt>>),
	Input,
	Context,
	Unop(Unop),
	Chain { init: Box<Node<Expr>>, chops: Vec<Node<Chop>> },
	Invalid { error_expr: Box<Node<Expr>> },
}

pub enum Unop {
	Negate(Box<Node<Expr>>),
	ReadFile(Box<Node<Expr>>),
	Ordered(Box<Node<Expr>>),
	OrderedStrictly(Box<Node<Expr>>),
	Length(Box<Node<Expr>>),
}

pub enum Chop {
	Plus(Node<Expr>),
	Minus(Node<Expr>),
	Star(Node<Expr>),
	Slash(Node<Expr>),
	Comma(Node<Expr>),
	DoubleComma(Node<Expr>),
	Index(Node<Expr>),
	ToRight(Node<Expr>),
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
	fn tree(&self, _loc: &Loc) -> StringTree {
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
			Chop::Index(expr_node) => StringTree::new_node(
				"chop dot".to_string(),
				styles::NORMAL,
				vec![StringTree::from(expr_node)],
			),
		}
	}
}

impl Treeable for Expr {
	fn tree(&self, _loc: &Loc) -> StringTree {
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
			Expr::Input => StringTree::new_leaf("input".to_string(), styles::NORMAL),
			Expr::Context => StringTree::new_leaf("context".to_string(), styles::NORMAL),
			Expr::Unop(Unop::Negate(expr)) => StringTree::new_node(
				"unary minus".to_string(),
				styles::NORMAL,
				vec![StringTree::from(&**expr)],
			),
			Expr::Unop(Unop::ReadFile(expr)) => StringTree::new_node(
				"unary read file".to_string(),
				styles::NORMAL,
				vec![StringTree::from(&**expr)],
			),
			Expr::Unop(Unop::Ordered(expr)) => StringTree::new_node(
				"unary ordered".to_string(),
				styles::NORMAL,
				vec![StringTree::from(&**expr)],
			),
			Expr::Unop(Unop::OrderedStrictly(expr)) => StringTree::new_node(
				"unary ordered strictly".to_string(),
				styles::NORMAL,
				vec![StringTree::from(&**expr)],
			),
			Expr::Unop(Unop::Length(expr)) => StringTree::new_node(
				"unary length".to_string(),
				styles::NORMAL,
				vec![StringTree::from(&**expr)],
			),
			Expr::Chain { init, chops } => StringTree::new_node(
				"chain".to_string(),
				styles::BLUE,
				std::iter::once(StringTree::from(&**init))
					.chain(chops.iter().map(StringTree::from))
					.collect(),
			),
			Expr::Invalid { error_expr } => StringTree::new_node(
				"invalid".to_string(),
				styles::BOLD_LIGHT_RED,
				vec![StringTree::from(&**error_expr)],
			),
		}
	}
}

impl Treeable for TargetExpr {
	fn tree(&self, _loc: &Loc) -> StringTree {
		match self {
			TargetExpr::VariableName(name) => {
				StringTree::new_leaf(format!("target variable {}", name), styles::NORMAL)
			},
			TargetExpr::DeclVariableName(name) => {
				StringTree::new_leaf(format!("target declare variable {}", name), styles::NORMAL)
			},
			TargetExpr::Invalid => {
				StringTree::new_leaf("invalid".to_string(), styles::BOLD_LIGHT_RED)
			}, // TODO
		}
	}
}

impl Treeable for Stmt {
	fn tree(&self, _loc: &Loc) -> StringTree {
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
			Stmt::Do { expr, wi_expr } => StringTree::new_node(
				"do".to_string(),
				styles::NORMAL,
				vec![
					StringTree::from(expr),
					if let Some(wi_expr) = wi_expr {
						StringTree::from(wi_expr)
					} else {
						StringTree::new_leaf("no interceptor".to_string(), styles::NORMAL)
					},
				],
			),
			Stmt::DoHere { expr } => StringTree::new_node(
				"do here".to_string(),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Stmt::If { cond_expr, th_stmts, el_stmts } => StringTree::new_node(
				"if".to_string(),
				styles::NORMAL,
				vec![
					StringTree::from(cond_expr),
					if !th_stmts.is_empty() {
						StringTree::new_node(
							"then branch".to_string(),
							styles::NORMAL,
							th_stmts.iter().map(StringTree::from).collect(),
						)
					} else {
						StringTree::new_leaf("no then branch".to_string(), styles::NORMAL)
					},
					if !el_stmts.is_empty() {
						StringTree::new_node(
							"else branch".to_string(),
							styles::NORMAL,
							el_stmts.iter().map(StringTree::from).collect(),
						)
					} else {
						StringTree::new_leaf("no else branch".to_string(), styles::NORMAL)
					},
				],
			),
			Stmt::Loop { wh_exprs, bd_stmts, sp_stmts, ao_flag } => StringTree::new_node(
				"loop".to_string(),
				styles::NORMAL,
				[
					if !wh_exprs.is_empty() {
						StringTree::new_node(
							"while condition".to_string(),
							styles::NORMAL,
							wh_exprs.iter().map(StringTree::from).collect(),
						)
					} else {
						StringTree::new_leaf("no while condition".to_string(), styles::NORMAL)
					},
					if !bd_stmts.is_empty() {
						StringTree::new_node(
							"body".to_string(),
							styles::NORMAL,
							bd_stmts.iter().map(StringTree::from).collect(),
						)
					} else {
						StringTree::new_leaf("no body".to_string(), styles::NORMAL)
					},
					if !sp_stmts.is_empty() {
						StringTree::new_node(
							"separator".to_string(),
							styles::NORMAL,
							sp_stmts.iter().map(StringTree::from).collect(),
						)
					} else {
						StringTree::new_leaf("no separator".to_string(), styles::NORMAL)
					},
				]
				.into_iter()
				.chain(if ao_flag.is_some() {
					Some(StringTree::new_leaf(
						"at least once".to_string(),
						styles::NORMAL,
					))
				} else {
					None
				})
				.collect(),
			),
			Stmt::RegisterInterceptor { expr } => StringTree::new_node(
				"register interceptor".to_string(),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Stmt::Emit { expr, target } => StringTree::new_node(
				"emit".to_string(),
				styles::NORMAL,
				vec![
					StringTree::from(expr),
					if let Some(target_expr) = target {
						StringTree::from(target_expr)
					} else {
						StringTree::new_leaf("no target".to_string(), styles::NORMAL)
					},
				],
			),
			Stmt::DeployContext { expr } => StringTree::new_node(
				"deploy context".to_string(),
				styles::NORMAL,
				vec![StringTree::from(expr)],
			),
			Stmt::GenericSyntax { expr, ar_exprs, target } => StringTree::new_node(
				"generic syntax".to_string(),
				styles::NORMAL,
				vec![
					StringTree::from(expr),
					if !ar_exprs.is_empty() {
						StringTree::new_node(
							"arguments".to_string(),
							styles::NORMAL,
							ar_exprs.iter().map(StringTree::from).collect(),
						)
					} else {
						StringTree::new_leaf("no arguments".to_string(), styles::NORMAL)
					},
					if let Some(target_expr) = target {
						StringTree::from(target_expr)
					} else {
						StringTree::new_leaf("no target".to_string(), styles::NORMAL)
					},
				],
			),
			Stmt::Invalid { error_expr } => StringTree::new_node(
				"invalid".to_string(),
				styles::BOLD_LIGHT_RED,
				vec![StringTree::from(error_expr)],
			),
		}
	}
}

impl Treeable for Program {
	fn tree(&self, _loc: &Loc) -> StringTree {
		StringTree::new_node(
			"program".to_string(),
			styles::CYAN,
			self.stmts.iter().map(StringTree::from).collect(),
		)
	}
}

impl Node<Program> {
	pub fn print(&self) {
		// TODO: Clean this old wird stuff.

		pub struct DebugMem {
			pub log: IndentedLog,
		}

		impl DebugMem {
			pub fn new() -> DebugMem {
				DebugMem { log: IndentedLog::new() }
			}
		}

		let mut debug_mem = DebugMem::new();
		debug_mem
			.log
			.log_line("Program tree".to_string(), crate::utils::styles::NEGATIVE);
		crate::stringtree::StringTree::from(self).print(&mut debug_mem.log);
		debug_mem.log.print_to_stdout();
	}
}
