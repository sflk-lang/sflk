use crate::{
	log::IndentedLog,
	parser::ParsingWarning,
	scu::Loc,
	stringtree::StringTree,
	utils::{escape_string, styles, Style},
};

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
		wi_expr: Option<Node<Expr>>,
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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Unop {
	Negate(Box<Node<Expr>>),
	ReadFile(Box<Node<Expr>>),
	Ordered(Box<Node<Expr>>),
	OrderedStrictly(Box<Node<Expr>>),
	Length(Box<Node<Expr>>),
}

#[derive(Debug)]
pub enum Chop {
	Plus(Node<Expr>),
	Minus(Node<Expr>),
	Star(Node<Expr>),
	Slash(Node<Expr>),
	Comma(Node<Expr>),
	DoubleComma(Node<Expr>),
	Index(Node<Expr>),
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
			Chop::Index(expr_node) => StringTree::new_node(
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
	fn tree(&self, loc: &Loc) -> StringTree {
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
			Stmt::DoFileHere { expr } => StringTree::new_node(
				"do file here".to_string(),
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
			Stmt::Invalid { error_expr } => StringTree::new_node(
				"invalid".to_string(),
				styles::BOLD_LIGHT_RED,
				vec![StringTree::from(error_expr)],
			),
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
			Stmt::Do { expr, wi_expr } => {
				expr.content.is_invalid()
					|| wi_expr
						.as_ref()
						.map_or(false, |expr| expr.content.is_invalid())
			},
			Stmt::DoHere { expr } => expr.content.is_invalid(),
			Stmt::DoFileHere { expr } => expr.content.is_invalid(),
			#[rustfmt::skip]
			Stmt::If { cond_expr, th_stmts, el_stmts } => {
				cond_expr.content.is_invalid()
				|| th_stmts
					.iter()
					.any(|stmt| (*stmt).content.is_invalid())
				|| el_stmts
					.iter()
					.any(|stmt| (*stmt).content.is_invalid())
			},
			#[rustfmt::skip]
			Stmt::Loop { wh_exprs, bd_stmts, sp_stmts, ao_flag: _ } => {
				wh_exprs
					.iter()
					.any(|expr| (*expr).content.is_invalid())
				|| bd_stmts
					.iter()
					.any(|stmt| (*stmt).content.is_invalid())
				|| sp_stmts
					.iter()
					.any(|stmt| (*stmt).content.is_invalid())
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
			Stmt::DeployContext { expr } => expr.content.is_invalid(),
			Stmt::Invalid { .. } => true,
		}
	}
}

impl TargetExpr {
	fn is_invalid(&self) -> bool {
		match self {
			TargetExpr::VariableName(_) => false,
			TargetExpr::DeclVariableName(_) => false,
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
			Expr::Context => false,
			Expr::Unop(_stmts) => false,
			Expr::Chain { init, chops } => {
				(*init).content.is_invalid()
					|| chops.iter().any(|chop| (*chop).content.is_invalid())
			},
			Expr::Invalid { .. } => true,
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
			Chop::Index(expr) => expr.content.is_invalid(),
			Chop::Invalid => true,
		}
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

		impl DebugMem {
			fn log_indent(&mut self, string: String, is_context: bool, style: Style) {
				self.log.indent(string, is_context, style);
			}

			fn log_deindent(&mut self) {
				self.log.deindent();
			}

			fn log_line(&mut self, string: String, style: Style) {
				self.log.log_line(string, style);
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
