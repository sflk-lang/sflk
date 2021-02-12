use crate::scu::Loc;

// TODO:
// - move Loc here
// - delete Located
// - parser->program must become parser->ast->program

pub struct Node<T> {
	content: T,
	loc: Loc,
}

pub enum Stmt {
	Nop,
	Print {
		expr: Node<Expr>,
	},
	PrintNewline,
	Assign {
		target: Node<AssignTarget>,
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
		if_stmt: Box<Node<Stmt>>,
		el_opt_stmt: Option<Box<Node<Stmt>>>,
	},
}

pub enum AssignTarget {
	VariableName(String),
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
}

pub enum Chop {
	Plus(Node<Expr>),
	Minus(Node<Expr>),
	Star(Node<Expr>),
	Slash(Node<Expr>),
	ToRight(Node<Expr>),
}
