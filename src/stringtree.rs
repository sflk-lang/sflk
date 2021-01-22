
use crate::machine::*;

type StyleBegEnd = (&'static str, &'static str);

pub struct StringTree {
	main: String,
	style: StyleBegEnd,
	sub_trees: Vec<StringTree>,
}


impl StringTree {
	fn new_leaf(main: String, style: StyleBegEnd) -> StringTree {
		StringTree {
			main,
			style,
			sub_trees: Vec::new(),
		}
	}

	fn new_node(main: String, style: StyleBegEnd, sub_trees: Vec<StringTree>) -> StringTree {
		StringTree {
			main,
			style,
			sub_trees,
		}
	}
}

impl From<&Expr> for StringTree {
	fn from(expr: &Expr) -> StringTree {
		match expr {
			Expr::Var {varname} => StringTree::new_leaf(format!("variable {}", varname), ("", "")),
			Expr::Const {val} => StringTree::from(val),
			Expr::BinOp {op, left, right} => StringTree::new_node(
				format!("op {}", op), ("", ""),
				vec![StringTree::from(&**left), StringTree::from(&**right)]),
		}
	}
}

fn escape(string: &str) -> String {
	string
		.replace("\"", "\\\"")
}

impl From<&Obj> for StringTree {
	fn from(obj: &Obj) -> StringTree {
		match obj {
			Obj::Integer(integer) => StringTree::new_leaf(
				format!("integer {}", integer), ("", "")),
			Obj::String(string) => StringTree::new_leaf(
				format!("string \"{}\"", escape(&string)), ("", "")),
			Obj::Block(block) => StringTree::from(&**block),
		}
	}
}

impl From<&Block> for StringTree {
	fn from(block: &Block) -> StringTree {
		StringTree::new_node("block".to_owned(), ("", ""), block.stmts.iter()
			.map(|stmt| StringTree::from(stmt))
			.collect()
		)
	}
}

impl From<&Stmt> for StringTree {
	fn from(stmt: &Stmt) -> StringTree {
		match stmt {
			Stmt::Print {expr} => StringTree::new_node(
				"pr".to_owned(), ("", ""),
				vec![StringTree::from(expr)]),
			Stmt::Assign {varname, expr} => StringTree::new_node(
				format!("assign to variable {}", varname), ("", ""),
				vec![StringTree::from(expr)]),
			Stmt::AssignIfFree {varname, expr} => StringTree::new_node(
				format!("assign if free to variable {}", varname), ("", ""),
				vec![StringTree::from(expr)]),
			Stmt::Do {expr} => StringTree::new_node(
				"do".to_owned(), ("", ""),
				vec![StringTree::from(expr)]),
			Stmt::Imp {expr} => StringTree::new_node(
				"imp".to_owned(), ("", ""),
				vec![StringTree::from(expr)]),
			Stmt::Exp {expr} => StringTree::new_node(
				"exp".to_owned(), ("", ""),
				vec![StringTree::from(expr)]),
			Stmt::Redo {expr} => StringTree::new_node(
				"redo".to_owned(), ("", ""),
				vec![StringTree::from(expr)]),
			Stmt::End {expr} => StringTree::new_node(
				"end".to_owned(), ("", ""),
				vec![StringTree::from(expr)]),
			Stmt::If {cond_expr, stmt} => StringTree::new_node(
				"if".to_owned(), ("", ""),
				vec![StringTree::from(cond_expr), StringTree::from(&**stmt)]),
		}
	}
}

const INDENT_TUBE: &str = "│ ";
const INDENT_ITEM: &str = "├─";
const INDENT_LAST: &str = "└─";
const INDENT_NONE: &str = "  ";

enum Tube {
	Tube,
	None,
}

impl Tube {
	fn string(&self) -> &'static str {
		match self {
			Tube::Tube => INDENT_TUBE,
			Tube::None => INDENT_NONE,
		}
	}
}

enum RightTube {
	Tube,
	Item,
	Last,
}

impl RightTube {
	fn string(&self) -> &'static str {
		match self {
			RightTube::Tube => INDENT_TUBE,
			RightTube::Item => INDENT_ITEM,
			RightTube::Last => INDENT_LAST,
		}
	}

	fn from_is_last(is_last: bool) -> RightTube {
		match is_last {
			false => RightTube::Item,
			true => RightTube::Last,
		}
	}
}

fn print_indent(indents: &Vec<(StyleBegEnd, Tube)>, right_override: RightTube) {
	if let Some(((indent_right_style, _), indents_left)) = indents.split_last() {
		for ((style_beg, style_end), tube) in indents_left {
			print!("{}{}{}", style_beg, tube.string(), style_end);
		}
		print!("{}{}{}", indent_right_style.0, right_override.string(), indent_right_style.1);
	}
}

impl StringTree {
	pub fn print(&self) {
		self.print_aux(&mut Vec::new(), false);
	}

	fn print_aux(&self, indent_styles: &mut Vec<(StyleBegEnd, Tube)>, is_last: bool) {
		print_indent(indent_styles, RightTube::from_is_last(is_last));
		println!("{}{}{}", self.style.0, self.main, self.style.1);
		if is_last && !indent_styles.is_empty() {
			indent_styles.last_mut().unwrap().1 = Tube::None;
		}
		indent_styles.push((self.style, Tube::Tube));
		if let Some((last_sub_tree, sub_trees)) = self.sub_trees.split_last() {
			for sub_tree in sub_trees {
				sub_tree.print_aux(indent_styles, false);
			}
			last_sub_tree.print_aux(indent_styles, true);
		}
		indent_styles.pop();
	}
}
