
use crate::machine::*;

type StyleBegEnd = (&'static str, &'static str);

pub struct StringTree {
	main: String,
	style: StyleBegEnd,
	sub_trees: Vec<StringTree>,
}

const STYLE_NORMAL: StyleBegEnd = ("", "");
const STYLE_CYAN: StyleBegEnd = ("\x1b[36m", "\x1b[39m");
const STYLE_BOLD: StyleBegEnd = ("\x1b[4m", "\x1b[24m");

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
			Expr::Var {varname} => StringTree::new_leaf(
				format!("variable {}", varname), STYLE_NORMAL),
			Expr::Const {val} => StringTree::from(val),
			Expr::BinOp {op, left, right} => StringTree::new_node(
				format!("op {}", op), STYLE_NORMAL,
				vec![StringTree::from(&**left), StringTree::from(&**right)]),
			Expr::Chain {init_expr, chain_ops} => {
				// TODO
				// make this code great again
				let mut vec = vec![StringTree::from(&**init_expr)];
				vec.extend(chain_ops.iter().map(|chain_op| StringTree::from(chain_op)));
				StringTree::new_node(
					"chain".to_string(), STYLE_NORMAL,
					vec)
			},
		}
	}
}

impl From<&ChainOp> for StringTree {
	fn from(chain_op: &ChainOp) -> StringTree {
		StringTree::new_node(
			format!("op {}", chain_op.op), STYLE_NORMAL,
			vec![StringTree::from(&chain_op.expr)])
	}
}

fn escape(string: &str, style: &StyleBegEnd) -> String {
	let mut ret = String::new();
	string.chars().for_each(|ch| match ch {
		'\"' => ret.extend(format!("{}\\\"{}", style.0, style.1).chars()),
		'\\' => ret.extend(format!("{}\\\\{}", style.0, style.1).chars()),
		'\n' => ret.extend(format!("{}\\n{}", style.0, style.1).chars()),
		'\t' => ret.extend(format!("{}\\t{}", style.0, style.1).chars()),
		'\x1b' => ret.extend(format!("{}\\e{}", style.0, style.1).chars()),
		'\x07' => ret.extend(format!("{}\\a{}", style.0, style.1).chars()),
		'\x08' => ret.extend(format!("{}\\b{}", style.0, style.1).chars()),
		'\x0b' => ret.extend(format!("{}\\v{}", style.0, style.1).chars()),
		'\x0c' => ret.extend(format!("{}\\f{}", style.0, style.1).chars()),
		'\r' => ret.extend(format!("{}\\r{}", style.0, style.1).chars()),
		ch if (ch as u32) < (' ' as u32) =>
			ret.extend(format!("{}\\x{:02x}{}", style.0, ch as u32, style.1).chars()),
		ch => ret.push(ch),
	});
	ret
}

impl From<&Obj> for StringTree {
	fn from(obj: &Obj) -> StringTree {
		match obj {
			Obj::Integer(integer) => StringTree::new_leaf(
				format!("integer {}", integer), STYLE_NORMAL),
			Obj::String(string) => StringTree::new_leaf(
				format!("string \"{}\"", escape(&string, &STYLE_BOLD)), STYLE_NORMAL),
			Obj::Block(block) => StringTree::from(&**block),
		}
	}
}

impl From<&Block> for StringTree {
	fn from(block: &Block) -> StringTree {
		StringTree::new_node("block".to_owned(), STYLE_CYAN, 
			block.stmts.iter()
				.map(|stmt| StringTree::from(stmt))
				.collect()
		)
	}
}

impl From<&Stmt> for StringTree {
	fn from(stmt: &Stmt) -> StringTree {
		match stmt {
			Stmt::Nop => StringTree::new_leaf(
				"nop".to_owned(), STYLE_NORMAL),
			Stmt::Print {expr} => StringTree::new_node(
				"pr".to_owned(), STYLE_NORMAL,
				vec![StringTree::from(expr)]),
			Stmt::PrintNewline => StringTree::new_leaf(
				"nl".to_owned(), STYLE_NORMAL),
			Stmt::Assign {varname, expr} => StringTree::new_node(
				format!("assign to variable {}", varname), STYLE_NORMAL,
				vec![StringTree::from(expr)]),
			Stmt::AssignIfFree {varname, expr} => StringTree::new_node(
				format!("assign if free to variable {}", varname), STYLE_NORMAL,
				vec![StringTree::from(expr)]),
			Stmt::Do {expr} => StringTree::new_node(
				"do".to_owned(), STYLE_NORMAL,
				vec![StringTree::from(expr)]),
			Stmt::Imp {expr} => StringTree::new_node(
				"imp".to_owned(), STYLE_NORMAL,
				vec![StringTree::from(expr)]),
			Stmt::Exp {expr} => StringTree::new_node(
				"exp".to_owned(), STYLE_NORMAL,
				vec![StringTree::from(expr)]),
			Stmt::Redo {expr} => StringTree::new_node(
				"redo".to_owned(), STYLE_NORMAL,
				vec![StringTree::from(expr)]),
			Stmt::End {expr} => StringTree::new_node(
				"end".to_owned(), STYLE_NORMAL,
				vec![StringTree::from(expr)]),
			Stmt::If {cond_expr, stmt} => StringTree::new_node(
				"if".to_owned(), STYLE_NORMAL,
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
	fn str(&self) -> &'static str {
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
	fn str(&self) -> &'static str {
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
			print!("{}{}{}", style_beg, tube.str(), style_end);
		}
		print!("{}{}{}", indent_right_style.0, right_override.str(), indent_right_style.1);
	}
}

impl StringTree {
	pub fn print(&self) {
		self.print_aux(&mut Vec::new(), false);
	}

	fn print_aux(&self, indent_styles: &mut Vec<(StyleBegEnd, Tube)>, is_last: bool) {
		// Print self.main with multiple lines main support
		let mut lines = self.main.lines();
		if let Some(line) = lines.next() {
			print_indent(indent_styles, RightTube::from_is_last(is_last));
			println!("{}{}{}", self.style.0, line, self.style.1);
		}
		for line in lines {
			print_indent(indent_styles, RightTube::Tube);
			println!("{}{}{}", self.style.0, line, self.style.1);
		}

		// Manage the indentation changes and recusive printing
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
