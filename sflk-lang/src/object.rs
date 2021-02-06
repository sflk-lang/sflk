
use crate::stringtree::StringTree;
use crate::program::Block;
use crate::utils::{escape_string, styles};


#[derive(Debug, Clone)]
pub enum Obj {
	Integer(isize),
	String(String),
	Block(Block),
	//Cx(Cx),  // Coming soon!
}

impl From<&Obj> for StringTree {
	fn from(obj: &Obj) -> StringTree {
		match obj {
			Obj::Integer(integer) => StringTree::new_leaf(
				format!("integer {}", integer), styles::NORMAL),
			Obj::String(string) => StringTree::new_leaf(
				format!("string \"{}\"", escape_string(&string, &styles::UNDERLINE)),
				styles::NORMAL),
			Obj::Block(block) => StringTree::from(block),
		}
	}
}

impl std::fmt::Display for Obj {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Obj::Integer(integer) => write!(f, "{}", integer),
			Obj::String(string) => write!(f, "\"{}\"", escape_string(&string, &styles::UNDERLINE)),
			Obj::Block(_) => write!(f, "{}", "block"), // TODO: Change this
			//Obj::Cx(cx) => write!(f, "{:?}", cx), // Coming soon!
		}
	}
}

impl Obj {
	pub fn plus(&mut self, other: Obj) {
		match (self, &other) {
			(Obj::Integer(val), Obj::Integer(right)) =>
				*val += right,
			(Obj::String(val), Obj::String(right)) =>
				*val += right.as_str(),
			(Obj::Block(val), Obj::Block(right)) =>
				val.stmts.extend(right.stmts.iter().cloned()),
			(obj_left, obj_right) => panic!("plus not yet supported between {} and {}",
				obj_left, obj_right),
		}
	}

	pub fn minus(&mut self, other: Obj) {
		match (self, other) {
			(Obj::Integer(val), Obj::Integer(right)) =>
				*val -= right,
			(obj_left, obj_right) => panic!("minus not yet supported between {} and {}",
				obj_left, obj_right),
		}
	}

	pub fn star(&mut self, other: Obj) {
		match (self, other) {
			(Obj::Integer(val), Obj::Integer(right)) =>
				*val *= right,
			(Obj::String(val), Obj::Integer(right)) =>
				*val = val.repeat(right as usize),
			(Obj::Block(val), Obj::Integer(right)) => 
				*val = val.clone_multiply(right as usize),
			(obj_left, obj_right) => panic!("star not yet supported between {} and {}",
				obj_left, obj_right),
		}
	}

	pub fn slash(&mut self, other: Obj) {
		match (&self, &other) {
			(Obj::Integer(val), Obj::Integer(right)) =>
				*self = Obj::Integer(val / right),
			(Obj::String(val), Obj::String(right)) =>
				*self = Obj::Integer(val.matches(right.as_str()).count() as isize),
			(obj_left, obj_right) => panic!("slash not yet supported between {} and {}",
				obj_left, obj_right),
		}
	}
}

impl Obj {
	pub fn as_cond(&self) -> bool {
		match self {
			Obj::Integer(integer) => *integer != 0,
			Obj::String(string) => string.len() != 0,
			Obj::Block(_) => true,
			//Obj::Cx(cx) => !cx.varmap.is_empty(),  // Coming soon!
		}
	}
}
