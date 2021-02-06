
use crate::utils::Style;


pub struct StringTree {
	string: String,
	style: Style,
	sub_trees: Vec<StringTree>,
}

impl StringTree {
	pub fn new_leaf(string: String, style: Style) -> StringTree {
		StringTree {
			string,
			style,
			sub_trees: Vec::new(),
		}
	}

	pub fn new_node(string: String, style: Style, sub_trees: Vec<StringTree>) -> StringTree {
		StringTree {
			string,
			style,
			sub_trees,
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

impl StringTree {
	pub fn print(&self) {
		self.print_aux(&mut Vec::new(), false);
	}

	fn print_aux(&self, indent_styles: &mut Vec<(Style, Tube)>, is_last: bool) {
		// Print self.string with multiple line string support
		let mut lines = self.string.lines();
		if let Some(line) = lines.next() {
			print_indents(indent_styles, RightTube::from_is_last(is_last));
			println!("{}{}{}", self.style.0, line, self.style.1);
		}
		for line in lines {
			print_indents(indent_styles, RightTube::Tube);
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

fn print_indents(indent_styles: &Vec<(Style, Tube)>, right_override: RightTube) {
	if let Some(((indent_right_style, _), indents_left)) = indent_styles.split_last() {
		for (style, tube) in indents_left {
			print!("{}{}{}", style.0, tube.str(), style.1);
		}
		print!("{}{}{}", indent_right_style.0, right_override.str(), indent_right_style.1);
	}
}
