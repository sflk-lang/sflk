
use crate::utils::Style;


pub struct StringRtlog {
	items: Vec<Item>,
}

impl StringRtlog {
	pub fn new() -> StringRtlog {
		StringRtlog {
			items: Vec::new(),
		}
	}

	fn push(&mut self, item: Item) {
		self.items.push(item);
	}

	pub fn indent(&mut self, string: String, is_context: bool, style: Style) {
		self.push(Item::IndentAdd {string, indent: Indent {is_context, style}});
	}

	pub fn deindent(&mut self) {
		self.push(Item::IndentRemove);
	}

	pub fn log(&mut self, string: String, style: Style) {
		self.push(Item::String(string, style));
	}
}

impl std::fmt::Write for StringRtlog {
	fn write_str(&mut self, string: &str) -> Result<(), std::fmt::Error> {
		self.push(Item::RawString(string.to_string()));
		Ok(())
	}
}


enum Item {
	IndentAdd {string: String, indent: Indent},
	IndentRemove,
	String(String, Style),
	RawString(String),
}

#[derive(Clone)]
struct Indent {
	is_context: bool,
	style: Style,
}


const INDENT_START: &str = "┌";
const INDENT_NORMAL: &str = "│";
const INDENT_WEAK: &str = "╎";


impl StringRtlog {
	pub fn print(&self) {
		let mut indents: Vec<Indent> = Vec::new();
		let mut is_newline: bool = true;
		for item in &self.items {
			match item {
				Item::IndentAdd {string, indent} => {
					if is_newline {
						print_indents(&indents, Some(indent));
					}
					println!("{}{}{}", indent.style.0, string, indent.style.1);
					is_newline = true;
					indents.push(indent.clone());
				},
				Item::IndentRemove => {
					indents.pop().expect("bug");
				},
				Item::String(string, style) => {
					if is_newline {
						print_indents(&indents, None);
					}
					println!("{}{}{}", style.0, string, style.1);
					is_newline = true;
				}
				Item::RawString(string) => {
					if is_newline {
						print_indents(&indents, None);
					}
					let formatted_string = format!("{}", string);
					is_newline = formatted_string.chars().last().map_or(false, |ch| ch == '\n');
					print!("{}", formatted_string);
				}
			}
		}
	}
}

fn print_indents(indents: &Vec<Indent>, add_start: Option<&Indent>) {
	let last_cx_index = match add_start {
		Some(indent) if indent.is_context => indents.len(),
		_ => indents.iter()
				.rposition(|indent| indent.is_context)
				.unwrap_or(0),
	};
	for indent in indents[..last_cx_index].iter() {
		print!("{}{}{}",
			indent.style.0,
			if indent.is_context {INDENT_NORMAL} else {INDENT_WEAK},
			indent.style.1);
	}
	for indent in indents[last_cx_index..].iter() {
		print!("{}{}{}", indent.style.0, INDENT_NORMAL, indent.style.1);
	}
	if let Some(indent) = add_start {
		print!("{}{}{}", indent.style.0, INDENT_START, indent.style.1);
	}
}
