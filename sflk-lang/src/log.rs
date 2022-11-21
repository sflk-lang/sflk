use crate::utils::{styles, StdoutWriter, Style};

pub struct IndentedLog {
	items: Vec<Item>,
}

impl IndentedLog {
	pub fn new() -> IndentedLog {
		IndentedLog { items: Vec::new() }
	}

	fn push(&mut self, item: Item) {
		self.items.push(item);
	}

	#[allow(unused)]
	pub fn indent(&mut self, string: String, is_context: bool, style: Style) {
		assert!(!string.contains('\n'));
		self.push(Item::IndentAdd { string, indent: Indent { is_context, style } });
	}

	#[allow(unused)]
	pub fn deindent(&mut self) {
		self.push(Item::IndentRemove);
	}

	pub fn log_line(&mut self, string: String, style: Style) {
		assert!(!string.contains('\n'));
		self.push(Item::String { string, is_line: true, style });
	}

	pub fn log_string(&mut self, string: String, style: Style) {
		self.push(Item::String { string, is_line: false, style });
	}
}

impl std::fmt::Write for IndentedLog {
	fn write_str(&mut self, string: &str) -> Result<(), std::fmt::Error> {
		self.log_string(string.to_string(), styles::NORMAL);
		Ok(())
	}
}

impl IndentedLog {
	pub fn print_to_stdout(&self) {
		self.print(&mut StdoutWriter::new());
	}
}

#[derive(Debug)]
enum Item {
	IndentAdd { string: String, indent: Indent },
	IndentRemove,
	String { string: String, is_line: bool, style: Style },
}

#[derive(Debug, Clone)]
struct Indent {
	is_context: bool,
	style: Style,
}

const INDENT_START: &str = "┌";
const INDENT_NORMAL: &str = "│";
const INDENT_WEAK: &str = "╎";

impl IndentedLog {
	pub fn print(&self, writer: &mut impl std::fmt::Write) {
		let mut indents: Vec<Indent> = Vec::new();
		let mut is_newline: bool = true;
		for item in &self.items {
			match item {
				Item::IndentAdd { string, indent } => {
					print_indents(writer, &indents, Some(indent));
					writeln!(writer, "{}{}{}", indent.style.0, string, indent.style.1)
						.expect("TODO");
					is_newline = true;
					indents.push(indent.clone());
				},
				Item::IndentRemove => {
					indents.pop().expect("bug");
				},
				Item::String { string, is_line: true, style } => {
					if is_newline {
						print_indents(writer, &indents, None);
					}
					writeln!(writer, "{}{}{}", style.0, string, style.1).expect("TODO");
					is_newline = true;
				},
				Item::String { string, is_line: false, style } => {
					let formatted_string = string.to_string();
					let fragments: Vec<&str> = formatted_string.split('\n').collect();
					if let Some((end, lines)) = fragments.split_last() {
						for line in lines {
							if is_newline {
								print_indents(writer, &indents, None);
							}
							writeln!(writer, "{}{}{}", style.0, line, style.1).expect("TODO");
							is_newline = true;
						}
						if !end.is_empty() {
							if is_newline {
								print_indents(writer, &indents, None);
							}
							write!(writer, "{}{}{}", style.0, end, style.1).expect("TODO");
							is_newline = false;
						}
					}
				},
			}
		}
	}
}

fn print_indents(
	writer: &mut impl std::fmt::Write,
	indents: &[Indent],
	add_start: Option<&Indent>,
) {
	let last_cx_index = match add_start {
		Some(indent) if indent.is_context => indents.len(),
		_ => indents
			.iter()
			.rposition(|indent| indent.is_context)
			.unwrap_or(0),
	};
	for indent in indents[..last_cx_index].iter() {
		write!(
			writer,
			"{}{}{}",
			indent.style.0,
			if indent.is_context {
				INDENT_NORMAL
			} else {
				INDENT_WEAK
			},
			indent.style.1
		)
		.expect("TODO");
	}
	for indent in indents[last_cx_index..].iter() {
		write!(
			writer,
			"{}{}{}",
			indent.style.0, INDENT_NORMAL, indent.style.1
		)
		.expect("TODO");
	}
	if let Some(indent) = add_start {
		write!(
			writer,
			"{}{}{}",
			indent.style.0, INDENT_START, indent.style.1
		)
		.expect("TODO");
	}
}
