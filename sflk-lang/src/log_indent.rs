use crate::utils::{styles, StdoutWriter, Style};

pub struct IndentedLogger {
	indents: Vec<Indent>,
	writer: Box<dyn std::fmt::Write>,
}

impl IndentedLogger {
	pub fn new() -> IndentedLogger {
		IndentedLogger {
			indents: Vec::new(),
			writer: Box::new(StdoutWriter::new()),
		}
	}

	pub fn for_writer(writer: Box<dyn std::fmt::Write>) -> IndentedLogger {
		IndentedLogger { indents: Vec::new(), writer }
	}

	pub fn indent(&mut self, string: &str, is_important: bool, style: Style) {
		// TODO: Make this more readable or something.
		let new_indent = Indent { is_important, style };
		let mut lines = string.lines();
		let first_line = lines.next().unwrap_or("");
		self.print_indents(Some(new_indent.clone()));
		writeln!(self.writer, "{}{}{}", style.0, first_line, style.1).unwrap();
		self.indents.push(new_indent);
		for line in lines {
			self.log_line(line, style);
		}
	}

	pub fn deindent(&mut self) {
		self.indents.pop().expect("bug: unmatched deindentation");
	}

	pub fn log_line(&mut self, line: &str, style: Style) {
		assert!(!line.contains('\n'));
		self.print_indents(None);
		writeln!(self.writer, "{}{}{}", style.0, line, style.1).unwrap();
	}

	pub fn log_string(&mut self, string: &str, style: Style) {
		for line in string.lines() {
			self.log_line(line, style);
		}
	}
}

impl std::fmt::Write for IndentedLogger {
	fn write_str(&mut self, string: &str) -> Result<(), std::fmt::Error> {
		self.log_string(string, styles::NORMAL);
		Ok(())
	}
}

#[derive(Debug, Clone)]
struct Indent {
	is_important: bool,
	style: Style,
}

const INDENT_START: &str = "┌";
const INDENT_NORMAL: &str = "│";
const INDENT_WEAK: &str = "╎";

impl IndentedLogger {
	fn print_indents(&mut self, new_indent: Option<Indent>) {
		let last_important_index = match &new_indent {
			Some(indent) if indent.is_important => self.indents.len(),
			_ => self
				.indents
				.iter()
				.rposition(|indent| indent.is_important)
				.unwrap_or(0),
		};
		for indent in self.indents[..last_important_index].iter() {
			write!(
				self.writer,
				"{}{}{}",
				indent.style.0,
				if indent.is_important {
					INDENT_NORMAL
				} else {
					INDENT_WEAK
				},
				indent.style.1
			)
			.expect("write failure");
		}
		for indent in self.indents[last_important_index..].iter() {
			write!(
				self.writer,
				"{}{}{}",
				indent.style.0, INDENT_NORMAL, indent.style.1
			)
			.expect("write failure");
		}
		if let Some(indent) = new_indent {
			write!(
				self.writer,
				"{}{}{}",
				indent.style.0, INDENT_START, indent.style.1
			)
			.expect("write failure");
		}
	}
}
