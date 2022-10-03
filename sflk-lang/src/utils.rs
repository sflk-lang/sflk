pub type Style = (&'static str, &'static str);

pub mod styles {
	pub const NORMAL: super::Style = ("", "");
	pub const NEGATIVE: super::Style = ("\x1b[7m", "\x1b[27m");
	pub const CYAN: super::Style = ("\x1b[36m", "\x1b[39m");
	pub const YELLOW: super::Style = ("\x1b[33m", "\x1b[39m");
	pub const BLUE: super::Style = ("\x1b[34m", "\x1b[39m");
	pub const UNDERLINE: super::Style = ("\x1b[4m", "\x1b[24m");
	pub const BOLD_LIGHT_RED: super::Style = ("\x1b[91m\x1b[1m", "\x1b[22m\x1b[39m");
	pub const BOLD: super::Style = ("\x1b[1m", "\x1b[22m");
}

pub fn escape_string(string: &str, escape_style: &Style) -> String {
	let mut ret = String::new();
	string.chars().for_each(|ch| {
		if let Some(escaped) = escape_character(ch) {
			ret.extend(format!("{}{}{}", escape_style.0, escaped, escape_style.1).chars());
		} else {
			ret.push(ch);
		}
	});
	ret
}

fn escape_character(ch: char) -> Option<String> {
	match ch {
		// ASCII range escapes
		'\"' => Some(String::from("\\\"")),
		'\\' => Some("\\\\".to_string()),
		'\n' => Some("\\n".to_string()),
		'\t' => Some("\\t".to_string()),
		'\x1b' => Some("\\e".to_string()),
		'\x07' => Some("\\a".to_string()),
		'\x08' => Some("\\b".to_string()),
		'\x0b' => Some("\\v".to_string()),
		'\x0c' => Some("\\f".to_string()),
		'\r' => Some("\\r".to_string()),
		ch if (ch as u32) < (' ' as u32) => Some(format!("\\x{:02x}", ch as u32)),
		// Non-ASCII range espaces
		'�' => Some("\\?".to_string()),
		// Not to be escaped
		_ => None,
	}
}

pub struct StdoutWriter;

impl std::fmt::Write for StdoutWriter {
	fn write_str(&mut self, string: &str) -> Result<(), std::fmt::Error> {
		print!("{}", string);
		Ok(())
	}
}

impl StdoutWriter {
	pub fn new() -> StdoutWriter {
		StdoutWriter {}
	}
}
