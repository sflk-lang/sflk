
pub type Style = (&'static str, &'static str);

pub mod styles {
	pub const NORMAL: super::Style = ("", "");
	pub const CYAN: super::Style = ("\x1b[36m", "\x1b[39m");
	pub const UNDERLINE: super::Style = ("\x1b[4m", "\x1b[24m");
	pub const BOLD_LIGHT_RED: super::Style = ("\x1b[91m\x1b[1m", "\x1b[22m\x1b[39m");
}


pub fn escape_string(string: &str, escape_style: &Style) -> String {
	let mut ret = String::new();
	string.chars().for_each(|ch| match ch {
		'\"'   => ret.extend(format!("{}\\\"{}", escape_style.0, escape_style.1).chars()),
		'\\'   => ret.extend(format!("{}\\\\{}", escape_style.0, escape_style.1).chars()),
		'\n'   => ret.extend(format!("{}\\n{}",  escape_style.0, escape_style.1).chars()),
		'\t'   => ret.extend(format!("{}\\t{}",  escape_style.0, escape_style.1).chars()),
		'\x1b' => ret.extend(format!("{}\\e{}",  escape_style.0, escape_style.1).chars()),
		'\x07' => ret.extend(format!("{}\\a{}",  escape_style.0, escape_style.1).chars()),
		'\x08' => ret.extend(format!("{}\\b{}",  escape_style.0, escape_style.1).chars()),
		'\x0b' => ret.extend(format!("{}\\v{}",  escape_style.0, escape_style.1).chars()),
		'\x0c' => ret.extend(format!("{}\\f{}",  escape_style.0, escape_style.1).chars()),
		'\r'   => ret.extend(format!("{}\\r{}",  escape_style.0, escape_style.1).chars()),
		ch if (ch as u32) < (' ' as u32) =>
			ret.extend(format!("{}\\x{:02x}{}",
				escape_style.0, ch as u32, escape_style.1).chars()),
		ch => ret.push(ch),
	});
	ret
}
