/*
pub fn uwu() {
	println!("uwu");
}
*/

#[derive(Debug)]
pub struct SourceCodeUnit {
	name: String,
	content: String,
	line_offsets: Vec<usize>,
}

impl SourceCodeUnit {
	pub fn from_str(s: &str, name: String) -> SourceCodeUnit {
		let line_offsets_iter = s.bytes()
			.enumerate()
			.filter_map(|(i, ch)|
				if ch as char == '\n' {
					Some(i+1)
				} else {
					None 
				});
		let mut line_offsets: Vec<usize> = [0 as usize].iter().map(|&x| x)
			.chain(line_offsets_iter)
			.collect();
		let mut content = s.to_string();
		if *line_offsets.last().unwrap() != content.len() {
			content += "\n";
			line_offsets.push(content.len());
			// If the content didn't end by a `\n`, then now it does.
		}
		SourceCodeUnit {
			name: name,
			content: content,
			line_offsets: line_offsets,
		}
	}
}
