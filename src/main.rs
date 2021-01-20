
struct Settings {
	src_filename: String,
	debug_mode: bool,
}

impl Settings {
	fn from_args() -> Settings {
		let mut args = std::env::args();
		args.next();
		let src_filename = args.next().expect("no source file provided");
		let mut debug_mode = false;
		for arg in args {
			if arg == "-d" {
				debug_mode = true;
			} else {
				panic!("unknown command line argument `{}`", arg);
			}
		}
		Settings {
			src_filename,
			debug_mode,
		}
	}
}

use std::rc::Rc;
mod tokenizer;
mod parser;
mod machine;

fn main() -> Result<(), parser::ParsingError> {
	let settings = Settings::from_args();

	let scu = Rc::new(tokenizer::SourceCodeUnit::from_filename(
		&settings.src_filename));
	if settings.debug_mode {
		dbg!(&scu);
	}

	let mut prh = parser::ProgReadingHead::from(
		tokenizer::TokReadingHead::from_scu(Rc::clone(&scu)));
	let (prog, _) = prh.parse_prog()?;
	if settings.debug_mode {
		dbg!(&prog);
	}

	let mut mem = machine::Mem::new();
	mem.exec_prog(&prog);

	Ok(())
}
