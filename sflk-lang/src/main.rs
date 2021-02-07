
mod tokenizer;
mod parser;
mod program;
mod object;
mod machine;
mod stringtree;
mod log;
mod utils;


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
			if arg == "-d" || arg == "--debug" {
				debug_mode = true;
			} else if arg == "-v" || arg == "--version" {
				println!("SFLK reference interpreter, version {}.{}.{} ({})",
					0, 1, 0, "indev");
				println!("Please note that there is NO warranty, \
					not even for MERCHANTABILITY or \
					FITNESS FOR A PARTICULAR PURPOSE.");
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


fn main() {
	let settings = Settings::from_args();

	let mut mem = machine::Mem::new(settings.debug_mode);
	mem.exec_root_file(settings.src_filename);
	if let Some(string_rtlog) =  mem.debug_mode {
		string_rtlog.print();
	}
}
