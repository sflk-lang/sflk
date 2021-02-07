
mod tokenizer;
mod parser;
mod program;
mod object;
mod machine;
mod stringtree;
mod log;
mod utils;


const HELP_MESSAGE: &str = 
	"Usage:\n\
	\tsflk <filename.sflk> [options]\n\
	\n\
	Options:\n\
	\t-d --debug    Turns on debug mode\n\
	\t-h --help     Prints this help message\n\
	\t-v --version  Prints the interpreter version\n\
	";

const NO_WARRANTY_NOTE: &str =
	"Please note that there is NO warranty, \
	not even for MERCHANTABILITY or \
	FITNESS FOR A PARTICULAR PURPOSE.";


struct Settings {
	root_filename: Option<String>,
	debug_mode: bool,
}

impl Settings {
	fn from_args() -> Settings {
		let mut args = std::env::args();
		args.next();
		let mut root_filename = None;
		let mut debug_mode = false;
		for arg in args {
			if arg == "-d" || arg == "--debug" {
				debug_mode = true;
			} else if arg == "-h" || arg == "--help" {
				print!("{}", HELP_MESSAGE);
			} else if arg == "-v" || arg == "--version" {
				println!("SFLK reference interpreter, version {}.{}.{} ({})",
					0, 1, 0, "indev");
				println!("{}", NO_WARRANTY_NOTE);
			} else {
				if root_filename.is_none() {
					root_filename = Some(arg);
				} else {
					panic!("unknown command line argument `{}`", arg);
				}
			}
		}
		Settings {
			root_filename,
			debug_mode,
		}
	}
}


fn main() {
	let settings = Settings::from_args();

	let mut mem = machine::Mem::new(settings.debug_mode);
	if let Some(root_filename) = settings.root_filename {
		mem.exec_file(root_filename);
		if let Some(indented_log) = mem.debug_mode {
			indented_log.print();
		}
	}
}
