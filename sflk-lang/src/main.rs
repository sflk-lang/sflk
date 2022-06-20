mod ast;
mod log;
mod log_indent;
mod machine;
mod object;
mod parser;
mod program;
mod scu;
mod sir;
mod stringtree;
mod tokenizer;
mod utils;

use crate::{
	log_indent::IndentedLogger,
	parser::{Parser, ParserDebuggingLogger},
	scu::SourceCodeUnit,
	tokenizer::{CharReadingHead, TokBuffer},
};

use std::rc::Rc;
use std::env;

const HELP_MESSAGE: &str = "\
	Usage:\n\
	\tsflk [filename.sflk] [options]\n\
	\n\
	Options:\n\
	\t-d --debug     Turns on debug mode\n\
	\t-h --help      Prints this help message\n\
	\t-v --version   Prints the interpreter version\n\
	\t   --tokens    Prints tokens and halts\n\
	\t   --lines     Prints line numbers in some debug logs\n\
	\t   --actions   Prints actions in parsing debug logs\n\
	\t   --sir       Prints the SIR code before running\n\
	";

const NO_WARRANTY_NOTE: &str = "\
	Please note that there is NO warranty, \
	not even for MERCHANTABILITY or \
	FITNESS FOR A PARTICULAR PURPOSE.";

#[derive(Debug)]
struct Settings {
	path: String,
	root_filename: Option<String>,
	debug: bool,
	debug_lines: bool,
	debug_actions: bool,
	debug_sir: bool,
	wants_help: bool,
	wants_version: bool,
	display_tokens: bool,
}

impl Settings {
	/// Creates a new `Settings` object filled with default values.
	///
	/// Everything is set as `None`, `false`, except `path` that it set from
	/// parameters
	fn new(path: String) -> Self {
		Self {
			path,
			root_filename: None,
			debug: false,
			debug_lines: false,
			debug_actions: false,
			debug_sir: false,
			wants_help: false,
			wants_version: false,
			display_tokens: false,
		}
	}

	/// Retrieves command line arguments to create the settings according to 
	/// them
	fn from_args() -> Self {
		let mut args = env::args(); // retrieved command line arguments
		let mut settings = Self::new(args.next().unwrap_or_else(|| "sflk".to_string()));

		for arg in args {
			match arg.as_str() {
				"-d" | "--debug" => {
					settings.debug = true;
				}
				"-h" | "--help" => {
					settings.wants_help = true;
				}
				"-v" | "--version" => {
					settings.wants_version = true;
				}
				"--tokens" => {
					settings.display_tokens = true;
				}
				"--lines" => {
					settings.debug_lines = true;
				}
				"--actions" => {
					settings.debug_actions = true;
				}
				"--sir" => {
					settings.debug_sir = true;
				}
				arg => {
					if settings.root_filename.is_none() {
						settings.root_filename = Some(arg.to_string());
					}
					panic!("Unknown command line argument `{}`", arg);
				}
			}
		}

		println!("{:?}", settings);

		settings
	}
}

fn main() {
	let settings = Settings::from_args();

	// Print some messages such as help or version if asked for.
	let mut did_something = false;
	if settings.wants_version {
		let version_name = "indev";
		println!(
			"SFLK reference interpreter, version {}.{}.{} ({})",
			0, 2, 0, version_name
		);
		println!("{}", NO_WARRANTY_NOTE);
		did_something = true;
	}
	if settings.wants_help {
		println!("{}", HELP_MESSAGE);
		did_something = true;
	}
	if settings.root_filename.is_none() {
		if !did_something {
			println!(
				"No filename provided, nothing to do. Try `{} --help` for usage.",
				settings.path
			);
		}
		return;
	}

	// Get the source code in memory.
	let scu = Rc::new(SourceCodeUnit::from_filename(
		&settings.root_filename.unwrap(),
	));

	// Get a tokenizer ready.
	let tfr = TokBuffer::from(CharReadingHead::from_scu(scu));
	if settings.display_tokens {
		tfr.display_all(settings.debug_lines);
		return;
	}

	// Get a parser ready.
	let parser_logger = if settings.debug {
		ParserDebuggingLogger {
			logger: Some(IndentedLogger::new()),
			log_lines: settings.debug_lines,
			log_actions: settings.debug_actions,
			last_line: 0,
		}
	} else {
		ParserDebuggingLogger {
			logger: None,
			log_lines: settings.debug_lines,
			log_actions: settings.debug_actions,
			last_line: 0,
		}
	};
	let mut parser = Parser::new(tfr, parser_logger);

	// Parse the source code into an AST.
	let ast = parser.parse_program();
	if settings.debug {
		ast.print();
	}

	// Transform the AST into SIR code.
	let sir_block = sir::program_to_sir_block(ast.unwrap_ref());
	if settings.debug_sir {
		dbg!(&sir_block);
	}

	// Actually run the code.
	sir::exec_sir_block(sir_block);
}
