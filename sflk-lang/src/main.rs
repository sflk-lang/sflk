mod ast;
mod log;
mod log_indent;
mod parser;
mod scu;
mod sir;
mod stringtree;
mod tokenizer;
mod utils;

use crate::log_indent::IndentedLogger;
use crate::parser::{Parser, ParserDebuggingLogger};
use crate::scu::SourceCodeUnit;
use crate::tokenizer::{CharReadingHead, TokBuffer};
use std::rc::Rc;

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
	fn from_args() -> Settings {
		let mut args = std::env::args();
		let mut settings = Settings {
			path: args.next().unwrap_or_else(|| "sflk".to_string()),
			root_filename: None,
			debug: false,
			debug_lines: false,
			debug_actions: false,
			debug_sir: false,
			wants_help: false,
			wants_version: false,
			display_tokens: false,
		};
		for arg in args {
			if arg == "-d" || arg == "--debug" {
				settings.debug = true;
			} else if arg == "-h" || arg == "--help" {
				settings.wants_help = true;
			} else if arg == "-v" || arg == "--version" {
				settings.wants_version = true;
			} else if arg == "--tokens" {
				settings.display_tokens = true;
			} else if arg == "--lines" {
				settings.debug_lines = true;
			} else if arg == "--actions" {
				settings.debug_actions = true;
			} else if arg == "--sir" {
				settings.debug_sir = true;
			} else if settings.root_filename.is_none() {
				settings.root_filename = Some(arg);
			} else {
				panic!("Unknown command line argument `{}`", arg);
			}
		}
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
