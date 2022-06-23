use crate::{
	log_indent::IndentedLogger,
	parser::{Parser, ParserDebuggingLogger},
	scu::SourceCodeUnit,
	tokenizer::{CharReadingHead, TokBuffer},
};

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
pub struct Settings {
	path: String,
	pub root_filename: Option<String>,
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
	pub fn new(path: String) -> Self {
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
	pub fn from_args() -> Self {
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
					} else {
						panic!("Unknown command line argument `{}`", arg);
					}
				}
			}
		}

		settings
	}

	/// Executes some wants of the user set in parameters, like version, help
	///
	/// Also, checks for input file
	pub fn execute_wants(&self) -> bool {
		let mut did_something = false;
		
		if self.wants_version {
			let version_name = "indev";
			println!(
				"SFLK reference interpreter, version {}.{}.{} ({})",
				0, 2, 0, version_name
			);
			println!("{}", NO_WARRANTY_NOTE);

			did_something = true;
		}

		if self.wants_help {
			println!("{}", HELP_MESSAGE);
			did_something = true;
		}
		
		if self.root_filename.is_none() {
			if !did_something {
				println!(
					"No filename provided, nothing to do. Try `{} --help` for usage.",
					self.path
				);
			}
			return true;
		}

		return false;
	}

	pub fn parser_debugging_logger(&self) -> ParserDebuggingLogger {
		let mut logger: Option<IndentedLogger> = None;
		if self.debug {
			logger = Some(IndentedLogger::new());
		}

		ParserDebuggingLogger {
			logger,
			log_lines: self.debug_lines,
			log_actions: self.debug_actions,
			last_line: 0,
		}
	}

    pub fn display_tokens(&self) -> bool {
        self.display_tokens
    }

    pub fn debug_lines(&self) -> bool {
        self.debug_lines
    }

    pub fn debug(&self) -> bool {
        self.debug
    }

    pub fn debug_sir(&self) -> bool {
        self.debug_sir
    }
}
