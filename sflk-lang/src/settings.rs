use crate::{log_indent::IndentedLogger, parser::ParserDebuggingLogger};

use std::env;

const HELP_MESSAGE: &str = "\
	Usage:\n\
	\tsflk [filename.sflk] [options]\n\
	\tsflk [-c \"some SFLK source code\"] [options]\n\
	\n\
	Options:\n\
	\t-h --help      Prints this help message\n\
	\t-v --version   Prints the interpreter version\n\
	\t-c --code      Next argument is the source code to run\n\
	\t-d --debug     Turns on debug mode\n\
	\t   --tokens    Prints tokens and halts\n\
	\t   --lines     Prints line numbers in some debug logs\n\
	\t   --actions   Prints actions in parsing debug logs\n\
	\t   --sir       Prints the SIR code before running\n\
	\n\
	Examples:\n\
	\tsflk tests/test6.sflk\n\
	\tsflk -c \"pr 8 nl\"\n\
	";

const NO_WARRANTY_NOTE: &str = "\
	Please note that there is NO warranty, \
	not even for MERCHANTABILITY or \
	FITNESS FOR A PARTICULAR PURPOSE.";

#[derive(Debug)]
pub struct Settings {
	path: String,
	pub src: Option<Source>,
	debug: bool,
	debug_lines: bool,
	debug_actions: bool,
	debug_sir: bool,
	wants_help: bool,
	wants_version: bool,
	display_tokens: bool,
}

#[derive(Debug)]
pub enum Source {
	FilePath(String),
	Code(String),
}

impl Settings {
	/// Creates a new `Settings` object filled with default values.
	///
	/// Everything is set as `None`, `false`, except `path` that it set from
	/// parameters
	pub fn new(path: String) -> Self {
		Self {
			path,
			src: None,
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

		enum Mode {
			Normal,
			SourceCode,
		}
		let mut mode = Mode::Normal;

		for arg in args {
			match mode {
				Mode::Normal => match arg.as_str() {
					"-h" | "--help" => {
						settings.wants_help = true;
					},
					"-v" | "--version" => {
						settings.wants_version = true;
					},
					"-c" | "--code" => {
						if settings.src.is_some() {
							panic!("Multiple source codes are given at the same time");
						}
						mode = Mode::SourceCode;
					},
					"-d" | "--debug" => {
						settings.debug = true;
					},
					"--tokens" => {
						settings.display_tokens = true;
					},
					"--lines" => {
						settings.debug_lines = true;
					},
					"--actions" => {
						settings.debug_actions = true;
					},
					"--sir" => {
						settings.debug_sir = true;
					},
					arg => {
						if settings.src.is_none() {
							settings.src = Some(Source::FilePath(arg.to_string()));
						} else {
							panic!(
								"Unknown command line argument `{}` \
								(it cannot be the source code file path because \
								a source code was already provided)",
								arg
							);
						}
					},
				},
				Mode::SourceCode => {
					if settings.src.is_none() {
						settings.src = Some(Source::Code(arg.to_string()));
					} else {
						panic!();
					}
					mode = Mode::Normal;
				},
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
				0, 3, 0, version_name
			);
			println!("{}", NO_WARRANTY_NOTE);

			did_something = true;
		}

		if self.wants_help {
			println!("{}", HELP_MESSAGE);
			did_something = true;
		}

		if self.src.is_none() {
			if !did_something {
				println!(
					"No source code provided, nothing to do. Try `{} --help` for usage.",
					self.path
				);
			}
			return true;
		}

		false
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
