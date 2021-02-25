mod ast;
mod log;
mod machine;
mod object;
mod parser;
mod parser2;
mod program;
mod scu;
mod stringtree;
mod tokenizer;
mod utils;

const HELP_MESSAGE: &str = "\
	Usage:\n\
	\tsflk <filename.sflk> [options]\n\
	\n\
	Options:\n\
	\t-d --debug    Turns on debug mode\n\
	\t-h --help     Prints this help message\n\
	\t-v --version  Prints the interpreter version\n\
	";

const NO_WARRANTY_NOTE: &str = "\
	Please note that there is NO warranty, \
	not even for MERCHANTABILITY or \
	FITNESS FOR A PARTICULAR PURPOSE.";

struct Settings {
	path: String,
	root_filename: Option<String>,
	debug_mode: bool,
	wants_help: bool,
	wants_version: bool,
}

impl Settings {
	fn from_args() -> Settings {
		let mut args = std::env::args();
		let mut settings = Settings {
			path: args.next().unwrap_or(String::from("sflk")),
			root_filename: None,
			debug_mode: false,
			wants_help: false,
			wants_version: false,
		};
		for arg in args {
			if arg == "-d" || arg == "--debug" {
				settings.debug_mode = true;
			} else if arg == "-h" || arg == "--help" {
				settings.wants_help = true;
			} else if arg == "-v" || arg == "--version" {
				settings.wants_version = true;
			} else {
				if settings.root_filename.is_none() {
					settings.root_filename = Some(arg);
				} else {
					panic!("unknown command line argument `{}`", arg);
				}
			}
		}
		settings
	}
}

fn main() {
	let settings = Settings::from_args();

	let mut did_something = false;
	if settings.wants_version {
		println!(
			"SFLK reference interpreter, version {}.{}.{} ({})",
			0, 1, 0, "indev"
		);
		println!("{}", NO_WARRANTY_NOTE);
		did_something = true;
	}
	if settings.wants_help {
		print!("{}", HELP_MESSAGE);
		did_something = true;
	}
	if settings.root_filename.is_none() && !did_something {
		println!(
			"No filename provided, nothing to do. Try `{} --help` for usage.",
			settings.path
		);
	}

	let mut mem = machine::Mem::new(settings.debug_mode);
	/*
	if let Some(root_filename) = settings.root_filename {
		mem.exec_file(root_filename);
		if let Some(indented_log) = mem.debug_mode {
			indented_log.print();
		}
	}
	*/

	/*
	use crate::parser::ProgReadingHead;
	use crate::scu::SourceCodeUnit;
	use crate::stringtree::StringTree;
	use crate::tokenizer::TokReadingHead;
	use std::rc::Rc;

	let scu = Rc::new(SourceCodeUnit::from_filename(&filename));

	let mut prh = ProgReadingHead::from(TokReadingHead::from_scu(scu));
	let prog = match prh.parse_prog() {
		Ok(located_porg) => located_porg.unwrap(),
		Err(parsing_error) => {
			self.log_line(
				format!(
					"\x1b[91m\x1b[1mParsing error:\x1b[22m\x1b[39m {}",
					parsing_error
				),
				styles::NORMAL,
			);
			return;
		}
	};
	self.log_line(String::from("\x1b[7mProgram tree\x1b[27m"), styles::NORMAL);
	if let Some(_) = self.debug_mode {
		StringTree::from(&prog).print(self.debug_mode.as_mut().unwrap());
	}

	self.log_line(
		String::from("\x1b[7mProgram execution\x1b[27m"),
		styles::NORMAL,
	);
	self.exec_prog_here(&prog);
	self.log_line(String::from("\x1b[7mProgram end\x1b[27m"), styles::NORMAL); */

	let mut log = crate::log::IndentedLog::new();
	use crate::utils::styles;

	use crate::ast::Treeable;
	use crate::parser2::{Parser, TokForwardRh};
	use crate::scu::SourceCodeUnit;
	use crate::stringtree::StringTree;
	use crate::tokenizer::TokReadingHead;
	use std::rc::Rc;

	let scu = Rc::new(SourceCodeUnit::from_filename(
		&settings.root_filename.as_ref().unwrap(),
	));

	let mut tfr = TokForwardRh::from(TokReadingHead::from_scu(scu));
	let mut parser = Parser::new();
	let prog_node = match parser.parse_program(&mut tfr) {
		Ok(prog_node) => prog_node,
		Err(parsing_error) => {
			log.log_line(
				format!(
					"\x1b[91m\x1b[1mParsing error:\x1b[22m\x1b[39m {}",
					parsing_error
				),
				styles::NORMAL,
			);
			log.print();
			return;
		}
	};
	log.log_line(String::from("\x1b[7mProgram tree\x1b[27m"), styles::NORMAL);
	if settings.debug_mode {
		StringTree::from(&prog_node).print(&mut log);
	}

	let scu = Rc::new(SourceCodeUnit::from_filename(
		&settings.root_filename.unwrap().clone(),
	));
	let ast = match parser2::Parser::new().parse_program(&mut parser2::TokForwardRh::from(
		TokReadingHead::from_scu(scu),
	)) {
		Ok(ast) => ast,
		Err(err) => panic!("{}", err),
	};
	let mut log = crate::log::IndentedLog::new();
	let tree = StringTree::from(&ast);
	tree.print(&mut log);
	log.print();
}
