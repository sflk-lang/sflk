#[cfg(old)]
mod ast;
#[allow(unused)]
mod bignums;
#[cfg(old)]
mod log;
#[cfg(old)]
mod log_indent;
#[cfg(old)]
mod object;
#[cfg(old)]
mod parser;
#[cfg(old)]
mod scu;
#[cfg(old)]
mod settings;
#[cfg(old)]
mod sir;
#[cfg(old)]
mod stringtree;
#[cfg(old)]
mod tokenizer;
#[cfg(old)]
mod utils;

#[cfg(old)]
use parser::{Parser, ParserDebuggingLogger};
#[cfg(old)]
use scu::SourceCodeUnit;
#[cfg(old)]
use settings::Settings;
#[cfg(old)]
use settings::Source;
#[cfg(old)]
use tokenizer::{CharReadingHead, TokBuffer};

#[cfg(old)]
use std::rc::Rc;

#[cfg(old)]
fn main() {
	// Parse the command line arguments.
	let settings = Settings::from_args();
	settings.print_info();
	if settings.src.is_none() {
		return;
	}

	// Get the source code in memory.
	let scu = Rc::new(match settings.src {
		Some(Source::FilePath(ref file_path)) => SourceCodeUnit::from_filename(file_path),
		Some(Source::Code(ref code)) => {
			SourceCodeUnit::from_str(code.to_string(), "input".to_string())
		},
		None => panic!(),
	});

	// Get the tokenizer ready.
	let tfr = TokBuffer::from(CharReadingHead::from_scu(scu));

	if settings.display_tokens {
		// Don't execute any code, only display tokens.
		tfr.display_all(settings.debug_lines);
		return;
	}

	// Get the parser ready.
	let parser_logger = settings.parser_debugging_logger();
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

fn main() {
	println!("fresh start!");
}
