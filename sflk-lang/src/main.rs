mod ast;
mod bignums;
mod log;
mod log_indent;
mod object;
mod parser;
mod scu;
mod settings;
mod sir;
mod stringtree;
mod tokenizer;
mod utils;

use parser::{Parser, ParserDebuggingLogger};
use scu::SourceCodeUnit;
use settings::Settings;
use settings::Source;
use tokenizer::{CharReadingHead, TokBuffer};

use std::rc::Rc;

fn main() {
	// Parse the command line arguments.
	let settings = Settings::from_args();
	settings.print_info();
	if settings.src_code().is_none() {
		return;
	}

	// Get the source code in memory.
	let scu = Rc::new(match settings.src_code() {
		Some(Source::FilePath(file_path)) => SourceCodeUnit::from_filename(file_path),
		Some(Source::Code(code)) => SourceCodeUnit::from_str(code.to_string(), "input".to_string()),
		None => panic!(),
	});

	// Get the tokenizer ready.
	let tfr = TokBuffer::from(CharReadingHead::from_scu(scu));

	if settings.display_tokens() {
		// Don't execute any code, only display tokens.
		tfr.display_all(settings.debug_lines());
		return;
	}

	// Get the parser ready.
	let parser_logger = settings.parser_debugging_logger();
	let mut parser = Parser::new(tfr, parser_logger);

	// Parse the source code into an AST.
	let ast = parser.parse_program();
	if settings.debug() {
		ast.print();
	}

	// Transform the AST into SIR code.
	let sir_block = sir::program_to_sir_block(ast.unwrap_ref());
	if settings.debug_sir() {
		dbg!(&sir_block);
	}

	// Actually run the code.
	sir::exec_sir_block(sir_block);
}
