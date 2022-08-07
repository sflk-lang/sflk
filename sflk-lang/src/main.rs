mod ast;
mod bigint;
mod log;
mod log_indent;
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
	let settings = Settings::from_args();
	if settings.execute_wants() {
		return;
	}

	// Get the source code in memory.
	let scu = Rc::new(match &settings.src {
		Some(Source::FilePath(file_path)) => SourceCodeUnit::from_filename(file_path),
		Some(Source::Code(code)) => SourceCodeUnit::from_str(code.to_string(), "input".to_string()),
		None => panic!(),
	});

	// Get a tokenizer ready.
	let tfr = TokBuffer::from(CharReadingHead::from_scu(scu));

	// Don't execute any code, only display tokens
	if settings.display_tokens() {
		tfr.display_all(settings.debug_lines());
		return;
	}

	// Parse the source code into an AST.
	let parser_logger = settings.parser_debugging_logger();
	let mut parser = Parser::new(tfr, parser_logger);

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
