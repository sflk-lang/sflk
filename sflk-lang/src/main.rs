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

use std::collections::VecDeque;
use std::rc::Rc;

fn main() {
	let mut logger = Logger { enabled: true };

	let src = {
		match std::env::args().nth(1) {
			Some(arg) if arg == "-c" => {
				let src_as_string = std::env::args()
					.nth(2)
					.expect("Expect source code in second command line argument");
				Rc::new(SourceCode::new(src_as_string, "input".to_string()))
			},
			Some(arg) if arg == "-f" => {
				let src_file_path = std::env::args()
					.nth(2)
					.expect("Expect source file path in second command line argument");
				let src_as_string = std::fs::read_to_string(&src_file_path)
					.unwrap_or_else(|_| panic!("Could not read source file \"{src_file_path}\""));
				Rc::new(SourceCode::new(src_as_string, src_file_path))
			},
			Some(unknown_arg) => panic!("Unknown first argument \"{unknown_arg}\""),
			None => panic!("Expected arguments"),
		}
	};

	logger.println("\x1b[1mSOURCE CODE:\x1b[22m");
	logger.print_src(Rc::clone(&src));

	let mut tokenizer = TokenStreamPeekable {
		token_stream: TokenStream {
			src_char_stream: SourceCodeCharacterStream::new(src),
		},
		tokens_ahead: VecDeque::new(),
	};

	logger.println("\x1b[1mTOKENS:\x1b[22m");
	{
		let mut i = 0;
		loop {
			let (token, loc) = tokenizer.peek(i);
			logger.println(&format!("token {}", token));
			if matches!(token, Token::EndOfFile) {
				break;
			}
			logger.print_loc(loc.clone());
			i += 1;
		}
	}

	let mut parser = Parser { tokenizer };
	let program_ast = parser.pop_full_program();

	logger.println("\x1b[1mSTATEMENTS:\x1b[22m");
	for statement in program_ast.statements.iter() {
		logger.print_loc(statement.loc());
	}

	let program_block = program_to_boc(&program_ast);

	logger.println("\x1b[1mEXECUTION:\x1b[22m");
	let mut machine = Machine {
		execution: Execution {
			execution_frame_stack: vec![ExecutionFrame {
				instruction_index: 0,
				block_of_code: program_block,
				temporary_value_stack: Vec::new(),
			}],
		},
	};
	while !machine.execution.execution_frame_stack.is_empty() {
		machine.execution.perform_one_step();
	}

	// TODO: Make user-friendly error reporting, not leaving any case.
	// TODO: Make a `Logger` (that will supports everything), and make every step use it.
	// TODO: Serialize blocks of code to bytes.
}

/// Represents a piece of source code that is given to the interpreter,
/// such as directly via command line or in a source file.
///
/// The source code remians useful even after parsing (for error reporting)
/// and stays around via `Rc` pointers (so as to not clone the potentially
/// big source code potentially many times).
struct SourceCode {
	content: String,
	/// The byte index of the first character of each line.
	/// Note that line indices are used to index lines (and not line numbers that start at 1).
	line_start_byte_indices: Vec<u32>,
	name: String,
}

impl SourceCode {
	fn new(content: String, name: String) -> SourceCode {
		let line_start_byte_indices = {
			// All the lines start on the character that follows a '\n',
			// except the first line that starts on the first character.
			// A '\n' character does not always indicate a line start though,
			// as we decide not to ignore the last '\n' if it is the last character.

			let mut byte_index = 0;
			let mut line_start_byte_indices: Vec<u32> = vec![0];
			let mut a_line_just_ended = false;
			loop {
				let ch_opt = content[byte_index..].chars().next();
				match ch_opt {
					Some(ch) => {
						if a_line_just_ended {
							// Previous character was a '\n',
							// which means the current character starts a line.
							line_start_byte_indices.push(byte_index as u32);
							a_line_just_ended = false;
						}
						if ch == '\n' {
							// Next character (if any) will be a line start.
							a_line_just_ended = true;
						}
						byte_index += ch.len_utf8();
					},
					None => break,
				}
			}
			line_start_byte_indices
		};
		SourceCode { content, line_start_byte_indices, name }
	}

	fn line_with_terminating_newline(&self, line_index: u32) -> &str {
		let line_index = line_index as usize;
		let start_byte_index = self.line_start_byte_indices[line_index] as usize;
		let is_last_line = line_index == self.line_count() - 1;
		if is_last_line {
			&self.content[start_byte_index..]
		} else {
			let next_start_byte_index = self.line_start_byte_indices[line_index + 1] as usize;
			&self.content[start_byte_index..next_start_byte_index]
		}
	}

	fn line(&self, line_index: u32) -> &str {
		let line = self.line_with_terminating_newline(line_index);
		if let Some(stripped_line) = line.strip_suffix('\n') {
			stripped_line
		} else {
			line
		}
	}

	fn line_count(&self) -> usize {
		self.line_start_byte_indices.len()
	}

	fn line_number_of_byte_index(&self, byte_index: u32) -> u32 {
		for (line_index, &line_start_byte_index) in self.line_start_byte_indices.iter().enumerate()
		{
			if line_start_byte_index > byte_index {
				// The previous line was the line in which is the character as the given byte index.
				let previous_line_index = line_index - 1;
				let previous_line_number = (previous_line_index + 1) as u32;
				return previous_line_number;
			}
		}
		let last_line_index = self.line_count() - 1;
		let last_line_number = (last_line_index + 1) as u32;
		#[allow(clippy::let_and_return)]
		last_line_number
	}
}

/// Represents an interval of some piece of source code.
#[derive(Clone)]
struct Location {
	src: Rc<SourceCode>,
	/// The line number of the line on which is the first character.
	line_number_start: u32,
	/// Included.
	byte_index_start: u32,
	/// Included.
	byte_index_end: u32,
}

impl Location {
	fn concat(mut self, rhs: Location) -> Location {
		self.concat_in_place(rhs);
		self
	}

	fn concat_in_place(&mut self, rhs: Location) {
		assert!(Rc::ptr_eq(&self.src, &rhs.src));
		self.byte_index_end = rhs.byte_index_end;
	}

	fn length_in_bytes(&self) -> usize {
		self.byte_index_end as usize + 1 - self.byte_index_start as usize
	}

	/// Split the `Location` into the biggest sub-`Location`s that do not cover multiple lines.
	///
	/// For example, in a code source of 3 lines of 3 characters each, a `Location` that covers
	/// the `#`s in the mask `--#\n###\n#--\n` (including the `\n`s that are inbetween `#`s) will
	/// be split into the `Location`s `--#\n---\n---\n`, `---\n###\n---\n` and `---\n---\n#--\n`
	/// (none of them covering any `\n`).
	fn split_across_lines(self) -> Vec<Location> {
		if self.byte_index_start == self.byte_index_end {
			// There is nothing to split in a one-byte-long `Location`.
			return vec![self];
		}
		let mut src =
			SourceCodeCharacterStream::starting_at(Rc::clone(&self.src), self.byte_index_start);
		let mut vec: Vec<Location> = Vec::new();
		loop {
			let (ch_opt, loc) = src.pop();
			if self.byte_index_end < loc.byte_index_start {
				break;
			}
			if ch_opt == Some('\n') {
				// Do not cover in any resulting `Location` (as said in the function doc).
			} else if vec.last().is_none()
				|| vec.last().unwrap().byte_index_end + 1 < loc.byte_index_start
			{
				// The current character is the first or does not touch the last character
				// of the current result `Location`, so we start a new `Location` in the
				// results (we just got acorss a '\n').
				vec.push(loc);
			} else {
				// The current character touches the current result `Location` and it is
				// not a '\n' so we can add it to the current `Location`.
				vec.last_mut().unwrap().concat_in_place(loc);
			}
		}
		vec
	}
}

/// Character stream from some piece of source code.
/// Supports look-aheads.
struct SourceCodeCharacterStream {
	src: Rc<SourceCode>,
	byte_index_next: usize,
	line_number_next: u32,
}

impl SourceCodeCharacterStream {
	fn new(src: Rc<SourceCode>) -> SourceCodeCharacterStream {
		SourceCodeCharacterStream { src, byte_index_next: 0, line_number_next: 1 }
	}

	fn starting_at(src: Rc<SourceCode>, byte_index_start: u32) -> SourceCodeCharacterStream {
		let line_number = src.line_number_of_byte_index(byte_index_start);
		SourceCodeCharacterStream {
			src,
			byte_index_next: byte_index_start as usize,
			line_number_next: line_number,
		}
	}

	fn advance(&mut self) {
		if let Some(ch) = self.peek(0) {
			self.byte_index_next += ch.len_utf8();
			if ch == '\n' {
				self.line_number_next += 1;
			}
		}
	}

	/// Returns the `n`-th next character, countring from zero.
	fn peek(&self, n: usize) -> Option<char> {
		self.src.content[self.byte_index_next..].chars().nth(n)
	}

	/// Pops the next character, returned with its location.
	fn pop(&mut self) -> (Option<char>, Location) {
		// The character that will be popped now is the "next" character.
		let loc = Location {
			src: Rc::clone(&self.src),
			line_number_start: self.line_number_next,
			byte_index_start: self.byte_index_next.try_into().unwrap(),
			byte_index_end: self.byte_index_next.try_into().unwrap(),
		};
		let ch_opt = self.peek(0);
		if ch_opt.is_some() {
			self.advance();
		}
		(ch_opt, loc)
	}

	/// Discards whitespace characters so that the next character is not whitespace.
	fn skip_whitespace(&mut self) {
		loop {
			match self.peek(0) {
				Some(ch) if ch.is_ascii_whitespace() => self.advance(),
				_ => break,
			}
		}
	}
}

enum Token {
	KeywordPr,
	KeywordDh,
	OpenCurly,
	CloseCurly,
	IntegerLiteral {
		value: u32,
	},
	UnexpectedChar {
		ch: char,
	},
	/// This token just marks the end of the source code
	/// even if that source code is not in a file.
	EndOfFile,
	/// Temporary (TODO: remove).
	OTHER,
}

impl std::fmt::Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Token::KeywordPr => write!(f, "keyword pr"),
			Token::KeywordDh => write!(f, "keyword dh"),
			Token::OpenCurly => write!(f, "open curly"),
			Token::CloseCurly => write!(f, "close curly"),
			Token::IntegerLiteral { value } => write!(f, "integer literal {value}"),
			Token::UnexpectedChar { ch } => write!(f, "unexpected character {ch}"),
			Token::EndOfFile => write!(f, "end-of-file"),
			Token::OTHER => write!(f, "some token"),
		}
	}
}

/// Token stream from some piece of source code.
/// Consumes characters from a character stream to produce tokens on demend,
/// no look-aheads here (see `TokenStreamPeekable` for look-aheads).
struct TokenStream {
	src_char_stream: SourceCodeCharacterStream,
}

impl TokenStream {
	fn pop(&mut self) -> (Token, Location) {
		self.src_char_stream.skip_whitespace();
		let (first_ch, first_loc) = self.src_char_stream.pop();
		match first_ch {
			Some('p') => {
				let (second_ch, second_loc) = self.src_char_stream.pop();
				match second_ch {
					Some('r') => (Token::KeywordPr, first_loc.concat(second_loc)),
					_ => (Token::OTHER, first_loc.concat(second_loc)),
				}
			},
			Some('d') => {
				let (second_ch, second_loc) = self.src_char_stream.pop();
				match second_ch {
					Some('h') => (Token::KeywordDh, first_loc.concat(second_loc)),
					_ => (Token::OTHER, first_loc.concat(second_loc)),
				}
			},
			Some('{') => (Token::OpenCurly, first_loc),
			Some('}') => (Token::CloseCurly, first_loc),
			Some(ch @ '0'..='9') => {
				let value = ch as u32 - '0' as u32;
				(Token::IntegerLiteral { value }, first_loc)
			},
			Some(ch) => (Token::UnexpectedChar { ch }, first_loc),
			None => (Token::EndOfFile, first_loc),
		}
	}
}

/// Wrapper around a `TokenStream` that supports look-aheads.
struct TokenStreamPeekable {
	token_stream: TokenStream,
	/// Tokens popped from the `TokenStream` for look-aheads are stored here
	/// until popped for real.
	tokens_ahead: VecDeque<(Token, Location)>,
}

impl TokenStreamPeekable {
	fn peek(&mut self, n: usize) -> &(Token, Location) {
		while n >= self.tokens_ahead.len() {
			self.tokens_ahead.push_back(self.token_stream.pop())
		}
		&self.tokens_ahead[n]
	}

	fn peek_token(&mut self, n: usize) -> &Token {
		&self.peek(n).0
	}

	fn pop(&mut self) -> (Token, Location) {
		self.tokens_ahead
			.pop_front()
			.unwrap_or_else(|| self.token_stream.pop())
	}
}

struct Parser {
	tokenizer: TokenStreamPeekable,
}

trait AstNode {
	fn loc(&self) -> Location;
}

struct IntegerLiteralExpression {
	loc: Location,
	value: u32,
}

impl AstNode for IntegerLiteralExpression {
	fn loc(&self) -> Location {
		self.loc.clone()
	}
}

struct BlockOfCodeLiteralExpression {
	open_curly_loc: Location,
	close_curly_loc: Location,
	program: Program,
}

impl AstNode for BlockOfCodeLiteralExpression {
	fn loc(&self) -> Location {
		self.open_curly_loc
			.clone()
			.concat(self.close_curly_loc.clone())
	}
}

enum Expression {
	IntegerLiteral(IntegerLiteralExpression),
	BlockOfCodeLiteral(BlockOfCodeLiteralExpression),
	ErrorUnexpectedToken(Token, Location),
}

impl AstNode for Expression {
	fn loc(&self) -> Location {
		match self {
			Expression::IntegerLiteral(integer_literal) => integer_literal.loc(),
			Expression::BlockOfCodeLiteral(boc_literal) => boc_literal.loc(),
			Expression::ErrorUnexpectedToken(_token, loc) => loc.clone(),
		}
	}
}

struct PrintStatement {
	kw_pr_loc: Location,
	expression: Expression,
}

impl AstNode for PrintStatement {
	fn loc(&self) -> Location {
		self.kw_pr_loc.clone().concat(self.expression.loc())
	}
}

struct DoHereStatement {
	kw_dh_loc: Location,
	expression: Expression,
}

impl AstNode for DoHereStatement {
	fn loc(&self) -> Location {
		self.kw_dh_loc.clone().concat(self.expression.loc())
	}
}

enum Statement {
	Print(PrintStatement),
	DoHere(DoHereStatement),
	ErrorUnexpectedToken(Token, Location),
}

impl AstNode for Statement {
	fn loc(&self) -> Location {
		match self {
			Statement::Print(print_statement) => print_statement.loc(),
			Statement::DoHere(do_here_statement) => do_here_statement.loc(),
			Statement::ErrorUnexpectedToken(_token, loc) => loc.clone(),
		}
	}
}

struct Program {
	statements: Vec<Statement>,
}

impl Parser {
	fn pop_full_program(&mut self) -> Program {
		self.pop_code(false).0
	}

	/// Returns some code and the `Location` of the token that comes after the code
	/// to mark its end (for example the location of the `}` for a block of code literal).
	fn pop_code(&mut self, is_block_of_code: bool) -> (Program, Location) {
		let mut statements = Vec::new();
		let termination_token_loc = loop {
			// Look ahead to see if the code being parsed ends here.
			// What marks the end of the code depends on weather we are parsing
			// - a full program (in which case the code parsed here is the full source code,
			//   ending on an end-of-file token)
			// - or just a block of code literal (like `{pr 4 pr 2}`)
			//   (in which case the code parsed here ends on a `}` token).
			if (is_block_of_code && matches!(self.tokenizer.peek_token(0), Token::CloseCurly))
				|| (!is_block_of_code && matches!(self.tokenizer.peek_token(0), Token::EndOfFile))
			{
				// Discard the token that marks the end of the code being parsed here.
				let (_token, loc) = self.tokenizer.pop();
				break loc;
			}

			// Code is just a sequence of statements.
			statements.push(self.pop_statement());
		};
		(Program { statements }, termination_token_loc)
	}

	fn pop_statement(&mut self) -> Statement {
		let (token, loc) = self.tokenizer.pop();
		match token {
			Token::KeywordPr => {
				let expression = self.pop_expression();
				Statement::Print(PrintStatement { expression, kw_pr_loc: loc })
			},
			Token::KeywordDh => {
				let expression = self.pop_expression();
				Statement::DoHere(DoHereStatement { expression, kw_dh_loc: loc })
			},
			unexpected_token => Statement::ErrorUnexpectedToken(unexpected_token, loc),
		}
	}

	fn pop_expression(&mut self) -> Expression {
		let (token, loc) = self.tokenizer.pop();
		match token {
			Token::IntegerLiteral { value } => {
				Expression::IntegerLiteral(IntegerLiteralExpression { value, loc })
			},
			Token::OpenCurly => {
				let (program, close_curly_loc) = self.pop_code(true);
				Expression::BlockOfCodeLiteral(BlockOfCodeLiteralExpression {
					open_curly_loc: loc,
					close_curly_loc,
					program,
				})
			},
			unexpected_token => Expression::ErrorUnexpectedToken(unexpected_token, loc),
		}
	}
}

#[derive(Clone)]
enum Instruction {
	/// Pushes the `constant_index`-th object of the `constants` table of the `BlockOfCode`
	/// on the temporary stack.
	///
	/// ( -- constant )
	PushConstant { constant_index: u32 },

	/// Pops a value,
	/// and prints some string representation of that value.
	///
	/// ( value -- )
	Print,

	/// Pops a value that is expected to be a block of code,
	/// and executes it in the current context.
	///
	/// ( boc -- )
	DoHere,

	/// Errors (temporary).
	Error,
}

#[derive(Clone)]
enum Object {
	Integer(u32),
	BlockOfCode(BlockOfCode),
}

#[derive(Clone)]
struct BlockOfCode {
	instructions: Vec<Instruction>,
	constants: Vec<Object>,
}

/// Represents a `BlockOfCode` that is not finished being constructed by the bytecode generation.
/// Code generation is actually handled by the `CodeGenerator` methods (what a surprise!).
struct CodeGenerator {
	boc: BlockOfCode,
}

impl CodeGenerator {
	fn add_constant_pushing_instruction_code(&mut self, constant: Object) {
		self.boc.constants.push(constant);
		let constant_index = (self.boc.constants.len() - 1) as u32;
		self.boc
			.instructions
			.push(Instruction::PushConstant { constant_index });
	}

	fn add_expression_evaluation_code(&mut self, expression: &Expression) {
		match expression {
			Expression::IntegerLiteral(IntegerLiteralExpression { value, .. }) => {
				self.add_constant_pushing_instruction_code(Object::Integer(*value));
			},
			Expression::BlockOfCodeLiteral(BlockOfCodeLiteralExpression { program, .. }) => {
				self.add_constant_pushing_instruction_code(Object::BlockOfCode(program_to_boc(
					program,
				)));
			},
			Expression::ErrorUnexpectedToken(..) => {
				self.boc.instructions.push(Instruction::Error);
			},
		}
	}

	fn add_statement_execution_code(&mut self, statement: &Statement) {
		match statement {
			Statement::Print(PrintStatement { expression, .. }) => {
				self.add_expression_evaluation_code(expression);
				self.boc.instructions.push(Instruction::Print);
			},
			Statement::DoHere(DoHereStatement { expression, .. }) => {
				self.add_expression_evaluation_code(expression);
				self.boc.instructions.push(Instruction::DoHere);
			},
			Statement::ErrorUnexpectedToken(..) => {
				self.boc.instructions.push(Instruction::Error);
			},
		}
	}

	fn construct(self) -> BlockOfCode {
		self.boc
	}
}

fn program_to_boc(program: &Program) -> BlockOfCode {
	let mut boc_in_construction = CodeGenerator {
		boc: BlockOfCode { instructions: Vec::new(), constants: Vec::new() },
	};
	for statement in &program.statements {
		boc_in_construction.add_statement_execution_code(statement);
	}
	boc_in_construction.construct()
}

struct ExecutionFrame {
	instruction_index: u32,
	block_of_code: BlockOfCode,
	temporary_value_stack: Vec<Object>,
}

struct Execution {
	execution_frame_stack: Vec<ExecutionFrame>,
}

struct Machine {
	execution: Execution,
}

impl Execution {
	fn top_frame(&self) -> &ExecutionFrame {
		self.execution_frame_stack.last().unwrap()
	}

	fn top_frame_mut(&mut self) -> &mut ExecutionFrame {
		self.execution_frame_stack.last_mut().unwrap()
	}

	fn perform_one_step(&mut self) {
		let instruction = match self
			.top_frame()
			.block_of_code
			.instructions
			.get(self.top_frame().instruction_index as usize)
		{
			Some(instruction) => instruction.clone(),
			None => {
				self.execution_frame_stack.pop();
				return;
			},
		};
		self.top_frame_mut().instruction_index += 1;
		match instruction {
			Instruction::PushConstant { constant_index } => {
				let value =
					self.top_frame().block_of_code.constants[constant_index as usize].clone();
				self.top_frame_mut().temporary_value_stack.push(value);
			},
			Instruction::Print => {
				let value = self.top_frame_mut().temporary_value_stack.pop().unwrap();
				match value {
					Object::Integer(value) => println!("{value}"),
					Object::BlockOfCode(_block_of_code) => todo!("print block of code"),
				}
			},
			Instruction::DoHere => {
				let value = self.top_frame_mut().temporary_value_stack.pop().unwrap();
				match value {
					Object::Integer(_value) => todo!("error"),
					Object::BlockOfCode(block_of_code) => {
						self.execution_frame_stack.push(ExecutionFrame {
							instruction_index: 0,
							block_of_code,
							temporary_value_stack: Vec::new(),
						})
					},
				}
			},
			Instruction::Error => panic!("error"),
		}
	}
}

struct Logger {
	enabled: bool,
}

fn is_strictly_sorted<T: std::cmp::PartialOrd>(vec: &[T]) -> bool {
	vec.windows(2).all(|window| window[0] < window[1])
}

impl Logger {
	fn println(&mut self, string: &str) {
		if !self.enabled {
			return;
		}
		println!("{}", string);
	}

	/// Prints a piece of the `src` source code.
	///
	/// If some `Location`s are provided via `locs_to_print`, then the printing will
	/// focus on these locations only instead of the whole source code.
	fn print_src_ex(&mut self, src: Rc<SourceCode>, locs_to_print: Option<&[Location]>) {
		if !self.enabled {
			return;
		}

		// If some `Location`s are to be focuced on, then it is a good thing to
		// break them and group them by lines so that we can examine each line at a time
		// to see which part of the line are covered by `Location`s to print.
		struct LineWithLocs {
			line_number: u32,
			locs: Vec<Location>,
		}
		let lwls_to_print = {
			let line_locs_to_print = match locs_to_print {
				Some(locs) => {
					let mut line_locs_to_print = Vec::new();
					for loc in locs.iter() {
						line_locs_to_print.append(&mut loc.clone().split_across_lines());
					}
					Some(line_locs_to_print)
				},
				None => None,
			};
			match line_locs_to_print {
				Some(locs) => {
					let mut lwls: Vec<LineWithLocs> = Vec::new();
					for line_loc in locs {
						let line_number = line_loc.line_number_start;
						if matches!(lwls.last(), Some(lwl) if lwl.line_number == line_number) {
							lwls.last_mut().unwrap().locs.push(line_loc);
						} else {
							lwls.push(LineWithLocs { line_number, locs: vec![line_loc] });
						}
					}
					Some(lwls)
				},
				None => None,
			}
		};

		let line_number_max = match &lwls_to_print {
			Some(lwls) => lwls.last().unwrap().line_number,
			None => src.line_count() as u32,
		};

		// To align the lines (to avoid situalitons like ```
		//  9 | pr "some line of code"
		//  10 | pr "line of code that is not aligned with the previous line"
		// ```) we have to know how many characters will take the longest line number
		// to add padding to all the smaller line numbers so that all lines
		// begin at the same column (like so ```
		//   9 | pr "some line of code (notice the padding before the '9')"
		//  10 | pr "line of code that is aligned with the previous line"
		// ```).
		let line_number_digit_max = line_number_max.to_string().len();

		let src_name = &src.name;
		self.println(&format!(
			"{nothing:line_number_digit_max$} ╭ {src_name}",
			nothing = ""
		));

		match lwls_to_print {
			None => {
				for line_number in 1..=line_number_max {
					let line_index = line_number - 1;
					let line = src.line(line_index);
					self.println(&format!("{line_number:line_number_digit_max$} │ {line}"));
				}
			},
			Some(lwls) => {
				for lwl in lwls {
					let line_number = lwl.line_number;
					let line_index = line_number - 1;
					let line = src.line(line_index);
					let line_start_byte_index = src.line_start_byte_indices[line_index as usize];

					let mut line_formatted = String::new();
					let mut current_byte_index_in_line = 0;
					for loc in lwl.locs {
						let byte_index_start_in_line =
							(loc.byte_index_start - line_start_byte_index) as usize;
						let byte_index_end_in_line =
							(loc.byte_index_end - line_start_byte_index) as usize;

						line_formatted +=
							&line[current_byte_index_in_line..byte_index_start_in_line];

						line_formatted += "\x1b[36m\x1b[1m";
						line_formatted += &line[byte_index_start_in_line..=byte_index_end_in_line];
						line_formatted += "\x1b[22m\x1b[39m";

						current_byte_index_in_line = byte_index_end_in_line + 1;
					}
					line_formatted += &line[current_byte_index_in_line..];

					self.println(&format!(
						"{line_number:line_number_digit_max$} │ {line_formatted}"
					));
				}
			},
		}
		
		self.println(&format!("{nothing:line_number_digit_max$} ╰", nothing = ""));
	}

	fn print_src(&mut self, src: Rc<SourceCode>) {
		self.print_src_ex(src, None);
	}

	fn print_loc(&mut self, loc: Location) {
		self.print_src_ex(Rc::clone(&loc.src), Some(&[loc]));
	}
}
