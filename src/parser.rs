
use tokenizer::*;

#[derive(Debug)]
pub enum ParsingError {
	TokenizingError(TokenizingError),
	UnexpectedToken {tok: Tok, loc: Loc},
}

impl From<TokenizingError> for ParsingError {
	fn from(tokenizer_error: TokenizingError) -> ParsingError {
		ParsingError::TokenizingError(tokenizer_error)
	}
}

impl std::fmt::Display for ParsingError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			ParsingError::TokenizingError(parsing_error) => write!(f, "{}", parsing_error),
			ParsingError::UnexpectedToken {tok, loc} =>
					write!(f, "unexpected token `{}` at line {}",
						tok, loc.line()),
		}
	}
}

pub struct ProgReadingHead {
	trh: TokReadingHead,
	tok_buffer: Vec<(Tok, Loc)>,
}

impl From<TokReadingHead> for ProgReadingHead {
	fn from(trh: TokReadingHead) -> ProgReadingHead {
		ProgReadingHead {
			trh,
			tok_buffer: Vec::new(),
		}
	}
}

impl ProgReadingHead {
	fn read_tok_from_trh(&mut self) -> Result<(Tok, Loc), TokenizingError> {
		self.trh.read_cur_tok()
	}

	fn peek_tok(&mut self) -> Result<&(Tok, Loc), ParsingError> {
		if self.tok_buffer.is_empty() {
			let tok_loc = self.read_tok_from_trh()?;
			self.tok_buffer.push(tok_loc);
		}
		Ok(self.tok_buffer.last().unwrap())
	}

	fn disc_tok(&mut self) -> Result<(), ParsingError> {
		match self.tok_buffer.pop() {
			Some(_) => (),
			None => {
				let tok = self.read_tok_from_trh()?;
				if cfg!(debug_assertions) {
					println!("debug warning: \
						token {:?} discaded without having been peeked",
						tok);
				}
			},
		}
		Ok(())
	}

	fn pop_tok(&mut self) -> Result<(Tok, Loc), ParsingError> {
		if let Some(tok_loc) = self.tok_buffer.pop() {
			Ok(tok_loc)
		} else {
			Ok(self.read_tok_from_trh()?)
		}
	}
}

use crate::{machine::*, tokenizer};

/*
enum BlockParserEnd {
	Void,
	Curly,
}
*/

pub enum ExprParsingEnd {
	Nothing,
	Paren,
}

impl ProgReadingHead {
	/*
	fn parse_block_content(&mut self, parser_end: BlockParserEnd) -> Result<(Block, Loc), ParsingError> {
		let mut stmts: Vec<Stmt> = Vec::new();
		loop {
			let (tok, loc) = self.read_cur_tok()?;
		}
		Ok((Block::new(stmts), ???))
	}
	*/

	fn parse_expr_left(&mut self) -> Result<(Expr, Loc), ParsingError> {
		let (tok, loc) = self.pop_tok()?;
		match tok {
			//Tok::Word(word) => Ok((Expr::Var {varname: word}, loc)),
			Tok::Integer(integer) => Ok((Expr::Const {val:
				Obj::Integer(str::parse(&integer).expect("integer parsing error"))}, loc)),
			Tok::Left(left) if left == "(" => self.parse_expr(ExprParsingEnd::Paren),
			tok => Err(ParsingError::UnexpectedToken {tok, loc}),
		}
	}

	pub fn parse_expr(&mut self, parsing_end: ExprParsingEnd) -> Result<(Expr, Loc), ParsingError> {
		let (mut expr, mut loc) = self.parse_expr_left()?;
		while self.peek_tok()?.0.is_bin_op() {
			let (op, _) = self.pop_tok()?;
			match op {
				Tok::BinOp(op_string) => {
					let (right_expr, right_loc) = self.parse_expr_left()?;
					expr = Expr::BinOp {
						op: match &op_string[..] {
							"+" => Op::Plus,
							"-" => Op::Minus,
							_ => panic!("operator bad"),
						},
						left: Box::new(expr),
						right: Box::new(right_expr),
					};
					loc += right_loc;
				},
				_ => unreachable!(),
			}
		}
		match parsing_end {
			ExprParsingEnd::Nothing => Ok((expr, loc)),
			ExprParsingEnd::Paren => match self.pop_tok()? {
				(Tok::Right(s), _) if s == ")" => Ok((expr, loc)),
				(tok, loc) => Err(ParsingError::UnexpectedToken {loc, tok}),
			}
		}
		/*
		let mut expr = self.parse_expr_left()?;
		while self.peek()?.0.is_bin_op() {
			let op = self.peek()?.to_owned();
			self.disc()?;
			match op {
				Tok::OpPlus =>
					expr = Expr::Plus(Box::new(expr),
						Box::new(self.parse_expr_left()?)),
				Tok::OpMinus =>
					expr = Expr::Minus(Box::new(expr),
						Box::new(self.parse_expr_left()?)),
				_ => unreachable!(),
			}
		}
		match end {
			ExprEnd::None => (),
			ExprEnd::Paren => match self.pop()? {
				Tok::ParenRight => (),
				tok => return Err(ParsingError::UnexpectedToken(tok)),
			},
		}
		Ok(expr)
		*/
	}
}

