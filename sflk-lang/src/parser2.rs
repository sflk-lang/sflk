// TODO:
// Remove the * from all uses
use crate::ast::*;
use crate::parser::*;
//use crate::program::*;
use crate::scu::*;
use crate::tokenizer::*;

// TODO:
//
// 1.
// Make a struct exactly like
// pub struct ProgReadingHead {
// 	trh: TokReadingHead,
// 	tok_buffer: Vec<(Tok, Loc)>,
// }
// but named TokStream
//
// 2.
// Make the Parser store the generated warnings
// (Note that errors are still returned by the Result returned objects)

struct TokForwardRh {
	trh: TokReadingHead,
	toks_ahead: Vec<(Tok, Loc)>,
}

impl TokForwardRh {
	fn from(trh: TokReadingHead) -> TokForwardRh {
		TokForwardRh {
			trh,
			toks_ahead: Vec::new(),
		}
	}

	fn peek(&mut self, n: usize) -> Result<&(Tok, Loc), ParsingError> {
		if self.toks_ahead.len() - 1 < n {
			self.toks_ahead.reserve(n - self.toks_ahead.len());
		}
		while self.toks_ahead.len() - 1 < n {
			self.toks_ahead.push(self.trh.read_cur_tok()?);
		}
		Ok(&self.toks_ahead[n])
	}

	fn pop(&mut self) -> Result<(Tok, Loc), ParsingError> {}
}

struct Parser {}

impl Parser {
	fn new() -> Parser {
		Parser {}
	}
}

impl Parser {
	fn parse_expr(&mut self, tok_stream: &mut TokForwardRh) -> Result<Node<Expr>, ParsingError> {
		todo!()
	}

	fn parse_expr_beg(
		&mut self,
		tok_stream: &mut TokForwardRh,
	) -> Result<Node<Expr>, ParsingError> {
		todo!()
	}

	fn maybe_parse_chop(&mut self, tok_stream: &mut TokForwardRh) -> Option<Node<Chop>> {
		todo!()
	}
}
