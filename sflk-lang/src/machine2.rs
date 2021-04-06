use crate::object::Obj;
use crate::program::{Block, Chain};
use std::collections::HashMap;

enum BlockOrChain {
	Block(Block),
	Chain(Chain),
}

struct ExecNode {
	block_or_chain: BlockOrChain,
	ip: usize,
	sub_nodes: Vec<ExecNode>,
}
