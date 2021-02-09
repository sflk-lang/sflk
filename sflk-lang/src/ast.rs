use crate::scu::Loc;

// TODO:
// - move Loc here
// - delete Located
// - parser->program must become parser->ast->program

struct MetaNode<T> {
	node: T,
	loc: Loc,
}
