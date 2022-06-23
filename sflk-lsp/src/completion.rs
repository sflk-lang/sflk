use tower_lsp::lsp_types::*;

pub fn symb(name: &str) -> CompletionItem {
	CompletionItem { label: name.to_string(), ..Default::default() }
}
