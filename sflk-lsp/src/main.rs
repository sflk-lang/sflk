use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

mod completion;

#[derive(Debug)]
struct Backend {
	client: Client,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
	async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
		Ok(InitializeResult {
			capabilities: ServerCapabilities {
				// We always sync documents
				text_document_sync: Some(TextDocumentSyncCapability::Kind(
					TextDocumentSyncKind::Full,
				)),
				// We know how to react on hover
				hover_provider: Some(HoverProviderCapability::Simple(true)),
				// We know how to provide completion
				completion_provider: Some(CompletionOptions {
					trigger_characters: Some(vec![
						" ".to_string(),
						"=".to_string(),
						"\n".to_string(),
					]),
					..Default::default()
				}),
				..Default::default()
			},
			..Default::default()
		})
	}

	async fn initialized(&self, _: InitializedParams) {
		self.client
			.log_message(MessageType::Info, "sflk server initialized (uwu) !")
			.await;
	}

	async fn did_open(&self, params: DidOpenTextDocumentParams) {
		let uri = params.text_document.uri;
		self.client
			.log_message(MessageType::Info, format!("Opening file {}", uri))
			.await;
	}

	async fn did_change(&self, params: DidChangeTextDocumentParams) {
		let uri = params.text_document.uri;
		self.client
			.log_message(MessageType::Info, format!("Changing file {}", uri))
			.await;
	}

	async fn shutdown(&self) -> Result<()> {
		Ok(())
	}

	async fn completion(&self, _params: CompletionParams) -> Result<Option<CompletionResponse>> {
		Ok(Some(CompletionResponse::Array(vec![
			completion::symb("pr"),
			completion::symb("nl"),
			completion::symb("do"),
			completion::symb("if"),
			completion::symb("redo"),
		])))
	}

	async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
		let position = params.text_document_position_params;
		self.client
			.log_message(
				MessageType::Info,
				format!("hover at line {}", position.position.line),
			)
			.await;
		Ok(Some(Hover {
			contents: HoverContents::Scalar(MarkedString::String(format!(
				"{}",
				position.position.line
			))),
			range: None,
		}))
	}
}

#[tokio::main]
async fn main() {
	let stdin = tokio::io::stdin();
	let stdout = tokio::io::stdout();

	let (service, messages) = LspService::new(|client| Backend { client });
	Server::new(stdin, stdout)
		.interleave(messages)
		.serve(service)
		.await;
}
