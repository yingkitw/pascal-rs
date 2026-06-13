//! Language Server Protocol implementation for Pascal.
//!
//! Build with `--features lsp` and run: `pascal lsp`

use crate::ide::{completions_at, hover_at, parse_diagnostics, CompletionKind};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

struct Document {
    text: String,
    version: i32,
}

pub struct PascalLanguageServer {
    client: Client,
    documents: Arc<RwLock<HashMap<Url, Document>>>,
}

impl PascalLanguageServer {
    fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    async fn publish_diagnostics(&self, uri: Url, text: &str) {
        let diags: Vec<Diagnostic> = parse_diagnostics(text)
            .into_iter()
            .map(|(line, col, message)| Diagnostic {
                range: Range {
                    start: Position {
                        line: line as u32,
                        character: col as u32,
                    },
                    end: Position {
                        line: line as u32,
                        character: col as u32 + 1,
                    },
                },
                severity: Some(DiagnosticSeverity::ERROR),
                message,
                ..Default::default()
            })
            .collect();

        self.client
            .publish_diagnostics(uri, diags, None)
            .await;
    }
}

fn completion_kind_to_lsp(kind: CompletionKind) -> CompletionItemKind {
    match kind {
        CompletionKind::Keyword => CompletionItemKind::KEYWORD,
        CompletionKind::Variable => CompletionItemKind::VARIABLE,
        CompletionKind::Constant => CompletionItemKind::CONSTANT,
        CompletionKind::Type => CompletionItemKind::TYPE_PARAMETER,
        CompletionKind::Procedure => CompletionItemKind::METHOD,
        CompletionKind::Function => CompletionItemKind::FUNCTION,
        CompletionKind::Class => CompletionItemKind::CLASS,
        CompletionKind::Builtin => CompletionItemKind::FUNCTION,
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for PascalLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "pascal-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string()]),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Pascal language server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let text = params.text_document.text.clone();
        let version = params.text_document.version;

        self.documents.write().await.insert(
            uri.clone(),
            Document {
                text: text.clone(),
                version,
            },
        );
        self.publish_diagnostics(uri, &text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let version = params.text_document.version;

        let text = params
            .content_changes
            .into_iter()
            .next()
            .map(|c| c.text)
            .unwrap_or_default();

        self.documents.write().await.insert(
            uri.clone(),
            Document { text: text.clone(), version },
        );
        self.publish_diagnostics(uri, &text).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents.write().await.remove(&params.text_document.uri);
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let items: Vec<CompletionItem> = completions_at(&doc.text, pos.line as usize, pos.character as usize)
            .into_iter()
            .map(|c| CompletionItem {
                label: c.label,
                kind: Some(completion_kind_to_lsp(c.kind)),
                detail: c.detail,
                documentation: c.documentation.map(|d| Documentation::String(d)),
                ..Default::default()
            })
            .collect();

        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(&uri) else {
            return Ok(None);
        };

        let content = match hover_at(&doc.text, pos.line as usize, pos.character as usize) {
            Some(c) => c,
            None => return Ok(None),
        };
        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: content,
            }),
            range: None,
        }))
    }
}

/// Run the LSP server on stdin/stdout (stdio transport).
pub async fn run_lsp_server() -> anyhow::Result<()> {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = LspService::new(PascalLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
    Ok(())
}
