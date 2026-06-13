;;; pascal-lsp.el — Emacs LSP client for pascal-rs
(require 'lsp-mode)

(lsp-register-client
 (make-lsp-client
  :connection (lsp-stdio-connection '("pascal" "lsp"))
  :activation-fn (lsp-activate-on "pascal-mode")
  :server-id 'pascal-rs))
