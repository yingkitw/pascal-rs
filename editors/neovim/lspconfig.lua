-- Neovim LSP config for pascal-rs
-- Requires: cargo build --features lsp
require('lspconfig').pascal_rs.setup({
  cmd = { 'pascal', 'lsp' },
  filetypes = { 'pascal' },
  root_dir = require('lspconfig.util').root_pattern('pascal.toml', '.git'),
})
