# Dependency Version Compatibility Matrix

| Crate | Version | Feature gate | Purpose |
|-------|---------|--------------|---------|
| clap | 4.5 | — | CLI |
| logos | 0.15 | — | Lexer |
| serde/toml | 1.0/0.8 | — | Manifest |
| tower-lsp | 0.20 | `lsp` | Language server |
| pprof | 0.14 | `profile` | CPU profiling |
| rmcp | 0.14 | `mcp` | MCP server |
| cocoa/objc | 0.25/0.2 | `gui` | macOS GUI |

## Package dependencies (pascal.toml)

| Source | Resolution |
|--------|------------|
| `path = "../lib"` | Local directory (validated, added to search paths) |
| `git = "url"` | Cloned to `.pascal/deps/<name>` |
| `"1.0"` (version) | Registry cache `.pascal/registry/<name>-<version>/` |

Set `PASCAL_REGISTRY` for registry URL. Vendor packages locally when offline.

## Semver

Version constraints use major.minor.patch comparison via `deps::version_satisfies`.
