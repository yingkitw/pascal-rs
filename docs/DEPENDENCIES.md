# Dependency Audit

This document describes the pascal-rs dependency tree and notable transitive dependencies.

## Direct Dependencies

| Crate | Purpose |
|-------|---------|
| clap | CLI argument parsing |
| anyhow | Application-level error handling |
| thiserror | Custom error types |
| logos | Lexer / tokenization |
| serde, toml | Manifest and config serialization |
| bincode | Binary serialization (PPU) |
| rayon | Parallel compilation |
| rmcp | MCP server (when `mcp` feature enabled) |
| sha2 | Checksums for lock file |
| colored | CLI output styling |
| num_cpus | Parallel thread count |

## Optional Dependencies (Feature-Gated)

| Feature | Crates | Purpose |
|---------|--------|---------|
| `lsp` | tower-lsp, tokio, serde_json | Language Server Protocol |
| `gui` | cocoa, objc | macOS native GUI |
| `debug` | gimli | Debug information |
| `profile` | pprof | CPU profiling; `pascal run --profile` writes flamegraph.svg |
| `full` | lsp + mcp + gui | All optional features |

## Transitive Dependencies (Notable)

- **chrono** (via rmcp, schemars): Date/time handling. Used for MCP and JSON schema.
- **iana-time-zone** (via chrono): Time zone detection. Adds `wasm-bindgen` and `js-sys` only for `target_arch = "wasm32"`; not compiled for native builds.
- **uuid** (via debugid, symbolic-common when `debug` feature enabled): UUID generation for debug IDs.

## wasm-bindgen

`wasm-bindgen` appears in `Cargo.lock` as a transitive dependency of:
- chrono (for wasm32 target)
- iana-time-zone (for wasm32 target)
- uuid (for wasm32 target)

These are **target-specific**: when building for native targets (e.g. x86_64-apple-darwin), wasm-bindgen is not compiled. It remains in the lock file because Cargo resolves the full dependency graph for all possible targets.

## Minimal Build

Default build (`cargo build`) uses no optional features. For all features:
```bash
cargo build --features full
```
