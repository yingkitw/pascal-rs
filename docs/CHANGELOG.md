# Changelog

All notable changes to pascal-rs are documented here.

## [Unreleased]

### Added
- Dependency resolver: path, git clone, registry cache (`src/deps.rs`)
- Incremental compile cache (`.pascal/build-cache.json`)
- `pascal deps --tree`, `pascal bench`
- Property-based tests (proptest), Criterion benchmarks
- Cross-platform CI, cargo-audit, Docker image
- LSP server, REPL, leak-check, profiler integration
- Editor grammar and Neovim/Emacs LSP configs

### Changed
- Build integrates dependency search paths and incremental caching

## [0.1.6]

- Interpreter Object Pascal support, build system, CLI tooling
