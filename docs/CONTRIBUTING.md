# Contributing to pascal-rs

Thank you for contributing. This project follows standard Rust and Pascal compiler conventions.

## Development setup

```bash
git clone https://github.com/yingkitw/pascal-rs
cd pascal-rs
cargo build
cargo test
```

## Before submitting

1. `cargo fmt --all`
2. `cargo clippy --all-targets`
3. `cargo test`

## Pull request guidelines

- Keep changes focused and well-tested.
- Update `TODO.md` if you complete planned work.
- Add tests for new behavior in `src/` (`#[cfg(test)]`) or `tests/`.

## Project layout

See `ARCHITECTURE.md` and `AGENTS.md` for module structure and build commands.
