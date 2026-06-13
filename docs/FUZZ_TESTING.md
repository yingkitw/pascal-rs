# Fuzz Testing

Fuzz targets for parser and interpreter can be added with `cargo-fuzz`:

```bash
cargo install cargo-fuzz
cargo fuzz init
# Add fuzz targets calling Parser::parse_program and Interpreter::run_program
cargo fuzz run parser
```

Recommended corpora: `examples/` and `tests/test_programs/`.

See [Rust Fuzz Book](https://rust-fuzz.github.io/book/) for AFL/libFuzzer integration.
