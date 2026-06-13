# Pascal Best Practices (pascal-rs)

## Project structure

- Use `pascal init` and keep sources under `src/`.
- Declare dependencies in `pascal.toml`; prefer path deps for monorepos.
- Run `pascal deps --tree` to audit dependency graphs.

## Coding style

- Use meaningful unit and program names (PascalCase).
- Keep procedures focused; extract helpers into separate units.
- Prefer `const` for compile-time values; use `pascal check` before commit.

## Error handling

- Use `try/except/finally` for recoverable errors.
- Re-raise with bare `raise;` inside except blocks when appropriate.
- Enable verbose builds during development: `pascal build -v`.

## Performance

- Use `pascal bench` to measure hot paths.
- Enable incremental builds (default) — cache at `.pascal/build-cache.json`.
- Use `-O2` or `-O3` for release builds.

## Testing

- Place unit tests in `tests/` mirroring your modules.
- Run `cargo test` and property tests before PRs.
- Use `pascal leak-check` for heap object audits.

## Security

- Run `cargo audit` locally; CI runs it on every push.
- Do not commit secrets; use environment variables for registry tokens.

## Interop

- Mark external routines with `external` and register FFI handlers in Rust for custom natives.
- Use `TypeName(x)` at runtime for reflection/debugging.
