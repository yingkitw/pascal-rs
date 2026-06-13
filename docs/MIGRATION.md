# Migration Guide

## From Free Pascal (FPC)

| FPC | pascal-rs |
|-----|-----------|
| `fpc program.pas` | `pascal run program.pas` |
| `fpc -O2` | `pascal compile -O2 program.pas` |
| `.lpi/.lfm` | Not supported (use `pascal.toml` projects) |
| RTL units | Partial; vendored units via `[dependencies]` path/git |

## From Delphi

- Project files (`.dpr`, `.dproj`) are not read directly; create `pascal.toml` with `pascal init`.
- VCL/FMX not supported; interpreter-only for UI code.
- Use `pascal check` for parse/type validation without running.

## Project migration steps

1. `pascal init myapp --template default`
2. Copy `.pas` sources into `src/`
3. Add dependencies: `pascal add utils --path ../utils`
4. `pascal build && pascal run`
