# Development Guide

## Profiling

CPU profiling is available when building with the `profile` feature:

```bash
cargo build --features profile
```

Run a program with profiling to generate a flamegraph:

```bash
pascal run myprog.pas --profile
# Writes flamegraph.svg by default

pascal run myprog.pas --profile --profile-output cpu.svg
```

Open the generated SVG in a browser to view the flamegraph. The graph shows where the interpreter spends time during execution.

## Memory Leak Detection

For compiled Pascal binaries and the interpreter:

### AddressSanitizer (Linux/macOS)

Build and run with AddressSanitizer to detect memory leaks and buffer overflows:

```bash
RUSTFLAGS="-Z sanitizer=address" cargo build --target x86_64-unknown-linux-gnu
# Or on macOS (aarch64):
RUSTFLAGS="-Z sanitizer=address" cargo build --target aarch64-apple-darwin
```

Then run your tests or interpreter:

```bash
cargo test
pascal run program.pas
```

### Valgrind (Linux)

If you have a compiled Pascal binary (from another backend), run:

```bash
valgrind --leak-check=full ./my_program
```

For the interpreter itself:

```bash
valgrind --leak-check=full cargo run -- run program.pas
```

### Running Tests with Sanitizers

```bash
RUSTFLAGS="-Z sanitizer=address" cargo test
```

Note: Sanitizers require a compatible target. On macOS, `x86_64-apple-darwin` and `aarch64-apple-darwin` are supported for AddressSanitizer.
