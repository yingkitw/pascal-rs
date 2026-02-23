# TODO - pascal-rs Pascal Compiler

## Current Status

**127 unit tests + 19 example integration tests + 82 other integration tests = 228 total tests passing.** Build clean.

Interpreter supports standard Pascal + full Object Pascal (classes, exceptions, inheritance, virtual dispatch, properties, arrays, records, string indexing, with, exit, uses, nested functions).

Build system with `pascal.toml` manifest, dependency management, lock file, and topological build ordering.

10 example programs at varying complexity levels validate the full pipeline (source → lexer → parser → interpreter).

---

## Phase 1: Object Pascal — Interpreter Support ✅ DONE

### 1.1 Exception Handling ✅
- [x] `PascalException` type with class_name + message
- [x] `execute_try` — try/except/finally flow
- [x] `execute_raise` — create and throw exceptions
- [x] `on E: ExceptionType do` matching (case-insensitive)
- [x] Re-raise with bare `raise;`
- [x] Finally block always executes (even on exception)
- [x] 7 tests

### 1.2 Class Support ✅
- [x] `Value::Object { class_name, fields }` variant
- [x] Class registry (`HashMap<String, ClassDecl>`)
- [x] Constructor: `ClassName.Create(args)` with body
- [x] Destructor support
- [x] Field access: `obj.field` (get + set)
- [x] Method dispatch with Self binding
- [x] Single inheritance (field + method merging)
- [x] 5 tests

### 1.3 Advanced OOP ✅
- [x] `is` / `as` type checks (walks inheritance chain)
- [x] `inherited` expression evaluation
- [x] `virtual` / `override` vtable dispatch via `find_method_in_hierarchy`
- [x] Property read/write infrastructure (`resolve_property_read`/`write`)
- [x] Dot-notation in parser: `obj.field`, `obj.Method()`, `a.b.c`
- [x] Bracket indexing in parser: `arr[i]`, `s[i]`
- [x] 5 tests (is, as, virtual/override, inherited)

---

## Phase 2: Parser Hardening & Cleanup ✅ DONE

- [x] Removed unused imports (`InterfaceDecl`)
- [x] Fixed unused mut variables
- [x] Source location tracking (`SourceLocation` with line/column)
- [x] Error messages include `at line N, column M`
- [x] Error recovery via `consume_or_skip` + `synchronize`

---

## Phase 3: Interpreter Enhancements ✅ DONE

- [x] `Value::Array { elements, lower_bound }` — dynamic arrays
- [x] Array indexing with bounds checking
- [x] `high()`, `low()`, `length()` for arrays
- [x] `SetLength()` for arrays and strings
- [x] `Value::Record { fields }` — record field access/assignment
- [x] String indexing: `s[i]` (1-indexed, Pascal style)
- [x] `uses` clause — loads `.pas` unit files, imports declarations
- [x] `with` statement — pushes object/record fields as scope
- [x] `exit` from functions/procedures (via `EarlyReturn` error type)
- [x] Nested function/procedure scoping
- [x] 7 tests (arrays, records, strings, with, exit, nested, uses)

---

## Phase 4: Build System & Package Manager ✅ DONE

- [x] `pascal.toml` manifest format (package, dependencies, build config)
- [x] `pascal init <name>` — scaffold project (pascal.toml, src/, tests/, examples/)
- [x] `pascal build` — compile all units in dependency order (topological sort)
- [x] `pascal run` — project mode (find pascal.toml, run main) + single-file mode
- [x] `pascal add <dep>` — add dependency (version, --path, --git)
- [x] `pascal remove <dep>` — remove dependency
- [x] `pascal.lock` — lock file with SHA-256 checksums
- [x] Path-based dependency resolution
- [x] `uses` clause extraction for build ordering
- [x] Circular dependency detection
- [x] 12 tests (manifest, lock file, init, build, add/remove, topo sort)

---

## Enhancement Ideas

### Architecture & Codebase
- [x] Unify AST types — `ast.rs` is the single unified module; `enhanced_ast` removed from docs
- [x] Modularize large source files — interpreter split into `interpreter/` with `value` submodule
- [x] Migrate or re-enable commented-out modules — formatter re-enabled in lib
- [x] Improve `From` conversions between module error types for cleaner error chaining
- [x] Optional minimal build profile (no LSP/MCP/GUI) for smaller binary — `full` feature
- [x] Plugin architecture — `plugin.rs`: `CompilerPlugin` trait, `PluginRegistry`
- [x] Event-driven compilation phases — `compilation_events.rs`: `EventEmitter`, `CompilationEvent`, `EventHandler`
- [x] Microservice decomposition — `CompilationWorker` in parallel.rs for distributed compilation
- [x] Configuration system with env profiles — `[profile.dev]`, `[profile.release]` in pascal.toml, `PASCAL_PROFILE` env
- [x] Feature flags system — `[features]` in pascal.toml for project-level feature toggles
- [x] Gradual migration to async/await where beneficial — `AsyncModuleLoader` trait (tokio feature), `load_unit_source_async`
- [ ] Memory-efficient data structures (e.g. Cow for identifiers) — future optimization
- [ ] Zero-copy parsing and AST construction — future; would require lifetime params in AST

### Compiler & Language
- [ ] Generics / generic type parameters with variance and constraints
- [ ] Interface types with multiple inheritance and default methods
- [x] Compile-time constant evaluation and constexpr functions — `constant_eval`, `parse_const_value` for const expressions
- [x] Optimization level flags (-O0, -O1, -O2, -O3) — Compile -O, Build -O override
- [x] Dead code elimination across units and link-time optimization — `eliminate_dead_procedures_and_functions` in optimizer
- [ ] Better Unicode/UTF-8 string handling with normalization
- [x] Advanced pattern matching (case expressions with guards) — `when` guard in CaseBranch
- [ ] Attribute system for metadata and compiler directives
- [x] Conditional compilation with feature flags — `{$IFDEF}`, `{$IFNDEF}`, `{$ENDIF}`, `{$DEFINE}`, `{$UNDEF}`, `-D` flag
- [ ] Macro system for code generation
- [ ] Reflection capabilities at runtime
- [x] Type inference for local variables — `infer_block_variable_types`, `TypeInference::infer_from_expr`
- [ ] Union types and variant records
- [ ] Anonymous functions and lambda expressions
- [ ] Async/await syntax for concurrent programming

### Tooling & UX
- [x] `pascal fmt` — basic code formatter (trim, blank lines)
- [x] `pascal check` — parse validation
- [x] `pascal doc` — documentation generator with Markdown/HTML output — `src/docgen.rs`
- [x] Error messages with suggestions ("did you mean X?") using Levenshtein distance — `error_suggestions.rs`
- [x] Source maps for debugging generated code — `source_map.rs`
- [x] Verbose/quiet flags and progress indicators for builds
- [x] Interactive debugger with breakpoints and watch expressions — `pascal debug -b ProcName -w var`
- [ ] Code completion and IntelliSense integration — LSP supports basic completion
- [x] Syntax highlighting extensions for popular editors — `syntaxes/pascal.tmGrammar.json` (TextMate)
- [x] Project templates for common application types — `pascal init --template default|library|console`
- [x] Hot reload for development mode — `pascal run --watch`
- [x] Performance profiler integration — `profile` feature with pprof (cargo build --features profile)
- [ ] Memory leak detection tools — run with RUSTFLAGS="-Z sanitizer=address" or valgrind

### Testing & Quality
- [ ] Property-based testing (quickcheck/proptest) for lexer/parser
- [ ] Fuzz testing for parser and interpreter with AFL/libFuzzer
- [ ] Performance regression tests in CI with benchmarks
- [ ] Broader integration test coverage for interpreter edge cases
- [ ] Mutation testing framework for test quality assessment
- [ ] Automated code coverage reporting with codecov
- [ ] Contract testing for module boundaries
- [ ] Visual regression testing for GUI components
- [ ] Load testing for compilation of large codebases
- [ ] Cross-platform compatibility test matrix
- [ ] Security vulnerability scanning in dependencies

### Dependencies
- [x] Audit and document transitive deps — see docs/DEPENDENCIES.md
- [ ] Evaluate lighter alternatives for heavy transitive deps where feasible
- [ ] Implement dependency vulnerability scanning and alerts
- [ ] Create dependency update automation with security checks
- [ ] Develop custom lightweight alternatives for critical paths
- [ ] Implement feature-gated dependencies to reduce binary size
- [ ] Add dependency version compatibility matrix
- [ ] Create reproducible builds with exact dependency versions
- [ ] Implement dependency caching for faster builds
- [ ] Add support for private package registries
- [ ] Develop dependency graph analysis tools
- [ ] Implement selective dependency loading based on features

---

## Future Work

### Development Experience
- [ ] CI/CD pipeline with GitHub Actions/GitLab CI
- [ ] Benchmark suite vs. FPC with detailed performance metrics
- [ ] LSP server for IDE integration with full language support
- [ ] REPL mode with autocompletion and history
- [ ] Package registry (fetch deps from URL) with semantic versioning
- [ ] Git dependency fetching with submodules and branches
- [ ] VS Code extension with syntax highlighting and debugging
- [ ] JetBrains plugin family (IntelliJ, CLion, Rider)
- [ ] Vim/Neovim plugin with LSP client integration
- [ ] Emacs package with tree-sitter grammar

### Language Features
- [ ] Array element assignment (`arr[i] := val`) with bounds checking
- [ ] Multi-dimensional arrays with dynamic allocation
- [ ] Enum types in interpreter with scoped constants
- [ ] Set operations in interpreter with efficient bit representations
- [ ] Pointer/reference support with garbage collection options
- [ ] Inline class method bodies in parser with lambda capture
- [ ] Variant records and discriminated unions
- [ ] Operator overloading for custom types
- [ ] Custom attributes and annotations system
- [ ] Partial units and interface sections

### Platform & Integration
- [ ] WebAssembly backend for browser execution
- [ ] LLVM backend for native code generation
- [ ] Docker containerization for reproducible builds
- [ ] Cloud compilation service with remote caching
- [ ] Plugin system for extending compiler functionality
- [ ] Foreign function interface (FFI) for C/C++ libraries
- [ ] Database connectivity libraries (SQL, NoSQL)
- [ ] HTTP client and server libraries
- [ ] GUI framework bindings (Qt, GTK, wxWidgets)
- [ ] Mobile app development support (iOS, Android)

### Performance & Optimization
- [ ] Profile-guided optimization (PGO) support
- [ ] Just-in-time (JIT) compilation for hot paths
- [ ] Incremental compilation for faster rebuild times
- [ ] Parallel parsing and compilation of independent units
- [ ] Smart linking and dead code elimination
- [ ] Memory pool allocation for performance-critical code
- [ ] Vectorization and SIMD optimizations
- [ ] Cache-aware data layout optimizations
- [ ] Branch prediction hints and optimization
- [ ] Loop transformations and optimizations

### Documentation & Community
- [ ] Comprehensive language reference manual
- [ ] Interactive tutorials and learning platform
- [ ] Video tutorial series and conference talks
- [ ] Community forum and Discord server
- [ ] Contribution guidelines and code of conduct
- [ ] Security vulnerability disclosure program
- [ ] Regular release schedule with changelog
- [ ] Migration guides from other Pascal compilers
- [ ] Best practices and design patterns guide
- [ ] Success stories and case studies

---

## Completed (Foundation)

- [x] Lexer (logos-based, 100+ tokens including OOP keywords)
- [x] Parser (recursive descent: programs, units, all statements, expressions)
- [x] AST (unified: Program, Unit, Block, Statement, Expression, Type, ClassDecl, etc.)
- [x] Interpreter (tree-walking: all standard Pascal + Object Pascal)
- [x] Codegen (x86-64 assembly generation)
- [x] Optimizer (constant folding, DCE, CSE, inlining, loop unrolling, strength reduction)
- [x] Type checker (basic validation, function signatures)
- [x] Module system (units, PPU files, parallel compilation)
- [x] CLI (init, build, run, add, remove, compile, info, clean)
- [x] Build system (pascal.toml, pascal.lock, dependency management)
- [x] 228 tests passing
