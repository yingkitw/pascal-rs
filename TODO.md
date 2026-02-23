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
- [ ] Unify AST types (`ast.rs` vs `enhanced_ast.rs`) into single coherent module
- [ ] Modularize large source files (split by responsibility)
- [ ] Migrate or re-enable commented-out modules for full compatibility
- [x] Improve `From` conversions between module error types for cleaner error chaining
- [x] Optional minimal build profile (no LSP/MCP/GUI) for smaller binary — `full` feature
- [ ] Plugin architecture for extending compiler functionality
- [ ] Event-driven architecture for compilation phases
- [ ] Microservice decomposition for distributed compilation
- [ ] Configuration system with environment-specific profiles
- [ ] Feature flags system for experimental features
- [ ] Gradual migration to async/await where beneficial
- [ ] Memory-efficient data structures and algorithms
- [ ] Zero-copy parsing and AST construction where possible

### Compiler & Language
- [ ] Generics / generic type parameters with variance and constraints
- [ ] Interface types with multiple inheritance and default methods
- [ ] Compile-time constant evaluation and constexpr functions
- [x] Optimization level flags (-O0, -O1, -O2, -O3) — Compile -O, Build -O override
- [ ] Dead code elimination across units and link-time optimization
- [ ] Better Unicode/UTF-8 string handling with normalization
- [ ] Advanced pattern matching (case expressions with guards)
- [ ] Attribute system for metadata and compiler directives
- [ ] Conditional compilation with feature flags
- [ ] Macro system for code generation
- [ ] Reflection capabilities at runtime
- [ ] Type inference for local variables
- [ ] Union types and variant records
- [ ] Anonymous functions and lambda expressions
- [ ] Async/await syntax for concurrent programming

### Tooling & UX
- [x] `pascal fmt` — basic code formatter (trim, blank lines)
- [x] `pascal check` — parse validation
- [ ] `pascal doc` — documentation generator with Markdown/HTML output
- [ ] Error messages with suggestions ("did you mean X?") using Levenshtein distance
- [ ] Source maps for debugging generated code
- [x] Verbose/quiet flags and progress indicators for builds
- [ ] Interactive debugger with breakpoints and watch expressions
- [ ] Code completion and IntelliSense integration
- [ ] Syntax highlighting extensions for popular editors
- [ ] Project templates for common application types
- [ ] Hot reload for development mode
- [ ] Performance profiler integration
- [ ] Memory leak detection tools

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
