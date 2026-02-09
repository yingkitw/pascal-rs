# TODO - pascal-rs Pascal Compiler

## Current Status

**115 unit tests + 19 example integration tests + 82 other integration tests = 216 total tests passing.** Build clean.

Interpreter supports standard Pascal + full Object Pascal (classes, exceptions, inheritance, virtual dispatch, properties, arrays, records, string indexing, with, exit, uses, nested functions).

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

## Future Work

- [ ] CI/CD pipeline
- [ ] Benchmark suite vs. FPC
- [ ] LSP server for IDE integration
- [ ] REPL mode
- [ ] Array element assignment (`arr[i] := val`)
- [ ] Multi-dimensional arrays
- [ ] Enum types in interpreter
- [ ] Set operations in interpreter
- [ ] Pointer/reference support

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
- [x] CLI (compile, run, info, clean)
- [x] 115 unit tests passing
