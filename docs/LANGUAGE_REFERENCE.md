# Pascal Language Reference (pascal-rs subset)

pascal-rs implements standard Pascal and Object Pascal features in the interpreter.

## Program structure

```pascal
program Name;
uses Unit1, Unit2;
const
  Max = 100;
type
  TPoint = record x, y: integer; end;
var
  n: integer;
begin
  n := 42;
end.
```

## Types

| Type | Notes |
|------|-------|
| integer, real, boolean, char, string | Built-in |
| array, record, set, enum | Supported in interpreter |
| class | Single inheritance, virtual/override |

## Statements

`if`, `while`, `for`, `repeat`, `case` (with `when` guards), `with`, `try/except/finally`, `raise`, `exit`.

## Object Pascal

- Classes, constructors, destructors
- `inherited`, `is`, `as`
- Properties (read/write infrastructure)

## Directives

Conditional compilation: `{$IFDEF}`, `{$IFNDEF}`, `{$DEFINE}`, `{$UNDEF}`, `{$ENDIF}`.

See `examples/` for runnable programs.
