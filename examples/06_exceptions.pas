{ Example 06: Exception Handling }
program Exceptions;
var
  x, y, result: integer;

function SafeDivide(a, b: integer): integer;
begin
  if b = 0 then
    raise Exception.Create('Division by zero');
  SafeDivide := a div b;
end;

procedure DangerousOperation;
begin
  writeln('  Starting dangerous operation...');
  raise Exception.Create('Something went wrong!');
  writeln('  This line should never execute');
end;

begin
  writeln('=== Exception Handling Demo ===');

  { Basic try/except }
  writeln('--- Try/Except ---');
  try
    x := SafeDivide(100, 5);
    writeln('100 / 5 = ', x);
    x := SafeDivide(42, 0);
    writeln('This should not print');
  except
    on E: Exception do
      writeln('Caught: division error');
  end;

  { Try/finally }
  writeln('--- Try/Finally ---');
  x := 0;
  try
    x := 1;
    writeln('  x set to ', x);
    DangerousOperation;
  except
    on E: Exception do
      writeln('  Caught exception');
  end;
  writeln('  After exception, x = ', x);

  { Nested try blocks }
  writeln('--- Nested Try ---');
  try
    writeln('  Outer try');
    try
      writeln('  Inner try');
      raise Exception.Create('inner error');
    except
      on E: Exception do
        writeln('  Inner caught');
    end;
    writeln('  Back in outer try');
  except
    on E: Exception do
      writeln('  Outer caught (should not reach here)');
  end;

  writeln('Done.');
end.
