program Fibonacci;
var
  n, i, a, b, temp: integer;
begin
  n := 10;
  a := 0;
  b := 1;

  if n >= 1 then
    writeln(a);

  if n >= 2 then
    writeln(b);

  for i := 3 to n do
  begin
    temp := a + b;
    a := b;
    b := temp;
  end;

  writeln('Fibonacci(', n, ') = ', b);
end.
