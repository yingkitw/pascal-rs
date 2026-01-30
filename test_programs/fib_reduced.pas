program FibReduced;
var
  n, i: integer;
begin
  n := 10;

  if n >= 1 then
    writeln(n);

  if n >= 2 then
    writeln(n);

  for i := 3 to n do
    writeln(i);
end.
