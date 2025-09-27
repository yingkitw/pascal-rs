program Fibonacci;
var
  n, i, a, b, temp: integer;
begin
  n := 10;
  a := 0;
  b := 1;
  
  if n >= 1 then
  begin
    // Print first number
    // Note: In a real Pascal implementation, this would use writeln
  end;
  
  if n >= 2 then
  begin
    // Print second number
    // Note: In a real Pascal implementation, this would use writeln
  end;
  
  i := 3;
  while i <= n do
  begin
    temp := a + b;
    a := b;
    b := temp;
    i := i + 1;
  end;
  
  // The nth Fibonacci number is now in b
end.
