program Hello;
var
  x: integer;
  y: integer;
begin
  x := 42;
  y := x + 1;
  // This is a comment
  if y > 40 then
    x := 100
  else
    x := 200;
  
  while x > 0 do
  begin
    x := x - 1;
  end;
end.
