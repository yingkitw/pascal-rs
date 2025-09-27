program Conditional;
var
  x, y: integer;
begin
  x := 15;
  y := 10;
  
  if x > y then
  begin
    x := x - y;
  end
  else
  begin
    y := y - x;
  end;
  
  if x = y then
  begin
    x := 0;
  end;
end.
