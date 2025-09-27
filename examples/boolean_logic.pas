program BooleanLogic;
var
  a, b, c: integer;
  flag1, flag2: boolean;
begin
  a := 10;
  b := 5;
  c := 3;
  
  flag1 := a > b;
  flag2 := b < c;
  
  if flag1 and flag2 then
  begin
    c := a + b;
  end;
  
  if flag1 or flag2 then
  begin
    c := c * 2;
  end;
end.
