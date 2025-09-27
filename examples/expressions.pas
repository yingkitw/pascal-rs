program Expressions;
var
  a, b, c, d, result: integer;
  x, y, z: real;
  flag1, flag2, flag3: boolean;
begin
  a := 10;
  b := 5;
  c := 3;
  d := 2;
  
  // Arithmetic expressions
  result := a + b * c - d;
  result := (a + b) * (c - d);
  result := a div b + c mod d;
  
  // Complex nested expressions
  result := ((a + b) * c) div (d + 1);
  result := a * b + c * d - (a + b) * (c - d);
  
  // Boolean expressions
  flag1 := a > b;
  flag2 := b <= c;
  flag3 := (a = b) or (c <> d);
  
  // Complex boolean logic
  if (a > b) and (c < d) or (a = 10) then
  begin
    result := result + 1;
  end;
  
  if not (a < b) and (c >= d) then
  begin
    result := result - 1;
  end;
  
  // Nested boolean expressions
  if ((a > b) and (c < d)) or ((a = b) and (c <> d)) then
  begin
    result := result * 2;
  end;
  
  // Real number expressions (if supported)
  x := 3.14;
  y := 2.71;
  z := x * y + 1.0;
  
  // Complex real expressions
  z := (x + y) * (x - y) / (x * y);
  z := x * x + y * y - 2.0 * x * y;
end.
