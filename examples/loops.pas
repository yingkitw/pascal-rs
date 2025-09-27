program Loops;
var
  i, j, sum, product: integer;
  counter: integer;
begin
  sum := 0;
  product := 1;
  counter := 0;
  
  // For loop simulation using while
  i := 1;
  while i <= 5 do
  begin
    sum := sum + i;
    i := i + 1;
  end;
  
  // Nested loops
  i := 1;
  while i <= 3 do
  begin
    j := 1;
    while j <= 3 do
    begin
      product := product * i * j;
      j := j + 1;
    end;
    i := i + 1;
  end;
  
  // Complex while loop with conditions
  counter := 0;
  while (counter < 10) and (sum < 50) do
  begin
    if counter mod 2 = 0 then
    begin
      sum := sum + counter;
    end
    else
    begin
      sum := sum - 1;
    end;
    counter := counter + 1;
  end;
  
  // Do-while simulation
  i := 0;
  repeat
    i := i + 1;
    sum := sum + i;
  until i >= 5;
end.
