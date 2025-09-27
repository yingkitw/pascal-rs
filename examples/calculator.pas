program Calculator;
var
  num1, num2, result: integer;
  operation: integer; // 1=add, 2=subtract, 3=multiply, 4=divide
begin
  num1 := 15;
  num2 := 3;
  operation := 1; // Addition
  
  if operation = 1 then
  begin
    result := num1 + num2;
  end
  else if operation = 2 then
  begin
    result := num1 - num2;
  end
  else if operation = 3 then
  begin
    result := num1 * num2;
  end
  else if operation = 4 then
  begin
    if num2 <> 0 then
    begin
      result := num1 div num2; // Integer division
    end
    else
    begin
      result := 0; // Division by zero case
    end;
  end
  else
  begin
    result := 0; // Invalid operation
  end;
  
  // Nested if-else for more complex logic
  if result > 100 then
  begin
    if result > 1000 then
    begin
      result := result - 1000;
    end
    else
    begin
      result := result - 100;
    end;
  end;
end.
