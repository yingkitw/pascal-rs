program Constants;
const
  MAX_SIZE = 100;
  PI = 3.14159;
  E = 2.71828;
  MESSAGE = 'Hello World';
  FLAG = true;
var
  radius, area, circumference: real;
  count, limit: integer;
  message: string;
begin
  limit := MAX_SIZE;
  count := 0;
  
  // Using constants in calculations
  radius := 5.0;
  area := PI * radius * radius;
  circumference := 2.0 * PI * radius;
  
  // Using constants in conditions
  while count < limit do
  begin
    if count mod 10 = 0 then
    begin
      // Process every 10th item
      area := area + 1.0;
    end;
    count := count + 1;
  end;
  
  // Using boolean constants
  if FLAG then
  begin
    count := count + 1;
  end;
  
  // Complex expressions with constants
  if (count > MAX_SIZE div 2) and (area > PI * 10) then
  begin
    circumference := circumference * E;
  end;
  
  // String constant usage
  message := MESSAGE;
end.
