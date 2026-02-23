program FormatterDemo;
uses
  SysUtils,
  Classes;

var
  i: Integer;
  message: String;
  numbers: array[1..10] of Integer;

procedure PrintMessage(msg: String);
begin
  writeln(msg);
end;

function Add(a, b: Integer): Integer;
begin
  Result := a + b;
end;

begin
  message := 'Hello from Pascal formatter!';
  PrintMessage(message);
  
  for i := 1 to 10 do
  begin
    numbers[i] := i * 2;
    writeln(numbers[i]);
  end;
  
  if Add(5, 3) > 7 then
    writeln('Calculation is correct')
  else
    writeln('Calculation is wrong');
    
  while i > 0 do
  begin
    i := i - 1;
    writeln('Countdown: ', i);
  end;
end.