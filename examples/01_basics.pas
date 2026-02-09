{ Example 01: Basic Pascal - variables, arithmetic, control flow }
program Basics;
var
  x, y, z: integer;
  name: string;
  pi: real;
  flag: boolean;
begin
  { Variable assignment and arithmetic }
  x := 10;
  y := 25;
  z := x + y * 2;

  { String assignment }
  name := 'Pascal';

  { Real numbers }
  pi := 3.14159;

  { Boolean logic }
  flag := (x > 5) and (y < 30);

  { If/else }
  if flag then
    z := z + 100
  else
    z := z - 100;

  { While loop }
  while x > 0 do
    x := x - 1;

  { For loop }
  for y := 1 to 10 do
    z := z + y;

  { Repeat/until }
  x := 5;
  repeat
    x := x - 1;
  until x = 0;

  { Case statement }
  x := 3;
  case x of
    1: z := 100;
    2: z := 200;
    3: z := 300;
  else
    z := 0;
  end;

  writeln('Basics complete. z = ', z);
end.
