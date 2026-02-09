{ Example 07: Functions with Exit }
program FunctionsWithExit;
var
  r: integer;

function Power(base, exp: integer): integer;
var
  i, acc: integer;
begin
  if exp = 0 then
    exit(1);
  acc := 1;
  for i := 1 to exp do
    acc := acc * base;
  Power := acc;
end;

function SumUpTo(n: integer): integer;
var
  i, total: integer;
begin
  total := 0;
  for i := 1 to n do
    total := total + i;
  SumUpTo := total;
end;

function SumRange(a, b: integer): integer;
begin
  SumRange := SumUpTo(b) - SumUpTo(a - 1);
end;

function FindFirst(target: integer): integer;
var
  i: integer;
begin
  for i := 1 to 100 do
  begin
    if i * i >= target then
      exit(i);
  end;
  FindFirst := -1;
end;

function Abs2(n: integer): integer;
begin
  if n < 0 then
    exit(-n);
  Abs2 := n;
end;

begin
  writeln('=== Functions with Exit ===');

  writeln('2^10 = ', Power(2, 10));
  writeln('3^5 = ', Power(3, 5));
  writeln('5^0 = ', Power(5, 0));

  writeln('Sum 1..10 = ', SumRange(1, 10));
  writeln('Sum 5..15 = ', SumRange(5, 15));

  writeln('First i where i^2 >= 50: ', FindFirst(50));
  writeln('First i where i^2 >= 100: ', FindFirst(100));

  writeln('Abs(-42) = ', Abs2(-42));
  writeln('Abs(7) = ', Abs2(7));
end.
