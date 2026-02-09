{ Example 08: Mathematical Algorithms }
program MathAlgorithms;
var
  i, n: integer;

function GCD(a, b: integer): integer;
begin
  while b <> 0 do
  begin
    result := b;
    b := a mod b;
    a := result;
  end;
  GCD := a;
end;

function LCM(a, b: integer): integer;
begin
  LCM := (a div GCD(a, b)) * b;
end;

function IsPrime(n: integer): boolean;
var
  i: integer;
begin
  if n < 2 then
  begin
    IsPrime := false;
    exit;
  end;
  i := 2;
  while i * i <= n do
  begin
    if n mod i = 0 then
    begin
      IsPrime := false;
      exit;
    end;
    inc(i);
  end;
  IsPrime := true;
end;

function Collatz(n: integer): integer;
var
  steps: integer;
begin
  steps := 0;
  while n <> 1 do
  begin
    if n mod 2 = 0 then
      n := n div 2
    else
      n := 3 * n + 1;
    inc(steps);
  end;
  Collatz := steps;
end;

function DigitSum(n: integer): integer;
var
  sum: integer;
begin
  sum := 0;
  if n < 0 then n := -n;
  while n > 0 do
  begin
    sum := sum + (n mod 10);
    n := n div 10;
  end;
  DigitSum := sum;
end;

function IntPow(base, exp: integer): integer;
var
  result: integer;
begin
  result := 1;
  while exp > 0 do
  begin
    if exp mod 2 = 1 then
      result := result * base;
    base := base * base;
    exp := exp div 2;
  end;
  IntPow := result;
end;

begin
  writeln('=== Math Algorithms ===');

  writeln('GCD(48, 18) = ', GCD(48, 18));
  writeln('GCD(100, 75) = ', GCD(100, 75));
  writeln('LCM(12, 18) = ', LCM(12, 18));

  write('Primes up to 30: ');
  for i := 2 to 30 do
    if IsPrime(i) then
      write(i, ' ');
  writeln;

  writeln('Collatz(27) steps: ', Collatz(27));
  writeln('Collatz(1000) steps: ', Collatz(1000));

  writeln('DigitSum(12345) = ', DigitSum(12345));
  writeln('DigitSum(999) = ', DigitSum(999));

  writeln('2^16 = ', IntPow(2, 16));
  writeln('3^10 = ', IntPow(3, 10));
end.
