{ Example 02: Functions and Procedures }
program Functions;
var
  result, n: integer;
  greeting: string;

function Factorial(n: integer): integer;
var
  i, acc: integer;
begin
  acc := 1;
  for i := 2 to n do
    acc := acc * i;
  Factorial := acc;
end;

function Fibonacci(n: integer): integer;
var
  a, b, temp, i: integer;
begin
  if n <= 1 then
  begin
    Fibonacci := n;
    exit;
  end;
  a := 0;
  b := 1;
  for i := 2 to n do
  begin
    temp := a + b;
    a := b;
    b := temp;
  end;
  Fibonacci := b;
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

procedure PrintFactorials(max: integer);
var
  i: integer;
begin
  for i := 1 to max do
    writeln(i, '! = ', Factorial(i));
end;

begin
  writeln('=== Functions Demo ===');

  writeln('Factorial(10) = ', Factorial(10));
  writeln('Fibonacci(10) = ', Fibonacci(10));
  writeln('IsPrime(17) = ', IsPrime(17));
  writeln('GCD(48, 18) = ', GCD(48, 18));

  writeln('--- Factorials 1..7 ---');
  PrintFactorials(7);

  { Count primes up to 50 }
  n := 0;
  for result := 2 to 50 do
    if IsPrime(result) then
      inc(n);
  writeln('Primes up to 50: ', n);
end.
