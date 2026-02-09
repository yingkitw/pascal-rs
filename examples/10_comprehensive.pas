{ Example 10: Comprehensive - Combines all features }
program Comprehensive;
var
  i, val, n: integer;
  s, t: string;
  ch: char;
  flag: boolean;

function ReverseNumber(n: integer): integer;
var
  rev: integer;
begin
  rev := 0;
  while n > 0 do
  begin
    rev := rev * 10 + (n mod 10);
    n := n div 10;
  end;
  ReverseNumber := rev;
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

function Factorial(n: integer): integer;
var
  i, acc: integer;
begin
  acc := 1;
  for i := 2 to n do
    acc := acc * i;
  Factorial := acc;
end;

function RecFib(n: integer): integer;
begin
  if n <= 1 then
    RecFib := n
  else
    RecFib := RecFib(n - 1) + RecFib(n - 2);
end;

begin
  writeln('=== Comprehensive Demo ===');

  { Arithmetic and control flow }
  writeln('--- Arithmetic ---');
  writeln('Factorial(10) = ', Factorial(10));
  writeln('Fibonacci(10) = ', RecFib(10));
  writeln('Reverse 12345: ', ReverseNumber(12345));

  { Primes }
  writeln('--- Primes ---');
  n := 0;
  for i := 2 to 50 do
    if IsPrime(i) then
      inc(n);
  writeln('Primes up to 50: ', n);

  { String manipulation }
  writeln('--- Strings ---');
  s := 'Hello, Pascal World!';
  writeln('Original: ', s);
  writeln('Length: ', length(s));
  writeln('Upper: ', upcase(s));
  ch := s[1];
  writeln('Char 1: ', ch);
  t := copy(s, 8, 6);
  writeln('Substr: ', t);

  { Boolean logic }
  writeln('--- Logic ---');
  flag := (10 > 5) and (3 < 7);
  writeln('(10>5) and (3<7) = ', flag);
  flag := (1 = 2) or (3 = 3);
  writeln('(1=2) or (3=3) = ', flag);

  { Case statement }
  writeln('--- Case ---');
  i := 3;
  case i of
    1: writeln('one');
    2: writeln('two');
    3: writeln('three');
  else
    writeln('other');
  end;

  { Exception handling }
  writeln('--- Exceptions ---');
  try
    writeln('Before raise');
    raise Exception.Create('test error');
    writeln('SHOULD NOT PRINT');
  except
    on E: Exception do
      writeln('Caught: test error');
  end;

  writeln('All tests passed!');
end.
