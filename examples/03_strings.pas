{ Example 03: String Operations }
program StringOps;
var
  s, t, result: string;
  i, p, len: integer;
  ch: char;

function ReverseString(s: string): string;
var
  i, n: integer;
  rev: string;
begin
  n := length(s);
  rev := '';
  for i := n downto 1 do
    rev := concat(rev, copy(s, i, 1));
  ReverseString := rev;
end;

function CountChar(s: string; target: char): integer;
var
  i, count: integer;
begin
  count := 0;
  for i := 1 to length(s) do
    if s[i] = target then
      inc(count);
  CountChar := count;
end;

function IsPalindrome(s: string): boolean;
begin
  IsPalindrome := (s = ReverseString(s));
end;

begin
  writeln('=== String Operations ===');

  s := 'Hello, World!';
  writeln('String: ', s);
  writeln('Length: ', length(s));
  writeln('Uppercase: ', upcase(s));
  writeln('Lowercase: ', lowercase(s));

  { Substring }
  t := copy(s, 1, 5);
  writeln('First 5 chars: ', t);

  { Find position }
  p := pos('World', s);
  writeln('Position of World: ', p);

  { Character access }
  ch := s[1];
  writeln('First char: ', ch);

  { String concatenation }
  result := concat('Hello', ' ', 'Pascal');
  writeln('Concat: ', result);

  { Reverse }
  writeln('Reversed: ', ReverseString('abcdef'));

  { Count characters }
  writeln('Count of l in Hello: ', CountChar('Hello', 'l'));

  { Palindrome check }
  if IsPalindrome('racecar') then
    writeln('racecar is a palindrome')
  else
    writeln('racecar is not a palindrome');

  if IsPalindrome('hello') then
    writeln('hello is a palindrome')
  else
    writeln('hello is not a palindrome');

  { IntToStr / StrToInt }
  writeln('42 as string: ', inttostr(42));
  writeln('"123" as int: ', strtoint('123'));
end.
