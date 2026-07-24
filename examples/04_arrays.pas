{ Example 04: Arrays - SetLength, indexing, high/low }
program Arrays;
var
  arr: array of integer;
  i, n, sum: integer;
  min, max, len: integer;
begin
  writeln('=== Array Operations ===');

  { Create array with SetLength }
  n := 10;
  setlength(arr, n);

  len := length(arr);
  writeln('Array length: ', len);
  writeln('Low: ', low(arr));
  writeln('High: ', high(arr));

  { Fill the array }
  for i := 0 to n - 1 do
    arr[i] := i * i;

  { Sum of values }
  sum := 0;
  for i := 0 to n - 1 do
    sum := sum + arr[i];
  writeln('Sum: ', sum);

  writeln('Element 0: ', arr[0]);
  writeln('Element 5: ', arr[5]);

  writeln('Done.');
end.
