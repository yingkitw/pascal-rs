{ Example 04: Arrays - SetLength, indexing, high/low }
program Arrays;
var
  arr: integer;
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

  { Sum of default values (all 0) }
  sum := 0;
  for i := 0 to n - 1 do
    sum := sum + arr[i];
  writeln('Sum of defaults: ', sum);

  writeln('Element 0: ', arr[0]);
  writeln('Element 5: ', arr[5]);

  writeln('Done.');
end.
