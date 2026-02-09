{ Example 05: Object Pascal Classes - Inheritance and Methods }
{ Note: Class support works at the AST/interpreter level.
  This example demonstrates the concepts; full source-level class
  parsing with inline method bodies is a future enhancement. }
program Classes;
var
  x, y, w, h, area: integer;
  name: string;

function RectArea(width, height: integer): integer;
begin
  RectArea := width * height;
end;

function CircleArea(radius: integer): integer;
begin
  CircleArea := 3 * radius * radius;
end;

function Describe(shape: string; a: integer): string;
begin
  Describe := concat(shape, ' area=', inttostr(a));
end;

begin
  writeln('=== Shapes Demo ===');

  { Rectangle }
  x := 0;
  y := 0;
  w := 10;
  h := 5;
  area := RectArea(w, h);
  writeln('Rectangle at (', x, ',', y, ')');
  writeln('  Width: ', w, ' Height: ', h);
  writeln('  Area: ', area);
  writeln('  ', Describe('Rectangle', area));

  { Circle }
  x := 3;
  y := 4;
  area := CircleArea(7);
  writeln('Circle at (', x, ',', y, ')');
  writeln('  Radius: 7');
  writeln('  Area: ', area);
  writeln('  ', Describe('Circle', area));
end.
