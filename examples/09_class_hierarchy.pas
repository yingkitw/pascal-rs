{ Example 09: Simulated Class Hierarchy with Functions }
{ Demonstrates polymorphic behavior using string-based dispatch }
program ClassHierarchy;
var
  kind: string;
  name: string;
  legs: integer;

function Speak(kind: string): string;
begin
  if kind = 'dog' then
    Speak := 'Woof!'
  else if kind = 'cat' then
    Speak := 'Meow!'
  else if kind = 'bird' then
    Speak := 'Tweet!'
  else
    Speak := '...';
end;

function Description(name: string; legs: integer): string;
begin
  Description := concat(name, ' has ', inttostr(legs), ' legs');
end;

procedure PrintAnimal(kind, name: string; legs: integer);
begin
  writeln(name, ': ', Speak(kind));
  writeln('  ', Description(name, legs));
end;

begin
  writeln('=== Animal Hierarchy ===');

  PrintAnimal('dog', 'Rex', 4);
  PrintAnimal('cat', 'Whiskers', 4);
  PrintAnimal('bird', 'Tweety', 2);

  { Demonstrate string comparisons }
  kind := 'dog';
  if kind = 'dog' then
    writeln('Rex is a dog')
  else
    writeln('Rex is not a dog');

  if kind = 'cat' then
    writeln('ERROR: dog should not be cat')
  else
    writeln('Rex is not a Cat (correct)');
end.
