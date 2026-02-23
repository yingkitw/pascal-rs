unit ExampleUnit;

interface

uses
  SysUtils,
  Classes;

type
  TPerson = class
  private
    FName: String;
    FAge: Integer;
  public
    constructor Create(Name: String; Age: Integer);
    function GetName: String;
    procedure SetName(Name: String);
    function GetAge: Integer;
    procedure SetAge(Age: Integer);
  end;

procedure PrintPerson(Person: TPerson);

implementation

{ TPerson }

constructor TPerson.Create(Name: String; Age: Integer);
begin
  inherited Create;
  FName := Name;
  FAge := Age;
end;

function TPerson.GetName: String;
begin
  Result := FName;
end;

procedure TPerson.SetName(Name: String);
begin
  FName := Name;
end;

function TPerson.GetAge: Integer;
begin
  Result := FAge;
end;

procedure TPerson.SetAge(Age: Integer);
begin
  FAge := Age;
end;

procedure PrintPerson(Person: TPerson);
begin
  writeln('Name: ', Person.GetName);
  writeln('Age: ', Person.GetAge);
end;

end.