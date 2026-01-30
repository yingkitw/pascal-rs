unit Student;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  { Student record for basic information }
  TStudentID = string[10];
  TName = string[50];
  TGPA = 0.0..4.0;

  { Student grade level }
  TGradeLevel = (Freshman, Sophomore, Junior, Senior);

  { Student record }
  TStudent = record
    ID: TStudentID;
    FirstName: TName;
    LastName: TName;
    GradeLevel: TGradeLevel;
    GPA: TGPA;
    BirthYear: Integer;
    IsActive: Boolean;
  end;

  { Student class with methods }
  TStudentClass = class
  private
    FStudent: TStudent;
  public
    constructor Create(const ID, FirstName, LastName: string; Level: TGradeLevel);
    destructor Destroy; override;

    procedure SetGPA(NewGPA: TGPA);
    function GetGPA: TGPA;
    function GetFullName: string;
    function GetFormattedID: string;
    function GetAge: Integer;
    procedure Promote;
    function IsHonors: Boolean;
  end;

{ Utility functions }
function CreateStudent(const ID, FirstName, LastName: string;
  Level: TGradeLevel; BirthYear: Integer): TStudent;
function StudentToString(const S: TStudent): string;
function CompareByGPA(const S1, S2: TStudent): Integer;

implementation

uses
  System;

{ TStudentClass }

constructor TStudentClass.Create(const ID, FirstName, LastName: string;
  Level: TGradeLevel);
begin
  inherited Create;
  FStudent.ID := ID;
  FStudent.FirstName := FirstName;
  FStudent.LastName := LastName;
  FStudent.GradeLevel := Level;
  FStudent.GPA := 0.0;
  FStudent.BirthYear := 2000;
  FStudent.IsActive := True;
end;

destructor TStudentClass.Destroy;
begin
  inherited Destroy;
end;

procedure TStudentClass.SetGPA(NewGPA: TGPA);
begin
  FStudent.GPA := NewGPA;
end;

function TStudentClass.GetGPA: TGPA;
begin
  Result := FStudent.GPA;
end;

function TStudentClass.GetFullName: string;
begin
  Result := FStudent.FirstName + ' ' + FStudent.LastName;
end;

function TStudentClass.GetFormattedID: string;
begin
  Result := 'ID:' + FStudent.ID;
end;

function TStudentClass.GetAge: Integer;
begin
  Result := 2024 - FStudent.BirthYear;
end;

procedure TStudentClass.Promote;
begin
  if FStudent.GradeLevel < Senior then
    FStudent.GradeLevel := Succ(FStudent.GradeLevel);
end;

function TStudentClass.IsHonors: Boolean;
begin
  Result := FStudent.GPA >= 3.5;
end;

{ Utility functions }

function CreateStudent(const ID, FirstName, LastName: string;
  Level: TGradeLevel; BirthYear: Integer): TStudent;
begin
  Result.ID := ID;
  Result.FirstName := FirstName;
  Result.LastName := LastName;
  Result.GradeLevel := Level;
  Result.GPA := 0.0;
  Result.BirthYear := BirthYear;
  Result.IsActive := True;
end;

function StudentToString(const S: TStudent): string;
var
  LevelStr: string;
begin
  case S.GradeLevel of
    Freshman: LevelStr := 'Freshman';
    Sophomore: LevelStr := 'Sophomore';
    Junior: LevelStr := 'Junior';
    Senior: LevelStr := 'Senior';
  end;

  Result := 'ID: ' + S.ID + ', Name: ' + S.FirstName + ' ' + S.LastName +
            ', Grade: ' + LevelStr + ', GPA: ' + FloatToStr(S.GPA);
end;

function CompareByGPA(const S1, S2: TStudent): Integer;
begin
  if S1.GPA > S2.GPA then
    Result := 1
  else if S1.GPA < S2.GPA then
    Result := -1
  else
    Result := 0;
end;

end.
