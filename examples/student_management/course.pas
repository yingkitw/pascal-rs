unit Course;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  { Course codes and types }
  TCourseCode = string[10];
  TCourseName = string[50];
  TCredits = 0..6;

  { Course level }
  TCourseLevel = (Introductory, Intermediate, Advanced, Graduate);

  { Course record }
  TCourse = record
    Code: TCourseCode;
    Name: TCourseName;
    Credits: TCredits;
    Level: TCourseLevel;
    MaxStudents: Integer;
    EnrolledCount: Integer;
    IsActive: Boolean;
  end;

  { Enrollment record }
  TEnrollment = record
    StudentID: string[10];
    CourseCode: TCourseCode;
    Grade: 0.0..100.0;
    LetterGrade: Char;
    IsCompleted: Boolean;
  end;

{ Course management functions }
function CreateCourse(const Code, Name: string; Credits: TCredits;
  Level: TCourseLevel; MaxStudents: Integer): TCourse;
function CanEnroll(const C: TCourse): Boolean;
procedure EnrollStudent(var C: TCourse);
procedure DropStudent(var C: TCourse);

{ Enrollment functions }
function CreateEnrollment(const StudentID, CourseCode: string): TEnrollment;
procedure SetGrade(var E: TEnrollment; Grade: 0.0..100.0);
function GetLetterGrade(Grade: 0.0..100.0): Char;
function IsPassing(const E: TEnrollment): Boolean;

{ Utility functions }
function CourseToString(const C: TCourse): string;
function GetCourseFullness(const C: TCourse): Real;

implementation

uses
  System;

{ Course management functions }

function CreateCourse(const Code, Name: string; Credits: TCredits;
  Level: TCourseLevel; MaxStudents: Integer): TCourse;
begin
  Result.Code := Code;
  Result.Name := Name;
  Result.Credits := Credits;
  Result.Level := Level;
  Result.MaxStudents := MaxStudents;
  Result.EnrolledCount := 0;
  Result.IsActive := True;
end;

function CanEnroll(const C: TCourse): Boolean;
begin
  Result := C.IsActive and (C.EnrolledCount < C.MaxStudents);
end;

procedure EnrollStudent(var C: TCourse);
begin
  if CanEnroll(C) then
    C.EnrolledCount := C.EnrolledCount + 1;
end;

procedure DropStudent(var C: TCourse);
begin
  if C.EnrolledCount > 0 then
    C.EnrolledCount := C.EnrolledCount - 1;
end;

{ Enrollment functions }

function CreateEnrollment(const StudentID, CourseCode: string): TEnrollment;
begin
  Result.StudentID := StudentID;
  Result.CourseCode := CourseCode;
  Result.Grade := 0.0;
  Result.LetterGrade := 'F';
  Result.IsCompleted := False;
end;

procedure SetGrade(var E: TEnrollment; Grade: 0.0..100.0);
begin
  E.Grade := Grade;
  E.LetterGrade := GetLetterGrade(Grade);
  E.IsCompleted := True;
end;

function GetLetterGrade(Grade: 0.0..100.0): Char;
begin
  if Grade >= 90.0 then
    Result := 'A'
  else if Grade >= 80.0 then
    Result := 'B'
  else if Grade >= 70.0 then
    Result := 'C'
  else if Grade >= 60.0 then
    Result := 'D'
  else
    Result := 'F';
end;

function IsPassing(const E: TEnrollment): Boolean;
begin
  Result := E.Grade >= 60.0;
end;

{ Utility functions }

function CourseToString(const C: TCourse): string;
var
  LevelStr: string;
begin
  case C.Level of
    Introductory: LevelStr := 'Intro';
    Intermediate: LevelStr := 'Interm';
    Advanced: LevelStr := 'Adv';
    Graduate: LevelStr := 'Grad';
  end;

  Result := C.Code + ': ' + C.Name + ' (' + LevelStr + ')' +
            ' [' + IntToStr(C.EnrolledCount) + '/' + IntToStr(C.MaxStudents) + ']';
end;

function GetCourseFullness(const C: TCourse): Real;
begin
  if C.MaxStudents > 0 then
    Result := (C.EnrolledCount / C.MaxStudents) * 100.0
  else
    Result := 0.0;
end;

end.
