unit Registry;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Student, Course;

type
  { Student registry - manages students and their enrollments }
  TStudentRegistry = record
    Students: array[1..50] of TStudent;
    StudentCount: Integer;
  end;

  { Course catalog }
  TCourseCatalog = record
    Courses: array[1..30] of TCourse;
    CourseCount: Integer;
  end;

{ Registry management }
function CreateRegistry: TStudentRegistry;
function AddStudent(var Registry: TStudentRegistry; const S: TStudent): Integer;
function FindStudent(const Registry: TStudentRegistry; const ID: string): Integer;
procedure RemoveStudent(var Registry: TStudentRegistry; Index: Integer);
procedure ListAllStudents(const Registry: TStudentRegistry);

{ Catalog management }
function CreateCatalog: TCourseCatalog;
function AddCourse(var Catalog: TCourseCatalog; const C: TCourse): Integer;
function FindCourse(const Catalog: TCourseCatalog; const Code: string): Integer;
procedure ListAllCourses(const Catalog: TCourseCatalog);
procedure ListAvailableCourses(const Catalog: TCourseCatalog);

{ Reporting procedures }
procedure PrintStudentSummary(const Registry: TStudentRegistry);
procedure PrintCourseSummary(const Catalog: TCourseCatalog);
procedure PrintFullReport(const Registry: TStudentRegistry;
  const Catalog: TCourseCatalog);

implementation

uses
  System, Classes;

{ Registry management }

function CreateRegistry: TStudentRegistry;
begin
  Result.StudentCount := 0;
end;

function AddStudent(var Registry: TStudentRegistry; const S: TStudent): Integer;
begin
  if Registry.StudentCount < 50 then
  begin
    Registry.StudentCount := Registry.StudentCount + 1;
    Registry.Students[Registry.StudentCount] := S;
    Result := Registry.StudentCount;
  end
  else
    Result := -1; { Registry full }
end;

function FindStudent(const Registry: TStudentRegistry; const ID: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 1 to Registry.StudentCount do
    if Registry.Students[I].ID = ID then
    begin
      Result := I;
      Exit;
    end;
end;

procedure RemoveStudent(var Registry: TStudentRegistry; Index: Integer);
var
  I: Integer;
begin
  if (Index >= 1) and (Index <= Registry.StudentCount) then
  begin
    for I := Index to Registry.StudentCount - 1 do
      Registry.Students[I] := Registry.Students[I + 1];
    Registry.StudentCount := Registry.StudentCount - 1;
  end;
end;

procedure ListAllStudents(const Registry: TStudentRegistry);
var
  I: Integer;
begin
  WriteLn('=== Student Registry ===');
  WriteLn('Total Students: ', Registry.StudentCount);
  WriteLn;

  for I := 1 to Registry.StudentCount do
  begin
    WriteLn('[', I, '] ', StudentToString(Registry.Students[I]));
  end;
end;

{ Catalog management }

function CreateCatalog: TCourseCatalog;
begin
  Result.CourseCount := 0;
end;

function AddCourse(var Catalog: TCourseCatalog; const C: TCourse): Integer;
begin
  if Catalog.CourseCount < 30 then
  begin
    Catalog.CourseCount := Catalog.CourseCount + 1;
    Catalog.Courses[Catalog.CourseCount] := C;
    Result := Catalog.CourseCount;
  end
  else
    Result := -1; { Catalog full }
end;

function FindCourse(const Catalog: TCourseCatalog; const Code: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 1 to Catalog.CourseCount do
    if Catalog.Courses[I].Code = Code then
    begin
      Result := I;
      Exit;
    end;
end;

procedure ListAllCourses(const Catalog: TCourseCatalog);
var
  I: Integer;
begin
  WriteLn('=== Course Catalog ===');
  WriteLn('Total Courses: ', Catalog.CourseCount);
  WriteLn;

  for I := 1 to Catalog.CourseCount do
  begin
    WriteLn('[', I, '] ', CourseToString(Catalog.Courses[I]));
    WriteLn('     Fullness: ', GetCourseFullness(Catalog.Courses[I]):0:1, '%');
  end;
end;

procedure ListAvailableCourses(const Catalog: TCourseCatalog);
var
  I: Integer;
begin
  WriteLn('=== Available Courses ===');
  for I := 1 to Catalog.CourseCount do
  begin
    if CanEnroll(Catalog.Courses[I]) then
      WriteLn('[OPEN]  ', CourseToString(Catalog.Courses[I]))
    else
      WriteLn('[FULL]  ', CourseToString(Catalog.Courses[I]));
  end;
end;

{ Reporting procedures }

procedure PrintStudentSummary(const Registry: TStudentRegistry);
var
  I, ActiveCount, HonorsCount: Integer;
  TotalGPA: Real;
begin
  ActiveCount := 0;
  HonorsCount := 0;
  TotalGPA := 0.0;

  for I := 1 to Registry.StudentCount do
  begin
    if Registry.Students[I].IsActive then
    begin
      ActiveCount := ActiveCount + 1;
      TotalGPA := TotalGPA + Registry.Students[I].GPA;
      if Registry.Students[I].GPA >= 3.5 then
        HonorsCount := HonorsCount + 1;
    end;
  end;

  WriteLn('=== Student Summary ===');
  WriteLn('Total Students: ', Registry.StudentCount);
  WriteLn('Active Students: ', ActiveCount);
  WriteLn('Honors Students: ', HonorsCount);

  if ActiveCount > 0 then
    WriteLn('Average GPA: ', (TotalGPA / ActiveCount):0:2);
end;

procedure PrintCourseSummary(const Catalog: TCourseCatalog);
var
  I, ActiveCount: Integer;
  TotalEnrolled, TotalCapacity: Integer;
begin
  ActiveCount := 0;
  TotalEnrolled := 0;
  TotalCapacity := 0;

  for I := 1 to Catalog.CourseCount do
  begin
    if Catalog.Courses[I].IsActive then
    begin
      ActiveCount := ActiveCount + 1;
      TotalEnrolled := TotalEnrolled + Catalog.Courses[I].EnrolledCount;
      TotalCapacity := TotalCapacity + Catalog.Courses[I].MaxStudents;
    end;
  end;

  WriteLn('=== Course Summary ===');
  WriteLn('Total Courses: ', Catalog.CourseCount);
  WriteLn('Active Courses: ', ActiveCount);
  WriteLn('Total Enrolled: ', TotalEnrolled);
  WriteLn('Total Capacity: ', TotalCapacity);

  if TotalCapacity > 0 then
    WriteLn('Overall Fill Rate: ', ((TotalEnrolled / TotalCapacity) * 100.0):0:1, '%');
end;

procedure PrintFullReport(const Registry: TStudentRegistry;
  const Catalog: TCourseCatalog);
begin
  WriteLn;
  WriteLn('========================================');
  WriteLn('   STUDENT MANAGEMENT SYSTEM REPORT');
  WriteLn('========================================');
  WriteLn;

  PrintStudentSummary(Registry);
  WriteLn;

  PrintCourseSummary(Catalog);
  WriteLn;

  WriteLn('========================================');
end;

end.
