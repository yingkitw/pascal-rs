unit Grades;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Course;

type
  { Grade book - collection of enrollments }
  TGradeBook = array[1..100] of TEnrollment;
  PGradeBook = ^TGradeBook;

  { Grade statistics }
  TGradeStats = record
    Count: Integer;
    Average: Real;
    Highest: Real;
    Lowest: Real;
    PassCount: Integer;
    FailCount: Integer;
  end;

{ Grade calculation functions }
function CalculateGPA(const Grades: array of Real): Real;
function CalculateAverage(const Grades: array of Real): Real;
function CalculateMedian(Grades: array of Real): Real;
function CalculateStandardDeviation(const Grades: array of Real): Real;

{ Grade statistics }
function CalculateGradeStats(const GradeBook: TGradeBook; Count: Integer): TGradeStats;
procedure PrintGradeStats(const Stats: TGradeStats);

{ Grade utilities }
function ConvertTo4PointScale(LetterGrade: Char): Real;
function IsDeanList(GPA: Real): Boolean;
function IsAcademicProbation(GPA: Real): Boolean;
function GetHonorsStatus(GPA: Real): string;

implementation

uses
  System;

{ Helper function to sum array }
function SumArray(const Grades: array of Real): Real;
var
  I: Integer;
begin
  Result := 0.0;
  for I := Low(Grades) to High(Grades) do
    Result := Result + Grades[I];
end;

{ Grade calculation functions }

function CalculateGPA(const Grades: array of Real): Real;
var
  I, Count: Integer;
  TotalPoints: Real;
begin
  TotalPoints := 0.0;
  Count := 0;

  for I := Low(Grades) to High(Grades) do
  begin
    TotalPoints := TotalPoints + ConvertTo4PointScale(GetLetterGrade(Grades[I]));
    Count := Count + 1;
  end;

  if Count > 0 then
    Result := TotalPoints / Count
  else
    Result := 0.0;
end;

function CalculateAverage(const Grades: array of Real): Real;
var
  Count: Integer;
begin
  Count := Length(Grades);
  if Count > 0 then
    Result := SumArray(Grades) / Count
  else
    Result := 0.0;
end;

function CalculateMedian(Grades: array of Real): Real;
var
  I, J: Integer;
  Temp: Real;
begin
  { Simple bubble sort }
  for I := Low(Grades) to High(Grades) - 1 do
    for J := I + 1 to High(Grades) do
      if Grades[I] > Grades[J] then
      begin
        Temp := Grades[I];
        Grades[I] := Grades[J];
        Grades[J] := Temp;
      end;

  { Calculate median }
  I := Length(Grades);
  if (I mod 2) = 1 then
    Result := Grades[I div 2]
  else
    Result := (Grades[(I div 2) - 1] + Grades[I div 2]) / 2.0;
end;

function CalculateStandardDeviation(const Grades: array of Real): Real;
var
  I, Count: Integer;
  Mean, SumSquaredDiff, Diff: Real;
begin
  Count := Length(Grades);
  if Count = 0 then
  begin
    Result := 0.0;
    Exit;
  end;

  Mean := CalculateAverage(Grades);
  SumSquaredDiff := 0.0;

  for I := Low(Grades) to High(Grades) do
  begin
    Diff := Grades[I] - Mean;
    SumSquaredDiff := SumSquaredDiff + (Diff * Diff);
  end;

  Result := Sqrt(SumSquaredDiff / Count);
end;

{ Grade statistics }

function CalculateGradeStats(const GradeBook: TGradeBook; Count: Integer): TGradeStats;
var
  I: Integer;
  GradesArray: array[0..99] of Real;
  Total, Sum: Real;
begin
  Result.Count := 0;
  Result.Average := 0.0;
  Result.Highest := 0.0;
  Result.Lowest := 100.0;
  Result.PassCount := 0;
  Result.FailCount := 0;

  Sum := 0.0;

  for I := 1 to Count do
  begin
    if GradeBook[I].IsCompleted then
    begin
      GradesArray[Result.Count] := GradeBook[I].Grade;
      Sum := Sum + GradeBook[I].Grade;

      if GradeBook[I].Grade > Result.Highest then
        Result.Highest := GradeBook[I].Grade;

      if GradeBook[I].Grade < Result.Lowest then
        Result.Lowest := GradeBook[I].Grade;

      if IsPassing(GradeBook[I]) then
        Result.PassCount := Result.PassCount + 1
      else
        Result.FailCount := Result.FailCount + 1;

      Result.Count := Result.Count + 1;
    end;
  end;

  if Result.Count > 0 then
    Result.Average := Sum / Result.Count;
end;

procedure PrintGradeStats(const Stats: TGradeStats);
begin
  WriteLn('=== Grade Statistics ===');
  WriteLn('Total Grades: ', Stats.Count);
  WriteLn('Average: ', Stats.Average:0:2);
  WriteLn('Highest: ', Stats.Highest:0:2);
  WriteLn('Lowest: ', Stats.Lowest:0:2);
  WriteLn('Passed: ', Stats.PassCount);
  WriteLn('Failed: ', Stats.FailCount);
end;

{ Grade utilities }

function ConvertTo4PointScale(LetterGrade: Char): Real;
begin
  case LetterGrade of
    'A': Result := 4.0;
    'B': Result := 3.0;
    'C': Result := 2.0;
    'D': Result := 1.0;
    'F': Result := 0.0;
    else
      Result := 0.0;
  end;
end;

function IsDeanList(GPA: Real): Boolean;
begin
  Result := GPA >= 3.5;
end;

function IsAcademicProbation(GPA: Real): Boolean;
begin
  Result := GPA < 2.0;
end;

function GetHonorsStatus(GPA: Real): string;
begin
  if GPA >= 3.8 then
    Result := 'High Honors'
  else if GPA >= 3.5 then
    Result := 'Dean''s List'
  else if GPA >= 3.0 then
    Result := 'Honors'
  else if GPA < 2.0 then
    Result := 'Academic Probation'
  else
    Result := 'Good Standing';
end;

end.
