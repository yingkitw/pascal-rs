program StudentManagementSystem;

{$mode objfpc}{$H+}

uses
  SysUtils, Student, Course, Grades, Registry;

var
  { Registry and Catalog }
  SchoolRegistry: TStudentRegistry;
  CourseCatalog: TCourseCatalog;

  { Sample students }
  Student1, Student2, Student3, Student4, Student5: TStudent;

  { Sample courses }
  Course1, Course2, Course3, Course4, Course5: TCourse;

  { Grade book for testing }
  GradeBook: TGradeBook;
  GradeStats: TGradeStats;

  { Test grades array }
  TestGrades: array[1..5] of Real;
  CalculatedGPA: Real;

  { Student class for OOP demonstration }
  HonorStudent: TStudentClass;

  { Loop variables }
  I: Integer;

begin
  WriteLn('=================================================');
  WriteLn('  Student Management System - Pascal Demo');
  WriteLn('=================================================');
  WriteLn;

  { ========================================
    STEP 1: Initialize Registry and Catalog
    ======================================== }
  WriteLn('Step 1: Initializing System...');
  SchoolRegistry := CreateRegistry;
  CourseCatalog := CreateCatalog;
  WriteLn('Registry and Catalog initialized.');
  WriteLn;

  { ========================================
    STEP 2: Create and Add Students
    ======================================== }
  WriteLn('Step 2: Creating Students...');

  { Create students using CreateStudent function }
  Student1 := CreateStudent('S001', 'John', 'Smith', Freshman, 2005);
  Student1.GPA := 3.8;

  Student2 := CreateStudent('S002', 'Jane', 'Doe', Sophomore, 2004);
  Student2.GPA := 3.5;

  Student3 := CreateStudent('S003', 'Bob', 'Johnson', Junior, 2003);
  Student3.GPA := 3.2;

  Student4 := CreateStudent('S004', 'Alice', 'Williams', Senior, 2002);
  Student4.GPA := 3.9;

  Student5 := CreateStudent('S005', 'Charlie', 'Brown', Freshman, 2005);
  Student5.GPA := 2.7;

  { Add students to registry }
  AddStudent(SchoolRegistry, Student1);
  AddStudent(SchoolRegistry, Student2);
  AddStudent(SchoolRegistry, Student3);
  AddStudent(SchoolRegistry, Student4);
  AddStudent(SchoolRegistry, Student5);

  WriteLn('Added 5 students to registry.');
  WriteLn;

  { ========================================
    STEP 3: Create and Add Courses
    ======================================== }
  WriteLn('Step 3: Creating Courses...');

  { Create courses }
  Course1 := CreateCourse('CS101', 'Introduction to Programming', 3, Introductory, 30);
  EnrollStudent(Course1);
  EnrollStudent(Course1);
  EnrollStudent(Course1);

  Course2 := CreateCourse('CS201', 'Data Structures', 4, Intermediate, 25);
  EnrollStudent(Course2);
  EnrollStudent(Course2);

  Course3 := CreateCourse('CS301', 'Algorithms', 4, Advanced, 20);
  EnrollStudent(Course3);

  Course4 := CreateCourse('MATH101', 'Calculus I', 4, Introductory, 35);
  EnrollStudent(Course4);
  EnrollStudent(Course4);
  EnrollStudent(Course4);
  EnrollStudent(Course4);

  Course5 := CreateCourse('MATH201', 'Calculus II', 4, Intermediate, 30);
  EnrollStudent(Course5);
  EnrollStudent(Course5);

  { Add courses to catalog }
  AddCourse(CourseCatalog, Course1);
  AddCourse(CourseCatalog, Course2);
  AddCourse(CourseCatalog, Course3);
  AddCourse(CourseCatalog, Course4);
  AddCourse(CourseCatalog, Course5);

  WriteLn('Added 5 courses to catalog.');
  WriteLn;

  { ========================================
    STEP 4: Demonstrate Student Operations
    ======================================== }
  WriteLn('Step 4: Student Operations Demo...');
  WriteLn;

  { Create a student class instance for OOP demo }
  HonorStudent := TStudentClass.Create('S006', 'Emma', 'Davis', Sophomore);
  HonorStudent.SetGPA(3.95);
  HonorStudent.Promote;

  WriteLn('--- OOP Student Demo ---');
  WriteLn('Student Name: ', HonorStudent.GetFullName);
  WriteLn('Student ID: ', HonorStudent.GetFormattedID);
  WriteLn('Student GPA: ', HonorStudent.GetGPA:0:2);
  WriteLn('Is Honors: ', HonorStudent.IsHonors);
  WriteLn;

  { ========================================
    STEP 5: List All Students
    ======================================== }
  WriteLn('Step 5: Listing All Students...');
  WriteLn;
  ListAllStudents(SchoolRegistry);
  WriteLn;

  { ========================================
    STEP 6: List All Courses
    ======================================== }
  WriteLn('Step 6: Listing All Courses...');
  WriteLn;
  ListAllCourses(CourseCatalog);
  WriteLn;

  { ========================================
    STEP 7: Grade Calculations
    ======================================== }
  WriteLn('Step 7: Grade Calculations Demo...');
  WriteLn;

  { Create some test grades }
  TestGrades[1] := 95.0;
  TestGrades[2] := 87.5;
  TestGrades[3] := 92.0;
  TestGrades[4] := 78.5;
  TestGrades[5] := 88.0;

  { Create grade book entries }
  for I := 1 to 5 do
  begin
    GradeBook[I] := CreateEnrollment('S00' + IntToStr(I), 'CS101');
    SetGrade(GradeBook[I], TestGrades[I]);
  end;

  { Calculate statistics }
  GradeStats := CalculateGradeStats(GradeBook, 5);
  PrintGradeStats(GradeStats);
  WriteLn;

  { Calculate GPA }
  CalculatedGPA := CalculateGPA(TestGrades);
  WriteLn('Calculated GPA: ', CalculatedGPA:0:2);
  WriteLn('Honors Status: ', GetHonorsStatus(CalculatedGPA));
  WriteLn;

  { Calculate average }
  WriteLn('Average Grade: ', CalculateAverage(TestGrades):0:2);
  WriteLn;

  { ========================================
    STEP 8: Search Operations
    ======================================== }
  WriteLn('Step 8: Search Operations Demo...');
  WriteLn;

  { Find a student }
  I := FindStudent(SchoolRegistry, 'S003');
  if I > 0 then
  begin
    WriteLn('Found student S003: ');
    WriteLn('  ', StudentToString(SchoolRegistry.Students[I]));
  end;

  { Find a course }
  I := FindCourse(CourseCatalog, 'MATH201');
  if I > 0 then
  begin
    WriteLn('Found course MATH201: ');
    WriteLn('  ', CourseToString(CourseCatalog.Courses[I]));
  end;
  WriteLn;

  { ========================================
    STEP 9: Full System Report
    ======================================== }
  PrintFullReport(SchoolRegistry, CourseCatalog);

  { ========================================
    STEP 10: Demonstrate Conditional Logic
    ======================================== }
  WriteLn('Step 10: Conditional Logic Demo...');
  WriteLn;

  for I := 1 to SchoolRegistry.StudentCount do
  begin
    with SchoolRegistry.Students[I] do
    begin
      Write(StudentToString(SchoolRegistry.Students[I]));
      Write(' - ');

      if GPA >= 3.5 then
        WriteLn('EXCELLENT - Dean''s List!')
      else if GPA >= 3.0 then
        WriteLn('GOOD - Honors')
      else if GPA >= 2.0 then
        WriteLn('SATISFACTORY')
      else
        WriteLn('NEEDS IMPROVEMENT - Academic Probation');
    end;
  end;
  WriteLn;

  { ========================================
    STEP 11: Clean Up
    ======================================== }
  WriteLn('Step 11: Cleaning up...');
  HonorStudent.Free;
  WriteLn('Memory cleaned up.');
  WriteLn;

  WriteLn('=================================================');
  WriteLn('  Program Completed Successfully!');
  WriteLn('=================================================');
end.
