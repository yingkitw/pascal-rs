# Student Management System

A comprehensive multi-file Pascal project demonstrating advanced Pascal features including:
- Multiple units with dependencies
- Records and classes
- Arrays and dynamic structures
- Functions and procedures
- Modular design patterns

## Project Structure

```
student_management/
├── student.pas      - Student unit (TStudent record, TStudentClass)
├── course.pas       - Course unit (TCourse record, enrollment)
├── grades.pas       - Grades unit (grade calculations, statistics)
├── registry.pas     - Registry unit (student/course management)
├── main.pas         - Main program (demonstrates all features)
└── README.md        - This file
```

## Compilation

### Using the Pascal-RS Compiler:

```bash
# From the project root directory
cargo build --release -p pascal-cli

# Compile the main program (will automatically compile dependencies)
./target/release/pascal compile projects/student_management/main.pas -v

# Or compile with assembly output
./target/release/pascal compile projects/student_management/main.pas -S

# Or compile with optimization
./target/release/pascal compile projects/student_management/main.pas -O2 -v
```

## Features Demonstrated

### Unit Dependencies:
- `main.pas` uses `SysUtils`, `Student`, `Course`, `Grades`, `Registry`
- `Student` uses `SysUtils`, `System`
- `Course` uses `SysUtils`, `System`
- `Grades` uses `SysUtils`, `Course`, `System`, `Classes`
- `Registry` uses `SysUtils`, `Student`, `Course`, `System`, `Classes`

### Data Structures:
- Records (TStudent, TCourse, TEnrollment, TGradeStats)
- Arrays (TGradeBook, Student/Course arrays)
- Pointer types (PGradeBook)
- Enumerated types (TGradeLevel, TCourseLevel)

### OOP Features:
- Classes (TStudentClass)
- Constructors and destructors
- Methods (GetFullName, SetGPA, Promote, IsHonors)
- Inheritance

### Algorithms:
- GPA calculation
- Grade statistics (average, median, standard deviation)
- Search operations (FindStudent, FindCourse)
- Sorting (bubble sort in median calculation)

### Control Structures:
- For loops
- While loops
- If-then-else statements
- Case statements
- With statements

## Program Output

The program demonstrates:
1. System initialization
2. Student creation and management
3. Course creation and enrollment
4. OOP student operations
5. Grade calculations and statistics
6. Search operations
7. Full system reporting
8. Conditional logic demonstration

Expected output shows:
- Student registry with GPAs and grade levels
- Course catalog with enrollment counts
- Grade statistics (averages, pass/fail counts)
- Academic honors status
- System summary reports
