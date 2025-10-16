# MiniPAS Standard Library

This directory contains the standard library units for the MiniPAS Pascal compiler.

## 📚 Available Units

### **System** - Core System Unit
The fundamental unit automatically included in all programs.

**Features:**
- Basic I/O operations (Write, WriteLn, Read, ReadLn)
- String functions (Length, Copy, Concat, Pos, UpCase, LowerCase, UpperCase)
- Type conversions (IntToStr, StrToInt, FloatToStr, StrToFloat, Chr, Ord)
- Math functions (Abs, Sqr, Sqrt, Sin, Cos, Tan, Round, Trunc, etc.)
- Memory management (New, Dispose, GetMem, FreeMem, SizeOf)
- Program control (Halt, Exit, ParamCount, ParamStr)
- File operations (Assign, Reset, Rewrite, Close, EOF, EOLn)
- Date/time functions (Now, Date, Time, DateToStr, TimeToStr)
- Boolean functions (Odd, Succ, Pred)
- Min/Max functions

**Constants:**
- MaxInt = 2147483647
- Pi = 3.14159265358979323846

### **SysUtils** - System Utilities
Extended system utilities and helper functions.

**Features:**
- **Exception handling**: Exception, EConvertError, EInOutError, EDivByZero, ERangeError
- **String functions**: Trim, TrimLeft, TrimRight, StringReplace, Format, QuotedStr, AnsiUpperCase, AnsiLowerCase, CompareStr, CompareText, SameText
- **File functions**: FileExists, DirectoryExists, DeleteFile, RenameFile, ExtractFileName, ExtractFilePath, ExtractFileExt, ChangeFileExt, ExpandFileName
- **Directory functions**: GetCurrentDir, SetCurrentDir, CreateDir, RemoveDir
- **Date/Time functions**: EncodeDate, EncodeTime, DecodeDate, DecodeTime, DayOfWeek, FormatDateTime
- **Conversion functions**: IntToHex, HexToInt, BoolToStr, StrToBool
- **Math functions**: Power, Log10, Log2, Ceil, Floor
- **Array functions**: High, Low, SetLength
- **Memory functions**: FillChar, Move, CompareMem
- **Miscellaneous**: Random, Randomize, Sleep, GetTickCount

### **Classes** - Object-Oriented Programming
Base classes for object-oriented programming.

**Classes:**
- **TObject**: Base class for all objects
  - Create, Destroy, Free, ClassName
  
- **TList**: Generic pointer list
  - Add, Insert, Delete, Clear, IndexOf, Remove
  - Properties: Count, Capacity, Items
  
- **TStringList**: String list with sorting
  - Add, Insert, Delete, Clear, IndexOf, Sort
  - LoadFromFile, SaveToFile
  - Properties: Count, Strings, Sorted
  
- **TStream**: Abstract stream base class
  - Read, Write, Seek
  - Properties: Position, Size
  
- **TFileStream**: File-based stream
  - Create, Read, Write, Seek
  - Property: FileName
  
- **TMemoryStream**: Memory-based stream
  - Create, Read, Write, Seek, Clear
  - LoadFromFile, SaveToFile
  
- **TComponent**: Component base class
  - Create, Destroy
  - Properties: Name, Owner

### **Math** - Mathematical Functions
Comprehensive mathematical functions and utilities.

**Constants:**
- E = 2.71828182845904523536
- Pi = 3.14159265358979323846
- TwoPi, HalfPi
- RadToDeg, DegToRad

**Functions:**
- **Trigonometric**: Sin, Cos, Tan, ArcSin, ArcCos, ArcTan, ArcTan2
- **Hyperbolic**: SinH, CosH, TanH, ArcSinH, ArcCosH, ArcTanH
- **Exponential/Logarithmic**: Exp, Ln, Log10, Log2, LogN, Power, IntPower
- **Root functions**: Sqrt, Cbrt, Hypot
- **Rounding**: Ceil, Floor, Round, Trunc, Frac, Int, Modf
- **Sign/Absolute**: Sign, Abs
- **Min/Max**: Min, Max, MinValue, MaxValue, MinIntValue, MaxIntValue
- **Comparison**: CompareValue, SameValue, IsZero, IsNaN, IsInfinite
- **Statistical**: Mean, Sum, SumInt, SumOfSquares, StdDev, Variance
- **Angle**: DegToRad, RadToDeg, NormalizeAngle
- **Miscellaneous**: Factorial, Fibonacci, GCD, LCM, IsPrime

## 🚀 Usage Examples

### Basic I/O
```pascal
program HelloWorld;
uses System;

begin
  WriteLn('Hello, World!');
  WriteLn('Enter your name:');
  ReadLn(name);
  WriteLn('Hello, ' + name + '!');
end.
```

### String Manipulation
```pascal
program StringDemo;
uses System, SysUtils;

var
  s: string;
begin
  s := '  Hello World  ';
  WriteLn('Original: "' + s + '"');
  WriteLn('Trimmed: "' + Trim(s) + '"');
  WriteLn('Upper: "' + UpperCase(s) + '"');
  WriteLn('Lower: "' + LowerCase(s) + '"');
end.
```

### Lists and Collections
```pascal
program ListDemo;
uses System, Classes;

var
  list: TStringList;
  i: Integer;
begin
  list := TStringList.Create;
  try
    list.Add('Apple');
    list.Add('Banana');
    list.Add('Cherry');
    list.Sort;
    
    for i := 0 to list.Count - 1 do
      WriteLn(list[i]);
  finally
    list.Free;
  end;
end.
```

### Mathematical Calculations
```pascal
program MathDemo;
uses System, Math;

var
  x, y: Real;
begin
  x := 45.0;
  y := DegToRad(x);
  WriteLn('Sin(45°) = ', Sin(y):0:4);
  WriteLn('Cos(45°) = ', Cos(y):0:4);
  WriteLn('Sqrt(2) = ', Sqrt(2.0):0:4);
  WriteLn('Factorial(5) = ', Factorial(5));
  WriteLn('Fibonacci(10) = ', Fibonacci(10));
end.
```

### File Operations
```pascal
program FileDemo;
uses System, SysUtils, Classes;

var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    list.Add('Line 1');
    list.Add('Line 2');
    list.Add('Line 3');
    list.SaveToFile('output.txt');
    WriteLn('File saved successfully');
    
    list.Clear;
    list.LoadFromFile('output.txt');
    WriteLn('File loaded, lines: ', list.Count);
  finally
    list.Free;
  end;
end.
```

### Exception Handling
```pascal
program ExceptionDemo;
uses System, SysUtils;

var
  x, y: Integer;
begin
  try
    x := 10;
    y := 0;
    WriteLn(x div y);
  except
    on E: EDivByZero do
      WriteLn('Error: Division by zero!');
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end.
```

## 📊 Statistics

```
Total Units: 4
Total Functions: 200+
Total Classes: 8
Total Lines: 1,500+

Coverage:
- I/O Operations: ✅ Complete
- String Functions: ✅ Complete
- Math Functions: ✅ Complete
- File Operations: ✅ Complete
- Collections: ✅ Complete
- Exception Handling: ✅ Complete
- Date/Time: ✅ Complete
```

## 🔧 Implementation Status

### ✅ Completed
- System unit (core functionality)
- SysUtils unit (utilities)
- Classes unit (OOP support)
- Math unit (mathematical functions)

### 🚧 Planned
- Graphics unit (drawing and graphics)
- Threads unit (multi-threading)
- RegEx unit (regular expressions)
- JSON unit (JSON parsing)
- HTTP unit (HTTP client)

## 📝 Notes

- All units follow Free Pascal Compiler (FPC) conventions
- Compatible with Delphi and Turbo Pascal where applicable
- Optimized for the MiniPAS compiler
- Full source code included for transparency

## 🔗 Related Documentation

- [MiniPAS Compiler Documentation](../README.md)
- [Language Reference](../docs/LANGUAGE_REFERENCE.md)
- [API Documentation](../docs/API_DOCUMENTATION.md)

---

*Standard Library Version: 1.0*  
*Last Updated: October 16, 2025*  
*Compatible with: MiniPAS Compiler v1.0+*
