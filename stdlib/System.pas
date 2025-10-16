unit System;

{$mode objfpc}{$H+}

interface

{ Basic types }
type
  PChar = ^Char;
  PInteger = ^Integer;
  PReal = ^Real;
  PBoolean = ^Boolean;
  
  { String types }
  ShortString = string[255];
  AnsiString = string;
  
  { Pointer types }
  Pointer = ^Byte;
  
  { Text file type }
  Text = file of Char;

{ I/O Procedures and Functions }
procedure Write(const s: string); external;
procedure WriteLn(const s: string); external;
procedure Read(var c: Char); external;
procedure ReadLn(var s: string); external;

{ String Functions }
function Length(const s: string): Integer; external;
function Copy(const s: string; index, count: Integer): string; external;
function Concat(const s1, s2: string): string; external;
function Pos(const substr, s: string): Integer; external;
function UpCase(c: Char): Char; external;
function LowerCase(const s: string): string; external;
function UpperCase(const s: string): string; external;

{ Type Conversion Functions }
function IntToStr(i: Integer): string; external;
function StrToInt(const s: string): Integer; external;
function FloatToStr(f: Real): string; external;
function StrToFloat(const s: string): Real; external;
function Chr(i: Integer): Char; external;
function Ord(c: Char): Integer; external;

{ Math Functions }
function Abs(x: Integer): Integer; overload; external;
function Abs(x: Real): Real; overload; external;
function Sqr(x: Integer): Integer; overload; external;
function Sqr(x: Real): Real; overload; external;
function Sqrt(x: Real): Real; external;
function Sin(x: Real): Real; external;
function Cos(x: Real): Real; external;
function Tan(x: Real): Real; external;
function ArcTan(x: Real): Real; external;
function Ln(x: Real): Real; external;
function Exp(x: Real): Real; external;
function Round(x: Real): Integer; external;
function Trunc(x: Real): Integer; external;
function Frac(x: Real): Real; external;
function Int(x: Real): Real; external;

{ Memory Management }
procedure New(var p: Pointer); external;
procedure Dispose(var p: Pointer); external;
procedure GetMem(var p: Pointer; size: Integer); external;
procedure FreeMem(var p: Pointer); external;
function SizeOf(x: any): Integer; external;

{ Program Control }
procedure Halt; overload; external;
procedure Halt(exitCode: Integer); overload; external;
procedure Exit; external;
function ParamCount: Integer; external;
function ParamStr(index: Integer): string; external;

{ File Operations }
procedure Assign(var f: Text; const name: string); external;
procedure Reset(var f: Text); external;
procedure Rewrite(var f: Text); external;
procedure Close(var f: Text); external;
function EOF(var f: Text): Boolean; external;
function EOLn(var f: Text): Boolean; external;

{ Date/Time Functions }
type
  TDateTime = Real;

function Now: TDateTime; external;
function Date: TDateTime; external;
function Time: TDateTime; external;
function DateToStr(d: TDateTime): string; external;
function TimeToStr(t: TDateTime): string; external;
function DateTimeToStr(dt: TDateTime): string; external;

{ Boolean Functions }
function Odd(x: Integer): Boolean; external;
function Succ(x: Integer): Integer; overload; external;
function Succ(c: Char): Char; overload; external;
function Pred(x: Integer): Integer; overload; external;
function Pred(c: Char): Char; overload; external;

{ Min/Max Functions }
function Min(a, b: Integer): Integer; overload; external;
function Min(a, b: Real): Real; overload; external;
function Max(a, b: Integer): Integer; overload; external;
function Max(a, b: Real): Real; overload; external;

{ Constants }
const
  MaxInt = 2147483647;
  MaxLongInt = 2147483647;
  Pi = 3.14159265358979323846;
  
implementation

end.
