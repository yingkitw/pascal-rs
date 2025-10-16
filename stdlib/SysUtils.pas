unit SysUtils;

{$mode objfpc}{$H+}

interface

uses
  System;

{ Exception handling }
type
  Exception = class
  private
    FMessage: string;
  public
    constructor Create(const Msg: string);
    property Message: string read FMessage;
  end;
  
  EConvertError = class(Exception);
  EInOutError = class(Exception);
  EDivByZero = class(Exception);
  ERangeError = class(Exception);

{ String functions }
function Trim(const s: string): string;
function TrimLeft(const s: string): string;
function TrimRight(const s: string): string;
function StringReplace(const s, oldPattern, newPattern: string): string;
function Format(const fmt: string; const args: array of const): string;
function QuotedStr(const s: string): string;
function AnsiUpperCase(const s: string): string;
function AnsiLowerCase(const s: string): string;
function CompareStr(const s1, s2: string): Integer;
function CompareText(const s1, s2: string): Integer;
function SameText(const s1, s2: string): Boolean;

{ File functions }
function FileExists(const fileName: string): Boolean;
function DirectoryExists(const dirName: string): Boolean;
function DeleteFile(const fileName: string): Boolean;
function RenameFile(const oldName, newName: string): Boolean;
function ExtractFileName(const fileName: string): string;
function ExtractFilePath(const fileName: string): string;
function ExtractFileExt(const fileName: string): string;
function ChangeFileExt(const fileName, extension: string): string;
function ExpandFileName(const fileName: string): string;

{ Directory functions }
function GetCurrentDir: string;
function SetCurrentDir(const dir: string): Boolean;
function CreateDir(const dir: string): Boolean;
function RemoveDir(const dir: string): Boolean;

{ Date/Time functions }
function EncodeDate(year, month, day: Word): TDateTime;
function EncodeTime(hour, min, sec, msec: Word): TDateTime;
procedure DecodeDate(dt: TDateTime; var year, month, day: Word);
procedure DecodeTime(dt: TDateTime; var hour, min, sec, msec: Word);
function DayOfWeek(dt: TDateTime): Integer;
function FormatDateTime(const fmt: string; dt: TDateTime): string;

{ Conversion functions }
function IntToHex(value: Integer; digits: Integer): string;
function HexToInt(const hexStr: string): Integer;
function BoolToStr(b: Boolean): string;
function StrToBool(const s: string): Boolean;

{ Math functions }
function Power(base, exponent: Real): Real;
function Log10(x: Real): Real;
function Log2(x: Real): Real;
function Ceil(x: Real): Integer;
function Floor(x: Real): Integer;

{ Array functions }
function High(const arr: array of Integer): Integer;
function Low(const arr: array of Integer): Integer;
procedure SetLength(var arr: array of Integer; newLength: Integer);

{ Memory functions }
procedure FillChar(var x; count: Integer; value: Byte);
procedure Move(const source; var dest; count: Integer);
function CompareMem(const buf1, buf2; count: Integer): Boolean;

{ Miscellaneous }
function Random: Real; overload;
function Random(range: Integer): Integer; overload;
procedure Randomize;
procedure Sleep(milliseconds: Integer);
function GetTickCount: Cardinal;

implementation

constructor Exception.Create(const Msg: string);
begin
  FMessage := Msg;
end;

function Trim(const s: string): string;
var
  i, len: Integer;
  start, finish: Integer;
begin
  len := Length(s);
  start := 1;
  finish := len;
  
  while (start <= len) and (s[start] = ' ') do
    Inc(start);
    
  while (finish >= start) and (s[finish] = ' ') do
    Dec(finish);
    
  if start > finish then
    Result := ''
  else
    Result := Copy(s, start, finish - start + 1);
end;

function TrimLeft(const s: string): string;
var
  i, len: Integer;
begin
  len := Length(s);
  i := 1;
  while (i <= len) and (s[i] = ' ') do
    Inc(i);
  Result := Copy(s, i, len - i + 1);
end;

function TrimRight(const s: string): string;
var
  i: Integer;
begin
  i := Length(s);
  while (i > 0) and (s[i] = ' ') do
    Dec(i);
  Result := Copy(s, 1, i);
end;

function QuotedStr(const s: string): string;
begin
  Result := '''' + s + '''';
end;

function CompareStr(const s1, s2: string): Integer;
begin
  if s1 < s2 then
    Result := -1
  else if s1 > s2 then
    Result := 1
  else
    Result := 0;
end;

function CompareText(const s1, s2: string): Integer;
begin
  Result := CompareStr(LowerCase(s1), LowerCase(s2));
end;

function SameText(const s1, s2: string): Boolean;
begin
  Result := CompareText(s1, s2) = 0;
end;

function BoolToStr(b: Boolean): string;
begin
  if b then
    Result := 'True'
  else
    Result := 'False';
end;

function StrToBool(const s: string): Boolean;
var
  lower: string;
begin
  lower := LowerCase(Trim(s));
  Result := (lower = 'true') or (lower = '1') or (lower = 'yes');
end;

end.
