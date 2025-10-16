unit Math;

{$mode objfpc}{$H+}

interface

uses
  System;

{ Mathematical constants }
const
  E = 2.71828182845904523536;
  Pi = 3.14159265358979323846;
  TwoPi = 6.28318530717958647692;
  HalfPi = 1.57079632679489661923;
  
  { Angle conversion }
  RadToDeg = 180.0 / Pi;
  DegToRad = Pi / 180.0;

{ Trigonometric functions }
function Sin(x: Real): Real; external;
function Cos(x: Real): Real; external;
function Tan(x: Real): Real; external;
function ArcSin(x: Real): Real; external;
function ArcCos(x: Real): Real; external;
function ArcTan(x: Real): Real; external;
function ArcTan2(y, x: Real): Real; external;

{ Hyperbolic functions }
function SinH(x: Real): Real;
function CosH(x: Real): Real;
function TanH(x: Real): Real;
function ArcSinH(x: Real): Real;
function ArcCosH(x: Real): Real;
function ArcTanH(x: Real): Real;

{ Exponential and logarithmic functions }
function Exp(x: Real): Real; external;
function Ln(x: Real): Real; external;
function Log10(x: Real): Real;
function Log2(x: Real): Real;
function LogN(base, x: Real): Real;
function Power(base, exponent: Real): Real;
function IntPower(base: Real; exponent: Integer): Real;

{ Root functions }
function Sqrt(x: Real): Real; external;
function Cbrt(x: Real): Real;
function Hypot(x, y: Real): Real;

{ Rounding and remainder functions }
function Ceil(x: Real): Integer;
function Floor(x: Real): Integer;
function Round(x: Real): Integer; external;
function Trunc(x: Real): Integer; external;
function Frac(x: Real): Real; external;
function Int(x: Real): Real; external;
function Modf(x: Real; var intPart: Real): Real;

{ Sign and absolute value }
function Sign(x: Real): Integer; overload;
function Sign(x: Integer): Integer; overload;
function Abs(x: Real): Real; overload; external;
function Abs(x: Integer): Integer; overload; external;

{ Min and Max functions }
function Min(a, b: Real): Real; overload; external;
function Min(a, b: Integer): Integer; overload; external;
function Max(a, b: Real): Real; overload; external;
function Max(a, b: Integer): Integer; overload; external;
function MinValue(const values: array of Real): Real;
function MaxValue(const values: array of Real): Real;
function MinIntValue(const values: array of Integer): Integer;
function MaxIntValue(const values: array of Integer): Integer;

{ Comparison functions }
function CompareValue(a, b: Real; epsilon: Real = 0.0): Integer;
function SameValue(a, b: Real; epsilon: Real = 0.0): Boolean;
function IsZero(a: Real; epsilon: Real = 0.0): Boolean;
function IsNaN(const x: Real): Boolean;
function IsInfinite(const x: Real): Boolean;

{ Statistical functions }
function Mean(const values: array of Real): Real;
function Sum(const values: array of Real): Real;
function SumInt(const values: array of Integer): Integer;
function SumOfSquares(const values: array of Real): Real;
function StdDev(const values: array of Real): Real;
function Variance(const values: array of Real): Real;

{ Angle functions }
function DegToRad(degrees: Real): Real;
function RadToDeg(radians: Real): Real;
function NormalizeAngle(angle: Real): Real;

{ Miscellaneous }
function Factorial(n: Integer): Integer;
function Fibonacci(n: Integer): Integer;
function GCD(a, b: Integer): Integer;
function LCM(a, b: Integer): Integer;
function IsPrime(n: Integer): Boolean;

implementation

{ Hyperbolic functions }

function SinH(x: Real): Real;
begin
  Result := (Exp(x) - Exp(-x)) / 2.0;
end;

function CosH(x: Real): Real;
begin
  Result := (Exp(x) + Exp(-x)) / 2.0;
end;

function TanH(x: Real): Real;
begin
  Result := SinH(x) / CosH(x);
end;

function ArcSinH(x: Real): Real;
begin
  Result := Ln(x + Sqrt(x * x + 1.0));
end;

function ArcCosH(x: Real): Real;
begin
  Result := Ln(x + Sqrt(x * x - 1.0));
end;

function ArcTanH(x: Real): Real;
begin
  Result := 0.5 * Ln((1.0 + x) / (1.0 - x));
end;

{ Exponential and logarithmic functions }

function Log10(x: Real): Real;
begin
  Result := Ln(x) / Ln(10.0);
end;

function Log2(x: Real): Real;
begin
  Result := Ln(x) / Ln(2.0);
end;

function LogN(base, x: Real): Real;
begin
  Result := Ln(x) / Ln(base);
end;

function Power(base, exponent: Real): Real;
begin
  if base = 0.0 then
    Result := 0.0
  else if exponent = 0.0 then
    Result := 1.0
  else
    Result := Exp(exponent * Ln(Abs(base)));
end;

function IntPower(base: Real; exponent: Integer): Real;
var
  i: Integer;
begin
  Result := 1.0;
  if exponent > 0 then
    for i := 1 to exponent do
      Result := Result * base
  else if exponent < 0 then
    for i := 1 to -exponent do
      Result := Result / base;
end;

{ Root functions }

function Cbrt(x: Real): Real;
begin
  if x >= 0.0 then
    Result := Power(x, 1.0 / 3.0)
  else
    Result := -Power(-x, 1.0 / 3.0);
end;

function Hypot(x, y: Real): Real;
begin
  Result := Sqrt(x * x + y * y);
end;

{ Rounding and remainder functions }

function Ceil(x: Real): Integer;
begin
  Result := Trunc(x);
  if Frac(x) > 0.0 then
    Result := Result + 1;
end;

function Floor(x: Real): Integer;
begin
  Result := Trunc(x);
  if Frac(x) < 0.0 then
    Result := Result - 1;
end;

function Modf(x: Real; var intPart: Real): Real;
begin
  intPart := Int(x);
  Result := Frac(x);
end;

{ Sign and absolute value }

function Sign(x: Real): Integer;
begin
  if x > 0.0 then
    Result := 1
  else if x < 0.0 then
    Result := -1
  else
    Result := 0;
end;

function Sign(x: Integer): Integer;
begin
  if x > 0 then
    Result := 1
  else if x < 0 then
    Result := -1
  else
    Result := 0;
end;

{ Min and Max functions }

function MinValue(const values: array of Real): Real;
var
  i: Integer;
begin
  if Length(values) = 0 then
    Result := 0.0
  else
  begin
    Result := values[0];
    for i := 1 to High(values) do
      if values[i] < Result then
        Result := values[i];
  end;
end;

function MaxValue(const values: array of Real): Real;
var
  i: Integer;
begin
  if Length(values) = 0 then
    Result := 0.0
  else
  begin
    Result := values[0];
    for i := 1 to High(values) do
      if values[i] > Result then
        Result := values[i];
  end;
end;

function MinIntValue(const values: array of Integer): Integer;
var
  i: Integer;
begin
  if Length(values) = 0 then
    Result := 0
  else
  begin
    Result := values[0];
    for i := 1 to High(values) do
      if values[i] < Result then
        Result := values[i];
  end;
end;

function MaxIntValue(const values: array of Integer): Integer;
var
  i: Integer;
begin
  if Length(values) = 0 then
    Result := 0
  else
  begin
    Result := values[0];
    for i := 1 to High(values) do
      if values[i] > Result then
        Result := values[i];
  end;
end;

{ Comparison functions }

function CompareValue(a, b: Real; epsilon: Real = 0.0): Integer;
var
  diff: Real;
begin
  if epsilon = 0.0 then
    epsilon := 1e-10;
    
  diff := a - b;
  if Abs(diff) < epsilon then
    Result := 0
  else if diff > 0.0 then
    Result := 1
  else
    Result := -1;
end;

function SameValue(a, b: Real; epsilon: Real = 0.0): Boolean;
begin
  Result := CompareValue(a, b, epsilon) = 0;
end;

function IsZero(a: Real; epsilon: Real = 0.0): Boolean;
begin
  if epsilon = 0.0 then
    epsilon := 1e-10;
  Result := Abs(a) < epsilon;
end;

function IsNaN(const x: Real): Boolean;
begin
  Result := (x <> x);
end;

function IsInfinite(const x: Real): Boolean;
begin
  Result := (x = 1.0 / 0.0) or (x = -1.0 / 0.0);
end;

{ Statistical functions }

function Mean(const values: array of Real): Real;
begin
  if Length(values) = 0 then
    Result := 0.0
  else
    Result := Sum(values) / Length(values);
end;

function Sum(const values: array of Real): Real;
var
  i: Integer;
begin
  Result := 0.0;
  for i := 0 to High(values) do
    Result := Result + values[i];
end;

function SumInt(const values: array of Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(values) do
    Result := Result + values[i];
end;

function SumOfSquares(const values: array of Real): Real;
var
  i: Integer;
begin
  Result := 0.0;
  for i := 0 to High(values) do
    Result := Result + values[i] * values[i];
end;

function Variance(const values: array of Real): Real;
var
  avg: Real;
  i: Integer;
begin
  if Length(values) <= 1 then
    Result := 0.0
  else
  begin
    avg := Mean(values);
    Result := 0.0;
    for i := 0 to High(values) do
      Result := Result + Sqr(values[i] - avg);
    Result := Result / (Length(values) - 1);
  end;
end;

function StdDev(const values: array of Real): Real;
begin
  Result := Sqrt(Variance(values));
end;

{ Angle functions }

function DegToRad(degrees: Real): Real;
begin
  Result := degrees * (Pi / 180.0);
end;

function RadToDeg(radians: Real): Real;
begin
  Result := radians * (180.0 / Pi);
end;

function NormalizeAngle(angle: Real): Real;
begin
  Result := angle;
  while Result > Pi do
    Result := Result - TwoPi;
  while Result < -Pi do
    Result := Result + TwoPi;
end;

{ Miscellaneous }

function Factorial(n: Integer): Integer;
var
  i: Integer;
begin
  if n < 0 then
    Result := 0
  else if n <= 1 then
    Result := 1
  else
  begin
    Result := 1;
    for i := 2 to n do
      Result := Result * i;
  end;
end;

function Fibonacci(n: Integer): Integer;
var
  a, b, temp: Integer;
  i: Integer;
begin
  if n <= 0 then
    Result := 0
  else if n = 1 then
    Result := 1
  else
  begin
    a := 0;
    b := 1;
    for i := 2 to n do
    begin
      temp := a + b;
      a := b;
      b := temp;
    end;
    Result := b;
  end;
end;

function GCD(a, b: Integer): Integer;
var
  temp: Integer;
begin
  a := Abs(a);
  b := Abs(b);
  
  while b <> 0 do
  begin
    temp := b;
    b := a mod b;
    a := temp;
  end;
  
  Result := a;
end;

function LCM(a, b: Integer): Integer;
begin
  if (a = 0) or (b = 0) then
    Result := 0
  else
    Result := Abs(a * b) div GCD(a, b);
end;

function IsPrime(n: Integer): Boolean;
var
  i: Integer;
begin
  if n <= 1 then
    Result := False
  else if n <= 3 then
    Result := True
  else if (n mod 2 = 0) or (n mod 3 = 0) then
    Result := False
  else
  begin
    Result := True;
    i := 5;
    while i * i <= n do
    begin
      if (n mod i = 0) or (n mod (i + 2) = 0) then
      begin
        Result := False;
        Exit;
      end;
      i := i + 6;
    end;
  end;
end;

end.
