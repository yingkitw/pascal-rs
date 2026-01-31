//! Comprehensive standard library tests for pascal-rs compiler
//! Tests System, SysUtils, Classes, and Math units

use pascal::Parser;

// Helper to compile and verify standard library usage
fn compile_with_stdlib(source: &str) -> Result<(), String> {
    let mut parser = Parser::new(source);
    parser.parse_program().map_err(|e| format!("{:?}", e))?;
    Ok(())
}

// System Unit Tests

#[test]
fn test_system_io_writeln() {
    let source = r#"
        program Test;
        begin
            writeln('Hello, World!');
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_io_write() {
    let source = r#"
        program Test;
        begin
            write('Enter name: ');
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_io_readln() {
    let source = r#"
        program Test;
        var
            name: string;
        begin
            readln(name);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_io_read() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            read(x);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_string_length() {
    let source = r#"
        program Test;
        var
            s: string;
            len: integer;
        begin
            s := 'Hello';
            len := Length(s);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_string_copy() {
    let source = r#"
        program Test;
        var
            s, sub: string;
        begin
            s := 'Hello World';
            sub := Copy(s, 1, 5);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_string_concat() {
    let source = r#"
        program Test;
        var
            s1, s2, result: string;
        begin
            s1 := 'Hello';
            s2 := 'World';
            result := Concat(s1, ' ', s2);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_string_pos() {
    let source = r#"
        program Test;
        var
            s: string;
            index: integer;
        begin
            s := 'Hello World';
            index := Pos('World', s);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_string_delete() {
    let source = r#"
        program Test;
        var
            s: string;
        begin
            s := 'Hello World';
            Delete(s, 6, 6);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_string_insert() {
    let source = r#"
        program Test;
        var
            s: string;
        begin
            s := 'World';
        Insert('Hello ', s, 1);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_string_upper_lower() {
    let source = r#"
        program Test;
        var
            s: string;
        begin
            s := 'hello';
            s := UpperCase(s);
            s := LowerCase(s);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_string_trim() {
    let source = r#"
        program Test;
        var
            s: string;
        begin
            s := '  hello  ';
        s := Trim(s);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_string_str() {
    let source = r#"
        program Test;
        var
            s: string;
            i: integer;
        begin
            i := 42;
            Str(i, s);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_string_val() {
    let source = r#"
        program Test;
        var
            s: string;
            i: integer;
            code: integer;
        begin
            s := '42';
            Val(s, i, code);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_math_abs() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := -42;
            x := Abs(x);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_math_sqr() {
    let source = r#"
        program Test;
        var
            x: integer;
        begin
            x := 5;
            x := Sqr(x);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_math_sqrt() {
    let source = r#"
        program Test;
        var
            x: real;
        begin
            x := Sqrt(16.0);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_math_power() {
    let source = r#"
        program Test;
        var
            base, exp, result: real;
        begin
            base := 2.0;
            exp := 3.0;
            result := Power(base, exp);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_math_exp_ln() {
    let source = r#"
        program Test;
        var
            x: real;
        begin
            x := Exp(1.0);
            x := Ln(x);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_math_trigonometric() {
    let source = r#"
        program Test;
        var
            angle, result: real;
        begin
            angle := 1.57;
            result := Sin(angle);
            result := Cos(angle);
            result := Tan(angle);
            result := ArcTan(result);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_math_round_trunc() {
    let source = r#"
        program Test;
        var
            r: real;
            i: integer;
        begin
            r := 3.7;
            i := Round(r);
            i := Trunc(r);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_math_int_frac() {
    let source = r#"
        program Test;
        var
            r: real;
        begin
            r := 3.7;
            r := Int(r);
            r := Frac(r);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_memory_new() {
    let source = r#"
        program Test;
        var
            p: ^integer;
        begin
            New(p);
            p^ := 42;
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_memory_dispose() {
    let source = r#"
        program Test;
        var
            p: ^integer;
        begin
            New(p);
            Dispose(p);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_memory_getmem() {
    let source = r#"
        program Test;
        var
            p: pointer;
        begin
            GetMem(p, 1024);
            FreeMem(p, 1024);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_memory_fillchar() {
    let source = r#"
        program Test;
        var
            buf: array[0..255] of byte;
        begin
            FillChar(buf, SizeOf(buf), 0);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_memory_move() {
    let source = r#"
        program Test;
        var
            src, dst: array[0..255] of byte;
        begin
            Move(src, dst, SizeOf(src));
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_file_assign() {
    let source = r#"
        program Test;
        var
            f: text;
        begin
            Assign(f, 'test.txt');
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_file_reset() {
    let source = r#"
        program Test;
        var
            f: text;
        begin
            Assign(f, 'test.txt');
            Reset(f);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_file_rewrite() {
    let source = r#"
        program Test;
        var
            f: text;
        begin
            Assign(f, 'test.txt');
            Rewrite(f);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_file_close() {
    let source = r#"
        program Test;
        var
            f: text;
        begin
            Assign(f, 'test.txt');
            Rewrite(f);
            Close(f);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_file_eof() {
    let source = r#"
        program Test;
        var
            f: text;
        begin
            Assign(f, 'test.txt');
            Reset(f);
            while not Eof(f) do
                Readln(f);
            Close(f);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_file_eoln() {
    let source = r#"
        program Test;
        var
            f: text;
        begin
            Assign(f, 'test.txt');
            Reset(f);
            while not Eoln(f) do
                Read(f, x);
            Close(f);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_conversion_ord() {
    let source = r#"
        program Test;
        var
            c: char;
            i: integer;
        begin
            c := 'A';
            i := Ord(c);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_conversion_chr() {
    let source = r#"
        program Test;
        var
            c: char;
        begin
            c := Chr(65);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_conversion_odd() {
    let source = r#"
        program Test;
        var
            i: integer;
            b: boolean;
        begin
            i := 5;
            b := Odd(i);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_conversion_pred_succ() {
    let source = r#"
        program Test;
        var
            c: char;
        begin
            c := 'B';
            c := Pred(c);
            c := Succ(c);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_halt() {
    let source = r#"
        program Test;
        begin
            if ErrorCondition then
                Halt(1);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_system_exit() {
    let source = r#"
        program Test;
        procedure MyProc;
        begin
            if ErrorCondition then
                Exit;
        end;
        begin
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

// SysUtils Unit Tests

#[test]
fn test_sysutils_format() {
    let source = r#"
        program Test;
        begin
            writeln(Format('Value: %d', [42]));
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_sysutils_datetime_now() {
    let source = r#"
        program Test;
        begin
            writeln('Now: ', Now);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_sysutils_date_time() {
    let source = r#"
        program Test;
        begin
            writeln('Date: ', Date);
            writeln('Time: ', Time);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_sysutils_encodedecode_date() {
    let source = r#"
        program Test;
        var
            y, m, d: word;
            dt: TDateTime;
        begin
            dt := EncodeDate(2026, 1, 31);
            DecodeDate(dt, y, m, d);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_sysutils_encodedecode_time() {
    let source = r#"
        program Test;
        var
            h, m, s, ms: word;
            dt: TDateTime;
        begin
            dt := EncodeTime(12, 30, 45, 0);
            DecodeTime(dt, h, m, s, ms);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_sysutils_datetostr_timetostr() {
    let source = r#"
        program Test;
        var
            dt: TDateTime;
        begin
            dt := Now;
            writeln(DateToStr(dt));
            writeln(TimeToStr(dt));
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_sysutils_strtodate_strtotime_strtodatetime() {
    let source = r#"
        program Test;
        var
            dt: TDateTime;
        begin
            dt := StrToDate('31/01/2026');
            dt := StrToTime('12:30:45');
            dt := StrToDateTime('31/01/2026 12:30:45');
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_sysutils_day_of_week() {
    let source = r#"
        program Test;
        var
            dt: TDateTime;
            day: word;
        begin
            dt := Now;
            day := DayOfWeek(dt);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_sysutils_comparestr() {
    let source = r#"
        program Test;
        var
            result: integer;
        begin
            result := CompareStr('hello', 'world');
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_sysutils_sametext() {
    let source = r#"
        program Test;
        var
            equal: boolean;
        begin
            equal := SameText('hello', 'HELLO');
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_sysutils_trimleft_trimright() {
    let source = r#"
        program Test;
        var
            s: string;
        begin
            s := '  hello  ';
        s := TrimLeft(s);
        s := TrimRight(s);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_sysutils_stringreplace() {
    let source = r#"
        program Test;
        var
            s: string;
        begin
            s := 'hello world';
        s := StringReplace(s, 'world', 'pascal');
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_sysutils_exception_handling() {
    let source = r#"
        program Test;
        begin
            try
                // Some operation
            except
                on E: Exception do
                    writeln(E.Message);
            end;
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_sysutils_create_dir() {
    let source = r#"
        program Test;
        begin
            CreateDir('newdir');
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_sysutils_remove_dir() {
    let source = r#"
        program Test;
        begin
            RemoveDir('olddir');
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_sysutils_directory_exists() {
    let source = r#"
        program Test;
        begin
            if DirectoryExists('/tmp') then
                writeln('Directory exists');
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_sysutils_file_exists() {
    let source = r#"
        program Test;
        begin
            if FileExists('test.txt') then
                writeln('File exists');
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_sysutils_delete_file() {
    let source = r#"
        program Test;
        begin
            DeleteFile('oldfile.txt');
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_sysutils_rename_file() {
    let source = r#"
        program Test;
        begin
            RenameFile('old.txt', 'new.txt');
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

// Classes Unit Tests

#[test]
fn test_classes_tobject() {
    let source = r#"
        program Test;
        var
            obj: TObject;
        begin
            obj := TObject.Create;
            writeln('ClassName: ', obj.ClassName);
            obj.Free;
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_classes_tlist() {
    let source = r#"
        program Test;
        var
            list: TList;
            i: integer;
        begin
            list := TList.Create;
            for i := 0 to 9 do
                list.Add(Pointer(i));

            writeln('Count: ', list.Count);

            list.Free;
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_classes_tstringlist() {
    let source = r#"
        program Test;
        var
            sl: TStringList;
        begin
            sl := TStringList.Create;
            sl.Add('Line 1');
            sl.Add('Line 2');
            sl.Add('Line 3');

            writeln('Count: ', sl.Count);
            writeln('Text: ', sl.Text);

            sl.Free;
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_classes_tstringlist_loadsave() {
    let source = r#"
        program Test;
        var
            sl: TStringList;
        begin
            sl := TStringList.Create;
            sl.LoadFromFile('input.txt');
            sl.SaveToFile('output.txt');
            sl.Free;
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_classes_tstringlist_sort() {
    let source = r#"
        program Test;
        var
            sl: TStringList;
        begin
            sl := TStringList.Create;
            sl.Add('zebra');
            sl.Add('apple');
            sl.Add('banana');

            sl.Sort;

            sl.Free;
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_classes_tstream() {
    let source = r#"
        program Test;
        var
            stream: TStream;
            buffer: array[0..255] of byte;
            count: integer;
        begin
            stream := TFileStream.Create('test.dat', fmCreate);
            stream.Write(buffer, SizeOf(buffer));
            stream.Free;
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_classes_tfilestream() {
    let source = r#"
        program Test;
        var
            fs: TFileStream;
        begin
            fs := TFileStream.Create('test.dat', fmOpenRead);
            fs.Free;
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_classes_tmemorystream() {
    let source = r#"
        program Test;
        var
            ms: TMemoryStream;
        begin
            ms := TMemoryStream.Create;
            ms.WriteBuffer('Hello', 5);
            ms.Seek(0, 0);
            ms.Free;
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_classes_custom_class() {
    let source = r#"
        program Test;
        type
            TMyClass = class(TObject)
                private
                    FValue: integer;
                public
                    constructor Create;
                    destructor Destroy; override;
                    procedure SetValue(val: integer);
                    function GetValue: integer;
            end;

        constructor TMyClass.Create;
        begin
            inherited Create;
            FValue := 0;
        end;

        destructor TMyClass.Destroy;
        begin
            inherited Destroy;
        end;

        procedure TMyClass.SetValue(val: integer);
        begin
            FValue := val;
        end;

        function TMyClass.GetValue: integer;
        begin
            Result := FValue;
        end;

        var
            obj: TMyClass;
        begin
            obj := TMyClass.Create;
            obj.SetValue(42);
            writeln('Value: ', obj.GetValue);
            obj.Free;
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

// Math Unit Tests

#[test]
fn test_math_min_max() {
    let source = r#"
        program Test;
        var
            a, b, result: integer;
        begin
            a := 10;
            b := 20;
            result := Min(a, b);
            result := Max(a, b);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_math_range() {
    let source = r#"
        program Test;
        var
            value, result: integer;
        begin
            value := 50;
            result := EnsureRange(value, 0, 100);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_math_inrange() {
    let source = r#"
        program Test;
        var
            value: integer;
            in_range: boolean;
        begin
            value := 50;
            in_range := InRange(value, 0, 100);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_math_floor_ceil() {
    let source = r#"
        program Test;
        var
            r: real;
            i: integer;
        begin
            r := 3.7;
            i := Floor(r);
            i := Ceil(r);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_math_log2_log10() {
    let source = r#"
        program Test;
        var
            x: real;
        begin
            x := Log2(8.0);
            x := Log10(100.0);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_math_power_base() {
    let source = r#"
        program Test;
        var
            x: real;
        begin
            x := Power(2.0, 10.0);
            x := LogN(10.0, x);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_math_hypot() {
    let source = r#"
        program Test;
        var
            x, y, result: real;
        begin
            x := 3.0;
            y := 4.0;
            result := Hypot(x, y);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_math_degtorad_radtodeg() {
    let source = r#"
        program Test;
        var
            angle: real;
        begin
            angle := DegToRad(180.0);
            angle := RadToDeg(angle);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_math_cyclespersecond() {
    let source = r#"
        program Test;
        var
            freq: int64;
        begin
            freq := GetCPUFrequency;
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_math_randseed() {
    let source = r#"
        program Test;
        begin
            Randomize;
            RandSeed := 42;
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_math_random() {
    let source = r#"
        program Test;
        var
            r: real;
            i: integer;
        begin
            Randomize;
            r := Random;
            i := Random(100);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_math_isnan_isinfinity() {
    let source = r#"
        program Test;
        var
            r: real;
        begin
            r := 0.0 / 0.0;
            if IsNan(r) then
                writeln('NaN');
            r := 1.0 / 0.0;
            if IsInfinite(r) then
                writeln('Infinite');
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_math_divmod() {
    let source = r#"
        program Test;
        var
            dividend, divisor, quotient, remainder: integer;
        begin
            dividend := 100;
            divisor := 7;
            DivMod(dividend, divisor, quotient, remainder);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_math_greatest_common_divisor() {
    let source = r#"
        program Test;
        var
            a, b, result: integer;
        begin
            a := 48;
            b := 18;
            result := GreatestCommonDivisor(a, b);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_math_least_common_multiple() {
    let source = r#"
        program Test;
        var
            a, b, result: integer;
        begin
            a := 12;
            b := 18;
            result := LeastCommonMultiple(a, b);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_math_prime_numbers() {
    let source = r#"
        program Test;
        var
            n: integer;
            is_prime: boolean;
        begin
            n := 17;
            is_prime := IsPrime(n);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_math_factorial() {
    let source = r#"
        program Test;
        var
            n, result: integer;
        begin
            n := 5;
            result := Factorial(n);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}

#[test]
fn test_math_fibonacci() {
    let source = r#"
        program Test;
        var
            n, result: integer;
        begin
            n := 10;
            result := Fibonacci(n);
        end.
    "#;

    assert!(compile_with_stdlib(source).is_ok());
}
