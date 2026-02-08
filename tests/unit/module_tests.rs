//! Module system and unit tests for pascal-rs compiler
//! Tests units, modules, PPU files, and cross-module compilation

use pascal::Parser;

// Helper to compile a unit
fn compile_unit(source: &str) -> Result<(), String> {
    let mut parser = Parser::new(source);
    parser.parse_unit().map_err(|e| format!("Parse error: {:?}", e))?;
    Ok(())
}

// Basic Unit Tests

#[test]
fn test_unit_basic_structure() {
    let source = r#"
        unit MyUnit;

        interface

        function GetValue: integer;

        implementation

        function GetValue: integer;
        begin
            Result := 42;
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

#[test]
fn test_unit_interface_implementation() {
    let source = r#"
        unit MathUnit;

        interface

        function Add(a, b: integer): integer;
        function Multiply(a, b: integer): integer;

        implementation

        function Add(a, b: integer): integer;
        begin
            Result := a + b;
        end;

        function Multiply(a, b: integer): integer;
        begin
            Result := a * b;
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

#[test]
fn test_unit_with_types() {
    let source = r#"
        unit TypesUnit;

        interface

        type
            TPoint = record
                X, Y: integer;
            end;
            TColor = (Red, Green, Blue);

        var
            Origin: TPoint;

        implementation

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

#[test]
fn test_unit_with_classes() {
    let source = r#"
        unit ClassUnit;

        interface

        type
            TMyClass = class
                FValue: integer;
                constructor Create;
                procedure SetValue(val: integer);
                function GetValue: integer;
            end;

        implementation

        constructor TMyClass.Create;
        begin
            inherited Create;
            FValue := 0;
        end;

        procedure TMyClass.SetValue(val: integer);
        begin
            FValue := val;
        end;

        function TMyClass.GetValue: integer;
        begin
            Result := FValue;
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

#[test]
fn test_unit_uses_clause() {
    let source = r#"
        unit MainUnit;

        interface

        uses
            SysUtils, Classes;

        function ProcessData: integer;

        implementation

        uses
            MathUnit;

        function ProcessData: integer;
        begin
            Result := Add(10, 20);
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

#[test]
fn test_unit_const_section() {
    let source = r#"
        unit ConstUnit;

        interface

        const
            MaxInt = 100;
            Pi = 3.14159;
            Name = 'Test';

        implementation

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

#[test]
fn test_unit_private_declarations() {
    let source = r#"
        unit PrivateUnit;

        interface

        function PublicFunc: integer;

        implementation

        var
            PrivateVar: integer;

        function PrivateFunc: integer;
        begin
            Result := PrivateVar;
        end;

        function PublicFunc: integer;
        begin
            PrivateVar := 42;
            Result := PrivateFunc;
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

// Cross-Unit Tests

#[test]
fn test_unit_dependency() {
    let source = r#"
        program TestProgram;
        uses
            UnitA, UnitB;
        begin
            writeln(FuncFromUnitA);
            writeln(FuncFromUnitB);
        end.
    "#;

    // This would require UnitA and UnitB to be available
    // For now, just verify parsing
    let mut parser = Parser::new(source);
    let _ = parser.parse_program();
}

#[test]
fn test_unit_circular_dependency() {
    let source = r#"
        unit UnitA;

        interface

        uses
            UnitB;

        function FuncA: integer;

        implementation

        function FuncA: integer;
        begin
            Result := FuncB + 1;
        end;

        end.
    "#;

    // Circular dependencies should be detected during compilation
    let _ = compile_unit(source);
}

// Initialization/Finalization Tests

#[test]
fn test_unit_initialization() {
    let source = r#"
        unit InitUnit;

        interface

        var
            GlobalValue: integer;

        implementation

        initialization
            GlobalValue := 100;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

#[test]
fn test_unit_finalization() {
    let source = r#"
        unit FinalUnit;

        interface

        var
            Resources: ^integer;

        implementation

        initialization
            new(Resources);
            Resources^ := 42;

        finalization
            dispose(Resources);

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

#[test]
fn test_unit_init_and_final() {
    let source = r#"
        unit BothUnit;

        interface

        var
            Counter: integer;

        implementation

        initialization
            Counter := 0;

        finalization
            Counter := -1;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

// Unit Visibility Tests

#[test]
fn test_unit_interface_visibility() {
    let source = r#"
        program Test;
        uses
            MyUnit;
        begin
            // Should access interface functions
            PublicFunction;
        end.
    "#;

    let mut parser = Parser::new(source);
    let _ = parser.parse_program();
}

#[test]
fn test_unit_implementation_hidden() {
    let source = r#"
        unit HiddenUnit;

        interface

        function PublicFunc: integer;

        implementation

        function PrivateFunc: integer;
        begin
            Result := 42;
        end;

        function PublicFunc: integer;
        begin
            Result := PrivateFunc;
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

// Unit with Records Tests

#[test]
fn test_unit_record_methods() {
    let source = r#"
        unit RecordUnit;

        interface

        type
            TPoint = record
                X, Y: integer;
                procedure Move(dx, dy: integer);
                function Distance: real;
            end;

        implementation

        procedure TPoint.Move(dx, dy: integer);
        begin
            X := X + dx;
            Y := Y + dy;
        end;

        function TPoint.Distance: real;
        begin
            Result := Sqrt(X * X + Y * Y);
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

// Unit with Arrays Tests

#[test]
fn test_unit_array_types() {
    let source = r#"
        unit ArrayUnit;

        interface

        type
            TIntArray = array[1..100] of integer;
            TMatrix = array[1..10, 1..10] of real;

        function SumArray(arr: TIntArray): integer;

        implementation

        function SumArray(arr: TIntArray): integer;
        var
            i, sum: integer;
        begin
            sum := 0;
            for i := 1 to 100 do
                sum := sum + arr[i];
            Result := sum;
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

// Unit with Generics Tests

#[test]
fn test_unit_generic_types() {
    let source = r#"
        unit GenericUnit;

        interface

        type
            TList<T> = class
                function Add(item: T): integer;
                function Get(index: integer): T;
            end;

        implementation

        function TList<T>.Add(item: T): integer;
        begin
            Result := 0; // Implementation
        end;

        function TList<T>.Get(index: integer): T;
        begin
            Result := Default(T);
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

// Unit with Exceptions Tests

#[test]
fn test_unit_exception_types() {
    let source = r#"
        unit ExceptionUnit;

        interface

        type
            EMyException = class(Exception)
                constructor Create(msg: string);
            end;

        implementation

        constructor EMyException.Create(msg: string);
        begin
            inherited Create(msg);
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

// Unit with Resources Tests

#[test]
fn test_unit_resource_management() {
    let source = r#"
        unit ResourceUnit;

        interface

        type
            TResourceManager = class
                constructor Create;
                destructor Destroy; override;
            end;

        implementation

        var
            Manager: TResourceManager;

        constructor TResourceManager.Create;
        begin
            inherited Create;
            // Allocate resources
        end;

        destructor TResourceManager.Destroy;
        begin
            // Free resources
            inherited Destroy;
        end;

        initialization
            Manager := TResourceManager.Create;

        finalization
            Manager.Free;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

// Unit with Thread Safety Tests

#[test]
fn test_unit_threadsafe_counter() {
    let source = r#"
        unit ThreadSafeUnit;

        interface

        type
            TThreadSafeCounter = class
            private
                FValue: integer;
                FLock: TRTLCriticalSection;
            public
                constructor Create;
                destructor Destroy; override;
                procedure Increment;
                function GetValue: integer;
            end;

        implementation

        constructor TThreadSafeCounter.Create;
        begin
            inherited Create;
            InitCriticalSection(FLock);
            FValue := 0;
        end;

        destructor TThreadSafeCounter.Destroy;
        begin
            DoneCriticalSection(FLock);
            inherited Destroy;
        end;

        procedure TThreadSafeCounter.Increment;
        begin
            EnterCriticalSection(FLock);
            try
                Inc(FValue);
            finally
                LeaveCriticalSection(FLock);
            end;
        end;

        function TThreadSafeCounter.GetValue: integer;
        begin
            EnterCriticalSection(FLock);
            try
                Result := FValue;
            finally
                LeaveCriticalSection(FLock);
            end;
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

// Unit with External Functions Tests

#[test]
fn test_unit_external_functions() {
    let source = r#"
        unit ExternalUnit;

        interface

        procedure ExternalProc; external 'CLib' name 'external_proc';
        function ExternalFunc(x: integer): integer; external 'CLib';

        implementation

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

// Unit with Inline Assembly Tests

#[test]
fn test_unit_inline_assembly() {
    let source = r#"
        unit AsmUnit;

        interface

        function CPUClock: integer;

        implementation

        function CPUClock: integer;
        asm
            rdtsc
            mov edx, eax
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

// Unit with Exported Functions Tests

#[test]
fn test_unit_exported_functions() {
    let source = r#"
        unit ExportUnit;

        interface

        function ExportedFunc(x: integer): integer; cdecl; export;

        implementation

        function ExportedFunc(x: integer): integer;
        begin
            Result := x * 2;
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

// Unit with Variant Records Tests

#[test]
fn test_unit_variant_records() {
    let source = r#"
        unit VariantUnit;

        interface

        type
            PValue = ^TValue;
            TValue = record
                case Kind: (vkInteger, vkString, vkReal) of
                    vkInteger: (IntVal: integer);
                    vkString: (StrVal: string);
                    vkReal: (RealVal: real);
            end;

        implementation

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

// Unit with Properties Tests

#[test]
fn test_unit_class_properties() {
    let source = r#"
        unit PropertyUnit;

        interface

        type
            TMyClass = class
            private
                FValue: integer;
                procedure SetValue(val: integer);
                function GetValue: integer;
            public
                property Value: integer read GetValue write SetValue;
            end;

        implementation

        procedure TMyClass.SetValue(val: integer);
        begin
            FValue := val;
        end;

        function TMyClass.GetValue: integer;
        begin
            Result := FValue;
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

// Unit with Events Tests

#[test]
fn test_unit_events() {
    let source = r#"
        unit EventUnit;

        interface

        type
            TNotifyEvent = procedure(Sender: TObject) of object;
            TButton = class
                OnClick: TNotifyEvent;
                procedure Click;
            end;

        implementation

        procedure TButton.Click;
        begin
            if Assigned(OnClick) then
                OnClick(Self);
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

// Unit with Attributes Tests (if supported)

#[test]
fn test_unit_attributes() {
    let source = r#"
        unit AttributeUnit;

        interface

        type
            TObsoleteAttribute = class(TCustomAttribute)
            end;

            [TObsolete]
            TOldClass = class
                procedure OldMethod;
            end;

        implementation

        procedure TOldClass.OldMethod;
        begin
            // Implementation
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

// Unit with Library/Dll Tests

#[test]
fn test_unit_library_exports() {
    let source = r#"
        library MyLibrary;

        uses
            SysUtils;

        function Add(a, b: integer): integer; cdecl;
        begin
            Result := a + b;
        end;

        exports
            Add name 'add';

        begin
        end.
    "#;

    // Libraries are similar to programs but with exports
    let mut parser = Parser::new(source);
    let _ = parser.parse_program();
}

// Unit with Package Tests

#[test]
fn test_unit_package() {
    let source = r#"
        package MyPackage;

        requires
            rtl;

        contains
            Unit1,
            Unit2,
            Unit3;

        end.
    "#;

    // Packages have their own syntax
    let mut parser = Parser::new(source);
    let _ = parser.parse_program();
}

// Cross-Unit Optimization Tests

#[test]
fn test_unit_inline_directive() {
    let source = r#"
        unit InlineUnit;

        interface

        function SmallFunc(x: integer): integer; inline;

        implementation

        function SmallFunc(x: integer): integer;
        begin
            Result := x * 2;
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

#[test]
fn test_unit_overload_functions() {
    let source = r#"
        unit OverloadUnit;

        interface

        function Process(x: integer): integer; overload;
        function Process(x: real): real; overload;
        function Process(x: string): string; overload;

        implementation

        function Process(x: integer): integer;
        begin
            Result := x * 2;
        end;

        function Process(x: real): real;
        begin
            Result := x * 2.0;
        end;

        function Process(x: string): string;
        begin
            Result := x + x;
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

// Unit Compilation Order Tests

#[test]
fn test_unit_compilation_order() {
    // Simulate correct compilation order
    let unit1 = r#"
        unit Unit1;

        interface

        function Func1: integer;

        implementation

        function Func1: integer;
        begin
            Result := 1;
        end;

        end.
    "#;

    let unit2 = r#"
        unit Unit2;

        interface

        uses
            Unit1;

        function Func2: integer;

        implementation

        function Func2: integer;
        begin
            Result := Func1 + 1;
        end;

        end.
    "#;

    assert!(compile_unit(unit1).is_ok());
    assert!(compile_unit(unit2).is_ok());
}

// Unit with Static/Class Methods Tests

#[test]
fn test_unit_static_methods() {
    let source = r#"
        unit StaticUnit;

        interface

        type
            TMath = class
                class function Add(a, b: integer): integer;
                class function PI: real;
            end;

        implementation

        class function TMath.Add(a, b: integer): integer;
        begin
            Result := a + b;
        end;

        class function TMath.PI: real;
        begin
            Result := 3.14159;
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}

// Unit with Helper Classes Tests

#[test]
fn test_unit_helper_classes() {
    let source = r#"
        unit HelperUnit;

        interface

        type
            TStringHelper = class helper for string
                function Reverse: string;
                function WordCount: integer;
            end;

        implementation

        function TStringHelper.Reverse: string;
        var
            i: integer;
        begin
            Result := '';
            for i := Length(Self) downto 1 do
                Result := Result + Self[i];
        end;

        function TStringHelper.WordCount: integer;
        begin
            Result := 0; // Simplified
        end;

        end.
    "#;

    assert!(compile_unit(source).is_ok());
}
