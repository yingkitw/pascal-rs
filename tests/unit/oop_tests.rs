//! Object-Oriented Programming tests for pascal-rs compiler
//! Tests classes, inheritance, interfaces, and generics

use pascal::Parser;

// Helper to compile and validate OOP features
fn compile_oop_program(source: &str) -> Result<(), String> {
    let mut parser = Parser::new(source);
    parser.parse_program().map_err(|e| format!("Parse error: {:?}", e))?;
    Ok(())
}

// Basic Class Tests

#[test]
fn test_oop_simple_class() {
    let source = r#"
        program Test;
        type
            TMyClass = class
                FValue: integer;
                procedure SetValue(val: integer);
                function GetValue: integer;
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
            writeln(obj.GetValue);
            obj.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

#[test]
fn test_oop_class_constructor() {
    let source = r#"
        program Test;
        type
            TPoint = class
                X, Y: integer;
                constructor Create(x, y: integer);
            end;
        constructor TPoint.Create(x, y: integer);
        begin
            inherited Create;
            X := x;
            Y := y;
        end;
        var
            p: TPoint;
        begin
            p := TPoint.Create(10, 20);
            writeln(p.X, ' ', p.Y);
            p.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

#[test]
fn test_oop_class_destructor() {
    let source = r#"
        program Test;
        type
            TMyClass = class
                Data: ^integer;
                constructor Create;
                destructor Destroy; override;
            end;
        constructor TMyClass.Create;
        begin
            inherited Create;
            new(Data);
            Data^ := 42;
        end;
        destructor TMyClass.Destroy;
        begin
            dispose(Data);
            inherited Destroy;
        end;
        var
            obj: TMyClass;
        begin
            obj := TMyClass.Create;
            obj.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

#[test]
fn test_oop_private_fields() {
    let source = r#"
        program Test;
        type
            TMyClass = class
            private
                FValue: integer;
            public
                procedure SetValue(val: integer);
                function GetValue: integer;
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
            writeln(obj.GetValue);
            obj.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

#[test]
fn test_oop_protected_members() {
    let source = r#"
        program Test;
        type
            TBase = class
            protected
                FProtected: integer;
            public
                constructor Create;
            end;
            TDerived = class(TBase)
            public
                procedure SetProtected(val: integer);
                function GetProtected: integer;
            end;
        constructor TBase.Create;
        begin
            inherited Create;
            FProtected := 10;
        end;
        procedure TDerived.SetProtected(val: integer);
        begin
            FProtected := val;
        end;
        function TDerived.GetProtected: integer;
        begin
            Result := FProtected;
        end;
        var
            obj: TDerived;
        begin
            obj := TDerived.Create;
            obj.SetProtected(20);
            writeln(obj.GetProtected);
            obj.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

#[test]
fn test_oop_public_members() {
    let source = r#"
        program Test;
        type
            TMyClass = class
            public
                PublicField: integer;
                constructor Create;
            end;
        constructor TMyClass.Create;
        begin
            inherited Create;
            PublicField := 42;
        end;
        var
            obj: TMyClass;
        begin
            obj := TMyClass.Create;
            writeln(obj.PublicField);
            obj.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

#[test]
fn test_oop_published_members() {
    let source = r#"
        program Test;
        type
            TMyClass = class
            published
                PropertyField: integer;
                constructor Create;
            end;
        constructor TMyClass.Create;
        begin
            inherited Create;
            PropertyField := 100;
        end;
        var
            obj: TMyClass;
        begin
            obj := TMyClass.Create;
            writeln(obj.PropertyField);
            obj.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

// Inheritance Tests

#[test]
fn test_oop_simple_inheritance() {
    let source = r#"
        program Test;
        type
            TAnimal = class
                Name: string;
                procedure Speak; virtual;
            end;
            TDog = class(TAnimal)
                procedure Speak; override;
            end;
        procedure TAnimal.Speak;
        begin
            writeln('Animal sound');
        end;
        procedure TDog.Speak;
        begin
            writeln('Bark');
        end;
        var
            dog: TDog;
        begin
            dog := TDog.Create;
            dog.Name := 'Buddy';
            dog.Speak;
            dog.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

#[test]
fn test_oop_polymorphism() {
    let source = r#"
        program Test;
        type
            TShape = class
                function GetArea: real; virtual;
            end;
            TCircle = class(TShape)
                Radius: real;
                constructor Create(r: real);
                function GetArea: real; override;
            end;
            TRectangle = class(TShape)
                Width, Height: real;
                constructor Create(w, h: real);
                function GetArea: real; override;
            end;
        function TShape.GetArea: real;
        begin
            Result := 0.0;
        end;
        constructor TCircle.Create(r: real);
        begin
            inherited Create;
            Radius := r;
        end;
        function TCircle.GetArea: real;
        begin
            Result := Pi * Radius * Radius;
        end;
        constructor TRectangle.Create(w, h: real);
        begin
            inherited Create;
            Width := w;
            Height := h;
        end;
        function TRectangle.GetArea: real;
        begin
            Result := Width * Height;
        end;
        var
            shape: TShape;
        begin
            shape := TCircle.Create(5.0);
            writeln(shape.GetArea:0:2);
            shape.Free;
            shape := TRectangle.Create(3.0, 4.0);
            writeln(shape.GetArea:0:2);
            shape.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

#[test]
fn test_oop_inherited_keyword() {
    let source = r#"
        program Test;
        type
            TBase = class
                procedure DoSomething; virtual;
            end;
            TDerived = class(TBase)
                procedure DoSomething; override;
            end;
        procedure TBase.DoSomething;
        begin
            writeln('Base doing something');
        end;
        procedure TDerived.DoSomething;
        begin
            writeln('Derived starting');
            inherited DoSomething;
            writeln('Derived ending');
        end;
        var
            obj: TDerived;
        begin
            obj := TDerived.Create;
            obj.DoSomething;
            obj.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

// Method Tests

#[test]
fn test_oop_virtual_methods() {
    let source = r#"
        program Test;
        type
            TBase = class
                procedure VirtualMethod; virtual;
            end;
            TDerived = class(TBase)
                procedure VirtualMethod; override;
            end;
        procedure TBase.VirtualMethod;
        begin
            writeln('Base virtual');
        end;
        procedure TDerived.VirtualMethod;
        begin
            writeln('Derived override');
        end;
        var
            obj: TBase;
        begin
            obj := TDerived.Create;
            obj.VirtualMethod;
            obj.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

#[test]
fn test_oop_abstract_methods() {
    let source = r#"
        program Test;
        type
            TShape = class
                function GetArea: real; virtual; abstract;
            end;
            TCircle = class(TShape)
                Radius: real;
                constructor Create(r: real);
                function GetArea: real; override;
            end;
        constructor TCircle.Create(r: real);
        begin
            inherited Create;
            Radius := r;
        end;
        function TCircle.GetArea: real;
        begin
            Result := Pi * Radius * Radius;
        end;
        var
            circle: TCircle;
        begin
            circle := TCircle.Create(5.0);
            writeln(circle.GetArea:0:2);
            circle.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

#[test]
fn test_oop_static_methods() {
    let source = r#"
        program Test;
        type
            TMathUtils = class
                class function Add(a, b: integer): integer;
                class function Multiply(a, b: integer): integer;
            end;
        class function TMathUtils.Add(a, b: integer): integer;
        begin
            Result := a + b;
        end;
        class function TMathUtils.Multiply(a, b: integer): integer;
        begin
            Result := a * b;
        end;
        begin
            writeln(TMathUtils.Add(10, 20));
            writeln(TMathUtils.Multiply(10, 20));
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

// Property Tests

#[test]
fn test_oop_simple_property() {
    let source = r#"
        program Test;
        type
            TMyClass = class
            private
                FValue: integer;
            public
                property Value: integer read FValue write FValue;
            end;
        var
            obj: TMyClass;
        begin
            obj := TMyClass.Create;
            obj.Value := 42;
            writeln(obj.Value);
            obj.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

#[test]
fn test_oop_property_with_methods() {
    let source = r#"
        program Test;
        type
            TMyClass = class
            private
                FValue: integer;
                procedure SetValue(val: integer);
                function GetValue: integer;
            public
                property Value: integer read GetValue write SetValue;
            end;
        procedure TMyClass.SetValue(val: integer);
        begin
            FValue := val * 2;
        end;
        function TMyClass.GetValue: integer;
        begin
            Result := FValue;
        end;
        var
            obj: TMyClass;
        begin
            obj := TMyClass.Create;
            obj.Value := 21;
            writeln(obj.Value);
            obj.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

#[test]
fn test_oop_readonly_property() {
    let source = r#"
        program Test;
        type
            TMyClass = class
            private
                FReadOnly: integer;
            public
                constructor Create;
                property ReadOnly: integer read FReadOnly;
            end;
        constructor TMyClass.Create;
        begin
            inherited Create;
            FReadOnly := 42;
        end;
        var
            obj: TMyClass;
        begin
            obj := TMyClass.Create;
            writeln(obj.ReadOnly);
            obj.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

#[test]
fn test_oop_default_property() {
    let source = r#"
        program Test;
        type
            TIntArray = class
            private
                FData: array[1..10] of integer;
                function GetItem(index: integer): integer;
                procedure SetItem(index: integer; value: integer);
            public
                property Items[index: integer]: integer read GetItem write SetItem; default;
            end;
        function TIntArray.GetItem(index: integer): integer;
        begin
            Result := FData[index];
        end;
        procedure TIntArray.SetItem(index: integer; value: integer);
        begin
            FData[index] := value;
        end;
        var
            arr: TIntArray;
        begin
            arr := TIntArray.Create;
            arr.Items[1] := 42;
            writeln(arr.Items[1]);
            arr.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

// Interface Tests

#[test]
fn test_oop_simple_interface() {
    let source = r#"
        program Test;
        type
            IComparable = interface
                function CompareTo(obj: TObject): integer;
            end;
            TMyClass = class(TObject, IComparable)
                Value: integer;
                function CompareTo(obj: TObject): integer;
            end;
        function TMyClass.CompareTo(obj: TObject): integer;
        begin
            if Value = TMyClass(obj).Value then
                Result := 0
            else if Value < TMyClass(obj).Value then
                Result := -1
            else
                Result := 1;
        end;
        var
            obj1, obj2: TMyClass;
        begin
            obj1 := TMyClass.Create;
            obj2 := TMyClass.Create;
            obj1.Value := 10;
            obj2.Value := 20;
            writeln(obj1.CompareTo(obj2));
            obj1.Free;
            obj2.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

#[test]
fn test_oop_multiple_interfaces() {
    let source = r#"
        program Test;
        type
            IPrintable = interface
                procedure Print;
            end;
            IComparable = interface
                function CompareTo(obj: TObject): integer;
            end;
            TMyClass = class(TObject, IPrintable, IComparable)
                Value: integer;
                procedure Print;
                function CompareTo(obj: TObject): integer;
            end;
        procedure TMyClass.Print;
        begin
            writeln(Value);
        end;
        function TMyClass.CompareTo(obj: TObject): integer;
        begin
            Result := Value - TMyClass(obj).Value;
        end;
        var
            obj: TMyClass;
        begin
            obj := TMyClass.Create;
            obj.Value := 42;
            obj.Print;
            obj.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

// Generic Tests

#[test]
fn test_oop_generic_class() {
    let source = r#"
        program Test;
        type
            TPair<T, U> = class
                Key: T;
                Value: U;
                constructor Create(k: T; v: U);
            end;
        constructor TPair<T, U>.Create(k: T; v: U);
        begin
            inherited Create;
            Key := k;
            Value := v;
        end;
        var
            pair: TPair<integer, string>;
        begin
            pair := TPair<integer, string>.Create(1, 'One');
            writeln(pair.Key);
            writeln(pair.Value);
            pair.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

#[test]
fn test_oop_generic_with_constraint() {
    let source = r#"
        program Test;
        type
            TComparable = class
                function CompareTo(other: TComparable): integer; virtual;
            end;
            TList<T: TComparable> = class
                function FindMax: T;
            end;
        function TList<T>.FindMax: T;
        begin
            Result := nil;
        end;
        var
            list: TList<TComparable>;
        begin
            list := TList<TComparable>.Create;
            list.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

#[test]
fn test_oop_generic_method() {
    let source = r#"
        program Test;
        type
            TUtils = class
                class function Swap<T>(var a, b: T);
                class function Max<T>(a, b: T): T;
            end;
        class function TUtils.Swap<T>(var a, b: T);
        var
            temp: T;
        begin
            temp := a;
            a := b;
            b := temp;
        end;
        class function TUtils.Max<T>(a, b: T): T;
        begin
            if a > b then
                Result := a
            else
                Result := b;
        end;
        var
            x, y: integer;
        begin
            x := 10;
            y := 20;
            writeln(TUtils.Max<integer>(x, y));
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

// Operator Overloading Tests

#[test]
fn test_oop_operator_overloading() {
    let source = r#"
        program Test;
        type
            TVector = class
                X, Y: real;
                constructor Create(x, y: real);
                class operator Add(a, b: TVector): TVector;
                class operator Multiply(a: TVector; scalar: real): TVector;
            end;
        constructor TVector.Create(x, y: real);
        begin
            inherited Create;
            X := x;
            Y := y;
        end;
        class operator TVector.Add(a, b: TVector): TVector;
        begin
            Result := TVector.Create(a.X + b.X, a.Y + b.Y);
        end;
        class operator TVector.Multiply(a: TVector; scalar: real): TVector;
        begin
            Result := TVector.Create(a.X * scalar, a.Y * scalar);
        end;
        var
            v1, v2, v3: TVector;
        begin
            v1 := TVector.Create(1.0, 2.0);
            v2 := TVector.Create(3.0, 4.0);
            v3 := v1 + v2;
            writeln(v3.X:0:2, ' ', v3.Y:0:2);
            v1.Free;
            v2.Free;
            v3.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

// Class Reference Tests

#[test]
fn test_oop_class_reference() {
    let source = r#"
        program Test;
        type
            TMyClass = class
                class function GetClassName: string;
            end;
        TMyClassClass = class of TMyClass;
        class function TMyClass.GetClassName: string;
        begin
            Result := 'TMyClass';
        end;
        procedure CreateInstance(cls: TMyClassClass);
        var
            obj: TMyClass;
        begin
            obj := cls.Create;
            writeln(cls.GetClassName);
            obj.Free;
        end;
        begin
            CreateInstance(TMyClass);
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

// Nested Classes Tests

#[test]
fn test_oop_nested_class() {
    let source = r#"
        program Test;
        type
            TOuter = class
            private
                type
                    TInner = class
                        Value: integer;
                    end;
                FInner: TInner;
            public
                constructor Create;
                destructor Destroy; override;
            end;
        constructor TOuter.Create;
        begin
            inherited Create;
            FInner := TInner.Create;
            FInner.Value := 42;
        end;
        destructor TOuter.Destroy;
        begin
            FInner.Free;
            inherited Destroy;
        end;
        var
            outer: TOuter;
        begin
            outer := TOuter.Create;
            outer.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

// Exception Handling in Classes Tests

#[test]
fn test_oop_exception_in_class() {
    let source = r#"
        program Test;
        type
            EMyException = class(Exception)
            end;
            TMyClass = class
                procedure DangerousMethod;
            end;
        procedure TMyClass.DangerousMethod;
        begin
            raise EMyException.Create('Something went wrong');
        end;
        var
            obj: TMyClass;
        begin
            obj := TMyClass.Create;
            try
                obj.DangerousMethod;
            except
                on E: EMyException do
                    writeln(E.Message);
            end;
            obj.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

// Metaclass Tests

#[test]
fn test_oop_metaclass() {
    let source = r#"
        program Test;
        type
            TComponent = class
                constructor Create(AOwner: TComponent); virtual;
            end;
            TComponentClass = class of TComponent;
            TControl = class(TComponent)
            end;
        constructor TComponent.Create(AOwner: TComponent);
        begin
            inherited Create;
        end;
        function CreateComponent(cls: TComponentClass): TComponent;
        begin
            Result := cls.Create(nil);
        end;
        var
            comp: TComponent;
        begin
            comp := CreateComponent(TControl);
            comp.Free;
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

// Advanced OOP Features

#[test]
fn test_oop_class_helper() {
    let source = r#"
        program Test;
        type
            TIntegerHelper = record helper for Integer
                function IsEven: boolean;
                function Double: integer;
            end;
        function TIntegerHelper.IsEven: boolean;
        begin
            Result := (Self mod 2) = 0;
        end;
        function TIntegerHelper.Double: integer;
        begin
            Result := Self * 2;
        end;
        var
            i: integer;
        begin
            i := 21;
            writeln(i.IsEven);
            writeln(i.Double);
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

#[test]
fn test_oop_anonymous_methods() {
    let source = r#"
        program Test;
        type
            TProc = reference to function(x: integer): integer;
        function Apply(func: TProc; value: integer): integer;
        begin
            Result := func(value);
        end;
        begin
            writeln(Apply(function(x: integer): integer
            begin
                Result := x * 2;
            end, 21));
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

#[test]
fn test_oop_class_vars() {
    let source = r#"
        program Test;
        type
            TMyClass = class
            private
                class var
                    FInstanceCount: integer;
            public
                constructor Create;
                destructor Destroy; override;
                class function GetInstanceCount: integer;
            end;
        constructor TMyClass.Create;
        begin
            inherited Create;
            Inc(FInstanceCount);
        end;
        destructor TMyClass.Destroy;
        begin
            Dec(FInstanceCount);
            inherited Destroy;
        end;
        class function TMyClass.GetInstanceCount: integer;
        begin
            Result := FInstanceCount;
        end;
        var
            obj1, obj2: TMyClass;
        begin
            writeln(TMyClass.GetInstanceCount);
            obj1 := TMyClass.Create;
            writeln(TMyClass.GetInstanceCount);
            obj2 := TMyClass.Create;
            writeln(TMyClass.GetInstanceCount);
            obj1.Free;
            writeln(TMyClass.GetInstanceCount);
            obj2.Free;
            writeln(TMyClass.GetInstanceCount);
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}

#[test]
fn test_oop_class_properties() {
    let source = r#"
        program Test;
        type
            TMyClass = class
            private
                class var
                    FDefault: integer;
                class function GetDefault: integer; static;
                class procedure SetDefault(value: integer); static;
            public
                class property Default: integer read GetDefault write SetDefault;
            end;
        class function TMyClass.GetDefault: integer;
        begin
            Result := FDefault;
        end;
        class procedure TMyClass.SetDefault(value: integer);
        begin
            FDefault := value;
        end;
        begin
            TMyClass.Default := 42;
            writeln(TMyClass.Default);
        end.
    "#;

    assert!(compile_oop_program(source).is_ok());
}
