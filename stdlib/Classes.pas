unit Classes;

{$mode objfpc}{$H+}

interface

uses
  System, SysUtils;

{ Base class for all objects }
type
  TObject = class
  public
    constructor Create;
    destructor Destroy; virtual;
    procedure Free;
    function ClassName: string; virtual;
  end;

{ List classes }
type
  TList = class(TObject)
  private
    FItems: array of Pointer;
    FCount: Integer;
    FCapacity: Integer;
    function GetItem(index: Integer): Pointer;
    procedure SetItem(index: Integer; value: Pointer);
    procedure Grow;
  public
    constructor Create;
    destructor Destroy; override;
    
    function Add(item: Pointer): Integer;
    procedure Insert(index: Integer; item: Pointer);
    procedure Delete(index: Integer);
    procedure Clear;
    function IndexOf(item: Pointer): Integer;
    function Remove(item: Pointer): Integer;
    
    property Count: Integer read FCount;
    property Capacity: Integer read FCapacity;
    property Items[index: Integer]: Pointer read GetItem write SetItem; default;
  end;

  TStringList = class(TObject)
  private
    FStrings: array of string;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    function GetString(index: Integer): string;
    procedure SetString(index: Integer; const value: string);
    procedure Grow;
  public
    constructor Create;
    destructor Destroy; override;
    
    function Add(const s: string): Integer;
    procedure Insert(index: Integer; const s: string);
    procedure Delete(index: Integer);
    procedure Clear;
    function IndexOf(const s: string): Integer;
    procedure Sort;
    procedure LoadFromFile(const fileName: string);
    procedure SaveToFile(const fileName: string);
    
    property Count: Integer read FCount;
    property Strings[index: Integer]: string read GetString write SetString; default;
    property Sorted: Boolean read FSorted write FSorted;
  end;

{ Stream classes }
type
  TStream = class(TObject)
  public
    function Read(var buffer; count: Integer): Integer; virtual; abstract;
    function Write(const buffer; count: Integer): Integer; virtual; abstract;
    function Seek(offset: Integer; origin: Integer): Integer; virtual; abstract;
    property Position: Integer;
    property Size: Integer;
  end;

  TFileStream = class(TStream)
  private
    FHandle: Integer;
    FFileName: string;
  public
    constructor Create(const fileName: string; mode: Integer);
    destructor Destroy; override;
    
    function Read(var buffer; count: Integer): Integer; override;
    function Write(const buffer; count: Integer): Integer; override;
    function Seek(offset: Integer; origin: Integer): Integer; override;
    
    property FileName: string read FFileName;
  end;

  TMemoryStream = class(TStream)
  private
    FMemory: Pointer;
    FSize: Integer;
    FPosition: Integer;
    FCapacity: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    function Read(var buffer; count: Integer): Integer; override;
    function Write(const buffer; count: Integer): Integer; override;
    function Seek(offset: Integer; origin: Integer): Integer; override;
    procedure Clear;
    procedure LoadFromFile(const fileName: string);
    procedure SaveToFile(const fileName: string);
  end;

{ Component base class }
type
  TComponent = class(TObject)
  private
    FName: string;
    FOwner: TComponent;
    FComponents: TList;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    
    property Name: string read FName write FName;
    property Owner: TComponent read FOwner;
  end;

implementation

{ TObject }

constructor TObject.Create;
begin
  inherited;
end;

destructor TObject.Destroy;
begin
  inherited;
end;

procedure TObject.Free;
begin
  if Self <> nil then
    Destroy;
end;

function TObject.ClassName: string;
begin
  Result := 'TObject';
end;

{ TList }

constructor TList.Create;
begin
  inherited Create;
  FCount := 0;
  FCapacity := 0;
  SetLength(FItems, 0);
end;

destructor TList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TList.Grow;
begin
  if FCapacity = 0 then
    FCapacity := 4
  else
    FCapacity := FCapacity * 2;
  SetLength(FItems, FCapacity);
end;

function TList.GetItem(index: Integer): Pointer;
begin
  if (index < 0) or (index >= FCount) then
    raise ERangeError.Create('List index out of bounds');
  Result := FItems[index];
end;

procedure TList.SetItem(index: Integer; value: Pointer);
begin
  if (index < 0) or (index >= FCount) then
    raise ERangeError.Create('List index out of bounds');
  FItems[index] := value;
end;

function TList.Add(item: Pointer): Integer;
begin
  if FCount >= FCapacity then
    Grow;
  FItems[FCount] := item;
  Result := FCount;
  Inc(FCount);
end;

procedure TList.Insert(index: Integer; item: Pointer);
var
  i: Integer;
begin
  if (index < 0) or (index > FCount) then
    raise ERangeError.Create('List index out of bounds');
    
  if FCount >= FCapacity then
    Grow;
    
  for i := FCount downto index + 1 do
    FItems[i] := FItems[i - 1];
    
  FItems[index] := item;
  Inc(FCount);
end;

procedure TList.Delete(index: Integer);
var
  i: Integer;
begin
  if (index < 0) or (index >= FCount) then
    raise ERangeError.Create('List index out of bounds');
    
  for i := index to FCount - 2 do
    FItems[i] := FItems[i + 1];
    
  Dec(FCount);
end;

procedure TList.Clear;
begin
  FCount := 0;
  FCapacity := 0;
  SetLength(FItems, 0);
end;

function TList.IndexOf(item: Pointer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FCount - 1 do
    if FItems[i] = item then
    begin
      Result := i;
      Exit;
    end;
end;

function TList.Remove(item: Pointer): Integer;
begin
  Result := IndexOf(item);
  if Result >= 0 then
    Delete(Result);
end;

{ TStringList }

constructor TStringList.Create;
begin
  inherited Create;
  FCount := 0;
  FCapacity := 0;
  FSorted := False;
  SetLength(FStrings, 0);
end;

destructor TStringList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TStringList.Grow;
begin
  if FCapacity = 0 then
    FCapacity := 4
  else
    FCapacity := FCapacity * 2;
  SetLength(FStrings, FCapacity);
end;

function TStringList.GetString(index: Integer): string;
begin
  if (index < 0) or (index >= FCount) then
    raise ERangeError.Create('StringList index out of bounds');
  Result := FStrings[index];
end;

procedure TStringList.SetString(index: Integer; const value: string);
begin
  if (index < 0) or (index >= FCount) then
    raise ERangeError.Create('StringList index out of bounds');
  FStrings[index] := value;
end;

function TStringList.Add(const s: string): Integer;
begin
  if FCount >= FCapacity then
    Grow;
  FStrings[FCount] := s;
  Result := FCount;
  Inc(FCount);
  
  if FSorted then
    Sort;
end;

procedure TStringList.Insert(index: Integer; const s: string);
var
  i: Integer;
begin
  if (index < 0) or (index > FCount) then
    raise ERangeError.Create('StringList index out of bounds');
    
  if FCount >= FCapacity then
    Grow;
    
  for i := FCount downto index + 1 do
    FStrings[i] := FStrings[i - 1];
    
  FStrings[index] := s;
  Inc(FCount);
end;

procedure TStringList.Delete(index: Integer);
var
  i: Integer;
begin
  if (index < 0) or (index >= FCount) then
    raise ERangeError.Create('StringList index out of bounds');
    
  for i := index to FCount - 2 do
    FStrings[i] := FStrings[i + 1];
    
  Dec(FCount);
end;

procedure TStringList.Clear;
begin
  FCount := 0;
  FCapacity := 0;
  SetLength(FStrings, 0);
end;

function TStringList.IndexOf(const s: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FCount - 1 do
    if FStrings[i] = s then
    begin
      Result := i;
      Exit;
    end;
end;

procedure TStringList.Sort;
var
  i, j: Integer;
  temp: string;
begin
  for i := 0 to FCount - 2 do
    for j := i + 1 to FCount - 1 do
      if FStrings[i] > FStrings[j] then
      begin
        temp := FStrings[i];
        FStrings[i] := FStrings[j];
        FStrings[j] := temp;
      end;
end;

procedure TStringList.LoadFromFile(const fileName: string);
var
  f: Text;
  line: string;
begin
  Clear;
  Assign(f, fileName);
  Reset(f);
  try
    while not EOF(f) do
    begin
      ReadLn(f, line);
      Add(line);
    end;
  finally
    Close(f);
  end;
end;

procedure TStringList.SaveToFile(const fileName: string);
var
  f: Text;
  i: Integer;
begin
  Assign(f, fileName);
  Rewrite(f);
  try
    for i := 0 to FCount - 1 do
      WriteLn(f, FStrings[i]);
  finally
    Close(f);
  end;
end;

{ TComponent }

constructor TComponent.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FComponents := TList.Create;
  if FOwner <> nil then
    FOwner.FComponents.Add(Self);
end;

destructor TComponent.Destroy;
var
  i: Integer;
begin
  for i := FComponents.Count - 1 downto 0 do
    TComponent(FComponents[i]).Free;
  FComponents.Free;
  inherited Destroy;
end;

end.
