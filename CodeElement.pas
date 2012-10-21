unit CodeElement;

interface

uses
  Classes, Types, Generics.Collections, UncountedInterfacedObject, WriterIntf;

type

  TCodeElementClass = class of TCodeElement;

  TCodeElement = class(TUncountedInterfacedObject, IWriter)
  private
    FName: string;
    FSubElements: TObjectList<TCodeElement>;
    FLine: Integer;
  protected
    FSource: TStringList;
    procedure Write(ALine: string); virtual;
    procedure WriteList(AList: TStrings); virtual;
    procedure AddMapping(); virtual;
  public
    constructor Create(AName: string);
    destructor Destroy(); override;
    function GetElement(AName: string; AType: TCodeElementClass): TCodeElement;
    function GetUniqueID(APrefix: string = ''): string;
    procedure GetDCPUSource(AWriter: IWriter); virtual;
    property Name: string read FName write FName;
    property SubElements: TObjectList<TCodeElement> read FSubElements;
    property Line: Integer read FLine write FLine;
  end;


implementation

uses
  SysUtils;

var
  GID: Integer = 0;

{ TCodeElement }

procedure TCodeElement.AddMapping;
begin
// we ignore it here :P
end;

constructor TCodeElement.Create(AName: string);
begin
  FName := AName;
  FLine := -1;
  FSubElements := TObjectList<TCodeElement>.Create();
  FSource := TStringList.Create();
end;

destructor TCodeElement.Destroy;
begin
  FSource.Free;
  inherited;
end;

procedure TCodeElement.GetDCPUSource;
var
  LElement: TCodeElement;
begin
  for LElement in FSubElements do
  begin
    LElement.GetDCPUSource(AWriter);
  end;
end;

function TCodeElement.GetElement(AName: string;
  AType: TCodeElementClass): TCodeElement;
var
  LElement: TCodeElement;
begin
  Result := nil;
  for LElement in FSubElements do
  begin
    if SameText(AName, LElement.Name) and LElement.InheritsFrom(AType) then
    begin
      Result := LElement;
      Break;
    end;
  end;
end;

function TCodeElement.GetUniqueID(APrefix: string = ''): string;
begin
  Result := APrefix+IntToHex(GID, 4);
  Inc(GID);
end;

procedure TCodeElement.Write(ALine: string);
begin
  FSource.Add(ALine);
end;

procedure TCodeElement.WriteList(AList: TStrings);
begin
  FSource.AddStrings(AList);
end;

end.
