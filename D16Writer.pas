unit D16Writer;

interface

uses
  Classes, Types, Generics.Collections, UncountedInterfacedObject, LineMapping, WriterIntf;

type
  TD16Writer = class(TUncountedInterfacedObject, IWriter)
  private
    FLineMapping: TObjectList<TLineMapping>;
    FSource: TStringList;
    FCurrentUnit: string;
  public
    constructor Create();
    destructor Destroy();
    procedure Write(ALine: string);
    procedure WriteList(AList: TStrings);
    procedure AddMapping(AElement: TObject; AOffset: Integer = 0; AHideName: Boolean = False);
    function GetMappingByASMLine(ALine: Integer): TLineMapping;
    property LineMapping: TObjectList<TLineMapping> read FLineMapping;
    property Source: TStringList read FSource;
    property CurrentUnit: string read FCurrentUnit write FCurrentUnit;
  end;

implementation

uses
  CodeELement;

{ TD16Writer }

procedure TD16Writer.AddMapping(AElement: TObject; AOffset: Integer;
  AHideName: Boolean);
var
  LMapping: TLineMapping;
begin
  LMapping := TLineMapping.Create();
  try
    LMapping.UnitLine := TCodeElement(AElement).Line + AOffset;
    LMapping.ASMLine := FSource.Count;
    if not AHideName then
    begin
      LMapping.ElementName := TCodeElement(AElement).Name;
    end;
    LMapping.D16UnitName := FCurrentUnit;
  finally
    FLineMapping.Add(LMapping);
  end;
end;

constructor TD16Writer.Create;
begin
  FSource := TStringList.Create();
  FLineMapping := TObjectList<TLineMapping>.Create();
end;

destructor TD16Writer.Destroy;
begin
  FSource.Free();
  FLineMapping.Free();
end;

function TD16Writer.GetMappingByASMLine(ALine: Integer): TLineMapping;
var
  LMapping: TLineMapping;
begin
  Result := nil;
  for LMapping in FLineMapping do
  begin
    if LMapping.ASMLine = ALine then
    begin
      Result := LMapping;
      Break;
    end;
  end;
end;

procedure TD16Writer.Write(ALine: string);
begin
  FSource.Add(ALine);
end;

procedure TD16Writer.WriteList(AList: TStrings);
begin
  FSource.AddStrings(AList);
end;

end.
