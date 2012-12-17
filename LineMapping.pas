unit LineMapping;

interface

uses
  Classes, Types;

type
  TLineMapping = class(TPersistent)
  private
    FMemoryAddress: Integer;
    FASMLine: Integer;
    FD16UnitName: string;
    FElementName: string;
    FUnitLine: Integer;
    function GetText: string;
  public
    constructor Create();
    procedure Clear();
    procedure ReadFromLine(ALine: string);
    procedure Assign(ASource: TPersistent); override;
    property D16UnitName: string read FD16UnitName write FD16UnitName;
    property ElementName: string read FElementName write FElementName;
    property UnitLine: Integer read FUnitLine write FUnitLine;
    property ASMLine: Integer read FASMLine write FASMLine;
    property MemoryAddress: Integer read FMemoryAddress write FMemoryAddress;
    property AsText: string read GetText;
  end;

implementation

uses
  SysUtils, StrUtils;

{ TLineMapping }

procedure TLineMapping.Assign(ASource: TPersistent);
var
  LSource: TLineMapping;
begin
  if ASource.InheritsFrom(TLineMapping) then
  begin
    LSource := TLineMapping(ASource);
    FD16UnitName := LSource.D16UnitName;
    FElementName := LSource.ElementName;
    FUnitLine := LSource.UnitLine;
    FASMLine := LSource.ASMLine;
    FMemoryAddress := LSOurce.MemoryAddress;
  end;
end;

procedure TLineMapping.Clear;
begin
  FD16UnitName := '';
  FElementName := '';
  FUnitLine := -1;
  FASMLine := -1;
  FMemoryAddress := -1;
end;

constructor TLineMapping.Create;
begin
  Clear();
end;

function TLineMapping.GetText: string;
begin
  Result := D16UnitName + ',' + ElementName + ',' + IntToStr(FUnitLine) + ',' + IntToStr(FASMLine) + ',$'  + IntToHex(FMemoryAddress, 4);
end;

procedure TLineMapping.ReadFromLine(ALine: string);
var
  LElements: TStringDynArray;
begin
  LElements := SplitString(Trim(ALine), ',');
  if Length(LElements) = 5 then
  begin
    D16UnitName := LElements[0];
    ElementName := LElements[1];
    UnitLine := StrToInt(LElements[2]);
    ASMLine := StrToInt(LElements[3]);
    MemoryAddress := StrToInt(LElements[4]);
  end
  else
  begin
    raise Exception.Create('Expected 5 elements, but found ' + IntToStr(Length(LElements)) + ' in ' + QuotedStr(ALine));
  end;
end;

end.
