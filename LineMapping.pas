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
    FUnitLine: Integer;
  protected
    FIsPossibleBreakPoint: Boolean;
    procedure ReadFromArray(AInput: TStringDynArray); virtual;
    function GetText: string; virtual;
  public
    constructor Create();
    procedure Clear();
    procedure ReadFromLine(ALine: string); virtual;
    procedure Assign(ASource: TPersistent); override;
    property D16UnitName: string read FD16UnitName write FD16UnitName;
    property UnitLine: Integer read FUnitLine write FUnitLine;
    property ASMLine: Integer read FASMLine write FASMLine;
    property MemoryAddress: Integer read FMemoryAddress write FMemoryAddress;
    property AsText: string read GetText;
    property IsPossibleBreakPoint: Boolean read FIsPossibleBreakPoint;
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
    FUnitLine := LSource.UnitLine;
    FASMLine := LSource.ASMLine;
    FMemoryAddress := LSource.MemoryAddress;
    FIsPossibleBreakPoint := LSource.IsPossibleBreakPoint;
  end;
end;

procedure TLineMapping.Clear;
begin
  FD16UnitName := '';
  FUnitLine := -1;
  FASMLine := -1;
  FMemoryAddress := -1;
end;

constructor TLineMapping.Create;
begin
  Clear();
  FIsPossibleBreakPoint := True;
end;

function TLineMapping.GetText: string;
begin
  Result := D16UnitName + ',' + IntToStr(FUnitLine) + ',' + IntToStr(FASMLine) + ',$'  + IntToHex(FMemoryAddress, 4);
end;

procedure TLineMapping.ReadFromArray(AInput: TStringDynArray);
begin
  D16UnitName := AInput[0];
  UnitLine := StrToInt(AInput[1]);
  ASMLine := StrToInt(AInput[2]);
  MemoryAddress := StrToInt(AInput[3]);
end;

procedure TLineMapping.ReadFromLine(ALine: string);
var
  LElements: TStringDynArray;
begin
  LElements := SplitString(Trim(ALine), ',');
  if Length(LElements) = 4 then
  begin
    ReadFromArray(LElements);
  end
  else
  begin
    raise Exception.Create('Expected 4 elements, but found ' + IntToStr(Length(LElements)) + ' in ' + QuotedStr(ALine));
  end;
end;

end.
