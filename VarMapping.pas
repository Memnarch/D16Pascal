unit VarMapping;

interface

uses
  Classes, Types, SysUtils, LineMapping;

type
  TVarMapping = class(TLineMapping)
  private
    FElementName: string;
    FTypeName: string;
  protected
    function GetText(): string; override;
  public
    constructor Create(); reintroduce;
    procedure ReadFromLine(ALine: string); override;
    property ElementName: string read FElementName write FElementName;
    property TypeName: string read FTypeName write FTypeName;
  end;

implementation

uses
  StrUtils;

{ TVarMapping }

constructor TVarMapping.Create;
begin
  inherited;
  FIsPossibleBreakPoint := False;
end;

function TVarMapping.GetText: string;
begin
  Result := inherited + '#' + ElementName + ',' + TypeName;
end;

procedure TVarMapping.ReadFromLine(ALine: string);
var
  LLines: TStringDynArray;
  LElements: TStringDynArray;
begin
  LLines := SplitString(ALine, '#');
  if Length(LLines) = 2 then
  begin
    inherited ReadFromLine(LLines[0]);
    LElements := SplitString(LLines[1], ',');
    FElementName := LElements[0];
    FTypeName := LElements[1];
  end
  else
  begin
    raise Exception.Create('Expected 2 LineElements in TVarMapping but found ' + IntToStr(Length(LLines)));
  end;
end;

end.
