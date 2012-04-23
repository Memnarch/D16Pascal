unit PascalUnit;

interface

uses
  Classes, Types, Generics.Collections, CodeElement, Lexer;

type
  TPascalUnit = class(TCodeElement)
  private
    FFooterSource: TStringList;
    FInitSection: TObjectList<TCodeElement>;
    FLexer: TLexer;
    FUsedUnits: TStringList;
  public
    constructor Create(AName: string); 
    destructor Destroy(); override;
    function GetDCPUSource(): string; override;
    property FooterSource: TStringList read FFooterSource;
    property InitSection: TObjectList<TCodeElement> read FInitSection;
    property Lexer: TLexer read FLexer;
    property UsedUnits: TStringList read FUsedUnits;
  end;

implementation

uses
  VarDeclaration, Optimizer;

{ TPascalUnit }

constructor TPascalUnit.Create(AName: string);
begin
  inherited;
  FFooterSource := TStringList.Create();
  FInitSection := TObjectList<TCodeElement>.Create();
  FLexer := TLexer.Create();
  FUsedUnits := TStringList.Create();
end;

destructor TPascalUnit.Destroy;
begin
  FFooterSource.Free;
  FinitSection.Free;
  FLexer.Free;
  FUsedUnits.Free;
  inherited;
end;

function TPascalUnit.GetDCPUSource: string;
var
  LElement: TCodeElement;
  LData, LInit: string;
begin
  Result := '';
  LData := '';
  for LElement in SubElements do
  begin
    if not (LElement is TVarDeclaration) then
    begin
      Result := Result + LElement.GetDCPUSource();
    end
    else
    begin
      LData := LData + LElement.GetDCPUSource();
    end;
  end;
  LInit := '';
  for LElement in FInitSection do
  begin
    LInit := LInit + LElement.GetDCPUSource();
  end;
  LInit := SimpleOptimizeDCPUCode(LInit);
  Result := LInit + Result + LData + FFooterSource.Text;
end;

end.
