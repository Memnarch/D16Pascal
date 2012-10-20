unit PascalUnit;

interface

uses
  Classes, Types, Generics.Collections, CodeElement, Lexer, WriterIntf;

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
    procedure GetDCPUSource(AWriter: IWriter); override;
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

procedure TPascalUnit.GetDCPUSource;
var
  LElement: TCodeElement;
//  LData, LInit: string;
begin
//  LData := '';

//  LInit := '';
  for LElement in FInitSection do
  begin
    LElement.GetDCPUSource(AWriter);
  end;

  for LElement in SubElements do
  begin
    if not (LElement is TVarDeclaration) then
    begin
      LElement.GetDCPUSource(AWriter);
    end;
  end;

  for LElement in SubElements do
  begin
    if LELement is TVarDeclaration then
    begin
      LElement.GetDCPUSource(AWriter);
    end;
  end;
  AWriter.Write(FFooterSource.Text);
//  Result := LInit + Result + LData + FFooterSource.Text;
end;

end.
