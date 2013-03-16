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
    FImplementationSection: TObjectList<TCodeElement>;
    function GetInterfaceSection: TObjectList<TCodeElement>;
  public
    constructor Create(AName: string); 
    destructor Destroy(); override;
    procedure GetDCPUSource(AWriter: IWriter); override;
    function GetImplementationElement(AName: string; AType: TCodeElementClass): TCodeElement;
    function GetElementFromAll(AName: string; AType: TCodeElementClass): TCodeElement;
    property FooterSource: TStringList read FFooterSource;
    property InitSection: TObjectList<TCodeElement> read FInitSection;
    property ImplementationSection: TObjectList<TCodeElement> read FImplementationSection;
    property InterfaceSection: TObjectList<TCodeElement> read GetInterfaceSection;
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
  FImplementationSection := TObjectList<TCodeElement>.Create();
  FLexer := TLexer.Create();
  FUsedUnits := TStringList.Create();
end;

destructor TPascalUnit.Destroy;
begin
  FFooterSource.Free;
  FinitSection.Free;
  FImplementationSection.Free;
  FLexer.Free;
  FUsedUnits.Free;
  inherited;
end;

procedure TPascalUnit.GetDCPUSource;
var
  LElement: TCodeElement;
begin
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

  for LElement in ImplementationSection do
  begin
    LElement.GetDCPUSource(AWriter);
  end;

  for LElement in SubElements do
  begin
    if LELement is TVarDeclaration then
    begin
      LElement.GetDCPUSource(AWriter);
    end;
  end;


  AWriter.WriteList(FFooterSource);
  AWriter.Write('');//one emptyline for padding, otherwhise we screw mapping...
end;

function TPascalUnit.GetElementFromAll(AName: string;
  AType: TCodeElementClass): TCodeElement;
begin
  Result := GetImplementationElement(AName, AType);
  if not Assigned(Result) then
  begin
    Result := GetElement(AName, AType);
  end;
end;

function TPascalUnit.GetImplementationElement(AName: string;
  AType: TCodeElementClass): TCodeElement;
begin
  Result := GetElementInScope(AName, AType, FImplementationSection);
end;

function TPascalUnit.GetInterfaceSection: TObjectList<TCodeElement>;
begin
  Result := SubElements;
end;

end.
