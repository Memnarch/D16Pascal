unit DummyCollection;

interface

uses
  Classes, Types, Generics.Collections, Operation, CodeELement, DataType, VarDeclaration, ProcDeclaration;

type
  TDummyCollection = class(TObject)
  private
    FVariable: TVarDeclaration;
    FOperation: TOperation;
    FDataType: TDataType;
    FElements: TObjectList<TCodeElement>;
  public
    constructor Create();
    destructor Destroy(); override;
    function GetDummyElement(AType: TCodeElementClass): TCodeElement;
    property Operation: TOperation read FOperation;
    property Variable: TVarDeclaration read FVariable;
    property DataType: TDataType read FDataType;
  end;

implementation

{ TDummyCollection }

constructor TDummyCollection.Create;
begin
  FElements := TObjectList<TCodeElement>.Create(False);
  FDataType := TDataType.Create('?', 2, rtNilType);
  FOperation := TOperation.Create('?', rtNilType, rtNilType, 2, 2, FDataType, '');
  FVariable := TVarDeclaration.Create('?', FDataType);
  FElements.Add(FDataType);
  FElements.Add(FVariable);
end;

destructor TDummyCollection.Destroy;
begin
  FVariable.Free;
  FOperation.Free;
  FDataType.Free;
  FElements.Free;
  inherited;
end;



function TDummyCollection.GetDummyElement(
  AType: TCodeElementClass): TCodeElement;
var
  LElement: TCodeElement;
begin
  Result := nil;
  for LElement in FElements do
  begin
    if LElement.InheritsFrom(AType) then
    begin
      Result := LElement;
      Break;
    end;
  end;
end;

end.
