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
  public
    constructor Create();
    destructor Destroy(); override;
    property Operation: TOperation read FOperation;
    property Variable: TVarDeclaration read FVariable;
    property DataType: TDataType read FDataType;
  end;

implementation

{ TDummyCollection }

constructor TDummyCollection.Create;
begin
  FDataType := TDataType.Create('?', 2, rtNilType);
  FOperation := TOperation.Create('?', rtNilType, rtNilType, 2, 2, FDataType, '');
  FVariable := TVarDeclaration.Create('?', FDataType);
end;

destructor TDummyCollection.Destroy;
begin
  FVariable.Free;
  FOperation.Free;
  FDataType.Free;
  inherited;
end;



end.
