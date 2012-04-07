unit VarDeclaration;

interface

uses
  Classes, Types, CodeElement, DataType;

type
  TVarDeclaration = class(TCodeElement)
  private
    FDataType: TDataType;
  published
  public
    constructor Create(AName: string; AType: TDataType); reintroduce;
    property DataType: TDataType read FDataType;
  end;

implementation

{ TVarDeclaration }

constructor TVarDeclaration.Create(AName: string; AType: TDataType);
begin
  inherited Create(AName);
  FDataType := AType;
end;

end.
