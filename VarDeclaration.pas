unit VarDeclaration;

interface

uses
  Classes, Types, CodeElement, DataType;

type
  TVarDeclaration = class(TCodeElement)
  private
    FDataType: TDataType;
  public
    constructor Create(AName: string; AType: TDataType); reintroduce;
    function GetDCPUSOurce(): string; override;
    property DataType: TDataType read FDataType;
  end;

implementation

{ TVarDeclaration }

constructor TVarDeclaration.Create(AName: string; AType: TDataType);
begin
  inherited Create(AName);
  FDataType := AType;
end;

function TVarDeclaration.GetDCPUSOurce: string;
begin
  Result := ':' + Name + ' dat 0x0' + sLineBreak;
end;

end.
