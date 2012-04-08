unit ProcDeclaration;

interface

uses
  Classes, Types, CodeElement, DataType, VarDeclaration, Generics.Collections;

type
  TArguments = array of TVarDeclaration;

  TProcDeclaration = class(TCodeElement)
  private
    FResultType: TDataType;
    FIsFunction: Boolean;
    FParameters: TObjectList<TCodeElement>;
  public
    constructor Create(AName: string); reintroduce;
    function GetDCPUSource(): string; override;
    property IsFunction: Boolean read FIsFunction;
    property ResultType: TDataType read FResultType write FResultType;
    property Parameters: TObjectList<TCodeElement> read FParameters;
  end;

implementation

{ TProcDeclaration }


{ TProcDeclaration }

constructor TProcDeclaration.Create(AName: string);
begin
  inherited;
  FParameters := TObjectList<TCodeElement>.Create();
end;

function TProcDeclaration.GetDCPUSource: string;
begin
  Result := ':' + Name + sLineBreak;
  Result := Result + 'set push, j' + sLineBreak;
  Result := Result + 'set j, sp' + sLineBreak;
  Result := Result + inherited GetDCPUSource();
  Result := Result + 'set sp, j' + sLineBreak;
  Result := Result + 'set j, pop' + sLineBreak;
  Result := Result + 'set pc, pop' + sLineBreak;
end;

end.
