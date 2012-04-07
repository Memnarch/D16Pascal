unit ProcDeclaration;

interface

uses
  Classes, Types, CodeElement, DataType, VarDeclaration;

type
  TArguments = array of TVarDeclaration;

  TProcDeclaration = class(TCodeElement)
  private
    FArgs: array of TVarDeclaration;
    FResultType: TDataType;
    FIsFunction: Boolean;
    function GetArgCount: Integer;
  public
    constructor Create(AName: string; AArgs: array of TVarDeclaration; AReturnType: TDataType = nil);
    property IsFunction: Boolean read FIsFunction;
    property ResultType: TDataType read FResultType;
    property ArgCount: Integer read GetArgCount;
    property Args: array of TVarDeclaration read FArgs;
  end;

implementation

{ TProcDeclaration }

constructor TProcDeclaration.Create(AName: string;
  AArgs: array of TVarDeclaration; AReturnType: TDataType);
begin

end;

function TProcDeclaration.GetArgCount: Integer;
begin

end;

end.
