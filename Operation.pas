unit Operation;

interface

uses
  Classes, Types, SysUtils, DataType;

type
  TOperation = class(TObject)
  private
    FLeftRawType: TRawType;
    FLeftSize: Integer;
    FResultType: TDataType;
    FRightRawType: TRawType;
    FRightSize: Integer;
    FOpName: string;
    FAssembly: string;
  public
    constructor Create(AOpName: string; ALeftType, ARightType: TRawType; ALeftSize, ARightSize: Integer;
      AResultType: TDataType; AAssembly: string);
    function GetAssembly(AOperands: array of string): string;
    property LeftRawType: TRawType read FLeftRawType;
    property RightRawType: TRawType read FRightRawType;
    property LeftSize: Integer read FLeftSize;
    property RightSize: Integer read FRightSize;
    property OpName: string read FOpName;
    property ResultType: TDataType read FResultType;
  end;

implementation

{ TOperation }

constructor TOperation.Create(AOpName: string; ALeftType, ARightType: TRawType;
  ALeftSize, ARightSize: Integer; AResultType: TDataType; AAssembly: string);
begin
  FOpName := AOpName;
  FLeftRawType := ALeftType;
  FRightRawType := ARightType;
  FLeftSize := ALeftSize;
  FRightSize := ARightSize;
  FResultType := AResultType;
  FAssembly := AAssembly;
end;

function TOperation.GetAssembly(AOperands: array of string): string;
var
  i: Integer;
begin
  Result := FAssembly;
  for i := 0 to High(AOperands) do
  begin
    Result := StringReplace(Result, '$' + IntToStr(i), AOperands[i], [rfReplaceAll, rfIgnoreCase]);
  end;
end;

end.
