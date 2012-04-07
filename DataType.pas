unit DataType;

interface

uses
  Classes, Types, CodeElement;

type
  TDataPrimitive = (dpInteger);

  TDataType = class(TCodeElement)
  private
    FDataPrimitive: TDataPrimitive;
    FSize: Integer;
  public
    constructor Create(AName: string; ASize: Integer = 2; APrimitive: TDataPrimitive = dpInteger); reintroduce;
    property Size: Integer read FSize;
    property DataPrimitive: TDataPrimitive read FDataPrimitive;
  end;

implementation

{ TDataType }

constructor TDataType.Create(AName: string; ASize: Integer;
  APrimitive: TDataPrimitive);
begin
  inherited Create(AName);
  FSize := ASize;
  FDataPrimitive := APrimitive;
end;

end.
