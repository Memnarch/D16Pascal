unit DataType;

interface

uses
  Classes, Types, Generics.Collections, CodeElement;

type
  TRawType = (rtUInteger, rtBoolean, rtPointer, rtArray, rtRecord, rtString);

  TDataType = class(TCodeElement)
  private
    FRawType: TRawType;
    FSize: Integer;
    FBaseType: TDataType;
    FDimensions: TList<Integer>;
  public
    constructor Create(AName: string; ASize: Integer = 2; APrimitive: TRawType = rtUInteger;
      ABaseType: TDataType = nil);
    function GetRamWordSize(): Integer;
    property Size: Integer read FSize;
    property RawType: TRawType read FRawType;
    property BaseType: TDataType read FBaseType;
    property Dimensions: TList<Integer> read FDimensions;
  end;

implementation

{ TDataType }

constructor TDataType.Create(AName: string; ASize: Integer = 2;
  APrimitive: TRawType = rtUInteger; ABaseType: TDataType = nil);
begin
  inherited Create(AName);
  FDimensions := TList<Integer>.Create();
  FSize := ASize;
  FRawType := APrimitive;
  if Assigned(ABaseType) then
  begin
    FBaseType := ABaseType;
  end
  else
  begin
    FBaseType := Self;
  end;
end;

function TDataType.GetRamWordSize: Integer;
var
  LArSize: Integer;
  LDimSize: Integer;
begin
  if RawType = rtArray then
  begin
    LArSize := 1;
    for LDimSize in FDimensions do
    begin
      LArSize := LArSize * LDimSize;
    end;
    Result := LArSize * BaseType.GetRamWordSize();
  end
  else
  begin
    Result := FSize div 2;
  end;
end;

end.
