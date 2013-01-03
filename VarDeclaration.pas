unit VarDeclaration;

interface

uses
  Classes, Types, CodeElement, DataType, Operations, WriterIntf;

type
  TVarDeclaration = class(TCodeElement)
  private
    FDataType: TDataType;
    FParamIndex: Integer;
    FDefaultValue: string;
    FID: string;
    FIsConstant: Boolean;
  public
    constructor Create(AName: string; AType: TDataType);
    function GetAccessIdentifier(): string;
    procedure GetDCPUSource(AWriter: IWriter); override;
    function IsParameter(): Boolean;
    function IsLocal(): Boolean;
    property DataType: TDataType read FDataType;
    property ParamIndex: Integer read FParamIndex write FParamIndex;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
    property IsConst: Boolean read FIsConstant write FIsConstant;
  end;

implementation

uses
  SysUtils;

{ TVarDeclaration }

constructor TVarDeclaration.Create(AName: string; AType: TDataType);
begin
  inherited Create(AName);
  FDataType := AType;
  FParamIndex := 0;
  FDefaultValue := '0x0';
  FID := GetUniqueID();
end;

function TVarDeclaration.GetAccessIdentifier: string;
var
  LMod: Integer;
begin
  Result := '';
  if IsParameter then
  begin
    case FParamIndex of
      1:
      begin
        Result := 'a';
      end;
      2:
      begin
        Result := 'b';
      end;
      3:
      begin
        Result := 'c';
      end;
      else
        LMod :=  FParamIndex-3 + 1;//+1 because otherwhise we hit the index for the return address on the stack;
        Result := 'j';
        if LMod > 0 then
        begin
          Result := Result + ' + ' + IntToSTr(LMod);
        end;
    end;
  end
  else
  begin
    if IsLocal then
    begin
      LMod := Abs(FParamIndex+1);
      Result := 'j';
      if LMod > 0 then
      begin
        Result := Result + ' + ' + IntToStr(LMod);
      end;
    end
    else
    begin
      Result := Name + FID;
    end;
  end;
end;

procedure TVarDeclaration.GetDCPUSource;
var
  i, LSize: Integer;
  LLine: string;
begin
  AWriter.AddMapping(Self);
  LLine := ':' + GetAccessIdentifier() + ' dat ';
  if DataType.RawType = rtArray then
  begin
    LSize := DataType.GetRamWordSize();
    for i := 0 to LSize - 1 do
    begin
      LLine := LLine + '0x0';
      if i < LSize - 1 then
      begin
        LLine := LLine + ', ';
      end;
    end;
  end
  else
  begin
    LLine := LLine + FDefaultValue;
  end;
  AWriter.Write(LLine);
end;

function TVarDeclaration.IsLocal: Boolean;
begin
  Result := FParamIndex < 0;
end;

function TVarDeclaration.IsParameter: Boolean;
begin
  Result := FParamIndex > 0;
end;

end.
