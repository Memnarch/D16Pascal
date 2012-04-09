unit VarDeclaration;

interface

uses
  Classes, Types, CodeElement, DataType;

type
  TVarDeclaration = class(TCodeElement)
  private
    FDataType: TDataType;
    FParamIndex: Integer;
    FDefaultValue: string;
  public
    constructor Create(AName: string; AType: TDataType); reintroduce;
    function GetAccessIdentifier(): string;
    function GetDCPUSource(): string; override;
    function IsParameter(): Boolean;
    function IsLocal(): Boolean;
    property DataType: TDataType read FDataType;
    property ParamIndex: Integer read FParamIndex write FParamIndex;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
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
end;

function TVarDeclaration.GetAccessIdentifier: string;
begin
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
        Result := IntToStr(FParamIndex-4) + ' + j';
    end;
  end
  else
  begin
    if IsLocal then
    begin
      Result := IntToStr(Abs(FParamIndex+1)) + ' + j';
    end
    else
    begin
      Result := Name;
    end;
  end;
end;

function TVarDeclaration.GetDCPUSource: string;
begin
  Result := ':' + Name + ' dat ' + FDefaultValue + sLineBreak;
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
