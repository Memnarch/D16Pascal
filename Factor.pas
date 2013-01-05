unit Factor;

interface

uses
  Classes, Types, Generics.Collections,CodeElement, OpElement, VarDeclaration, WriterIntf;

type
  TFactor = class(TOpElement)
  private
    FVarDeclaration: TVarDeclaration;
    FValue: string;
    FGetAdress: Boolean;
    FInverse: Boolean;
    FDereference: Boolean;
    procedure GetPushValue(AWriter: IWriter);
  public
    function IsConstant(): Boolean;
    procedure GetDCPUSource(AWriter: IWriter); override;
    property VarDeclaration: TVarDeclaration read FVarDeclaration write FVarDeclaration;
    property Value: string read FValue write FValue;
    property GetAdress: Boolean read FGetAdress write FGetAdress;
    property Inverse: Boolean read FInverse write FInverse;
    property Dereference: Boolean read FDereference write FDereference;
  end;

implementation

uses
  StrUtils, SysUtils, DataType;

{ TFactor }

procedure TFactor.GetDCPUSource;
begin
  if SubElements.Count > 0 then
  begin
    inherited;
  end
  else
  begin
    GetPushValue(AWriter);
  end;
  if Dereference then
  begin
    AWriter.Write('set x, pop');
    AWriter.Write('set x, [x]');
    AWriter.Write('set push, x');
  end;
  GetModifierSource(AWriter);
  if Inverse then
  begin
    AWriter.Write('set x, pop');
    AWriter.Write('xor x, 0xffff');
    AWriter.Write('set push, x');
  end;
end;

procedure TFactor.GetPushValue;
var
  LValToPush: string;
  LParts: TStringDynArray;
begin
  if IsConstant then
  begin
    LValToPush := FValue;
  end
  else
  begin
    if FGetAdress or ((FVarDeclaration.ParamIndex > 0) and (FVarDeclaration.ParamIndex <= 3))
      or ((Length(FVarDeclaration.DefaultValue) > 0) and (FVarDeclaration.DefaultValue[1] = '"')) then
    begin
      if VarDeclaration.IsLocal then
      begin
        LParts := SplitString(FVarDeclaration.GetAccessIdentifier, '+');
        if Length(LParts) = 1 then
        begin
          LValToPush := Trim(LParts[0]);
        end
        else
        begin
          AWriter.Write('set x, ' + Trim(LParts[1]));
          AWriter.Write('add x, ' + Trim(LParts[0]));
          LValToPush := 'x';
        end;
      end
      else
      begin
        LValToPush := FVarDeclaration.GetAccessIdentifier();
      end;
      //Result := 'set push, ' +  + sLineBreak;
    end
    else
    begin
      //Result := 'set push, [' + FVarDeclaration.GetAccessIdentifier() + ']' + sLineBreak;
      if (FVarDeclaration.DataType.RawType = rtString) or (FVarDeclaration.DataType.RawType = rtArray) then
      begin
        LValToPush := FVarDeclaration.GetAccessIdentifier();
      end
      else
      begin
        LValToPush := '[' + FVarDeclaration.GetAccessIdentifier() + ']';
      end;
    end;
  end;
  AWriter.Write('set push, ' + LValToPush);
end;

function TFactor.IsConstant: Boolean;
begin
  Result := (not Assigned(FVarDeclaration)) and (SubElements.Count = 0);
end;

end.
