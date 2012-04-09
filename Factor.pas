unit Factor;

interface

uses
  Classes, Types, OpElement, VarDeclaration;

type
  TFactor = class(TOpElement)
  private
    FVarDeclaration: TVarDeclaration;
    FValue: string;
    FGetAdress: Boolean;
    FInverse: Boolean;
    FDereference: Boolean;
  public
    function IsConstant(): Boolean;
    function GetDCPUSource(): string; override;
    property VarDeclaration: TVarDeclaration read FVarDeclaration write FVarDeclaration;
    property Value: string read FValue write FValue;
    property GetAdress: Boolean read FGetAdress write FGetAdress;
    property Inverse: Boolean read FInverse write FInverse;
    property Dereference: Boolean read FDereference write FDereference;
  end;

implementation

uses
  StrUtils, SysUtils;

{ TFactor }

function TFactor.GetDCPUSource: string;
var
  LValToPush: string;
  LParts: TStringDynArray;
begin
  Result := '';
  if IsConstant then
  begin
    LValToPush := FValue;
    //Result := 'set push, ' + FValue + sLineBreak;
  end
  else
  begin
    if FGetAdress or ((FVarDeclaration.ParamIndex > 0) and (FVarDeclaration.ParamIndex <= 3)) then
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
          Result := Result + 'set x, ' + Trim(LParts[1]) + sLineBreak;
          Result := Result + 'add x, ' + Trim(LParts[0]) + sLineBreak;
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
      LValToPush := '[' + FVarDeclaration.GetAccessIdentifier() + ']';
    end;
  end;
  if Dereference then
  begin
    if not (LValToPush[1] = '[') then
    begin
      LValToPush := '[' + LValToPush + ']';
    end;
    Result := Result + 'set x, ' + LValToPush + sLineBreak;
    if AnsiIndexText(LValToPush, ['[a]', '[b]', '[c]']) = -1 then
    begin
      Result := Result + 'set x, [x]' + sLineBreak;
    end;
    LValToPush := 'x';
  end;
  Result := Result + 'set push, ' + LValToPush + sLineBreak;
  if Inverse then
  begin
    Result := Result + 'set x, pop' + sLineBreak;
    Result := Result + 'xor x, 0xffff' + sLineBreak;
    Result := Result + 'set push, x' + sLineBreak;
  end;
end;

function TFactor.IsConstant: Boolean;
begin
  Result := not Assigned(FVarDeclaration);
end;

end.
