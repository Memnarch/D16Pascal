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
  public
    function IsConstant(): Boolean;
    function GetDCPUSource(): string; override;
    property VarDeclaration: TVarDeclaration read FVarDeclaration write FVarDeclaration;
    property Value: string read FValue write FValue;
    property GetAdress: Boolean read FGetAdress write FGetAdress;
    property Inverse: Boolean read FInverse write FInverse;
  end;

implementation

{ TFactor }

function TFactor.GetDCPUSource: string;
begin
  if IsConstant then
  begin
    Result := 'set push, ' + FValue + sLineBreak;
  end
  else
  begin
    if FGetAdress then
    begin
      Result := 'set push, ' + FVarDeclaration.Name + sLineBreak;
    end
    else
    begin
      Result := 'set push, [' + FVarDeclaration.Name + ']' + sLineBreak;
    end;
    if Inverse then
    begin
      Result := Result + 'set x, pop' + sLineBreak;
      Result := Result + 'xor x, 0xffff' + sLineBreak;
      Result := Result + 'set push, x' + sLineBreak;
    end;
  end;
end;

function TFactor.IsConstant: Boolean;
begin
  Result := not Assigned(FVarDeclaration);
end;

end.
