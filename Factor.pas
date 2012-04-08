unit Factor;

interface

uses
  Classes, Types, OpElement, VarDeclaration;

type
  TFactor = class(TOpElement)
  private
    FVarDeclaration: TVarDeclaration;
    FValue: string;
  public
    function IsConstant(): Boolean;
    function GetDCPUSource(): string; override;
    property VarDeclaration: TVarDeclaration read FVarDeclaration write FVarDeclaration;
    property Value: string read FValue write FValue;
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
    Result := 'set push, [' + FVarDeclaration.Name + ']' + sLineBreak;
  end;
end;

function TFactor.IsConstant: Boolean;
begin
  Result := not Assigned(FVarDeclaration);
end;

end.
