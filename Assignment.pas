unit Assignment;

interface

uses
  Classes, Types, CodeElement, VarDeclaration;

type
  TAssignment = class(TCodeElement)
  private
    FTargetVar: TVarDeclaration;
    FDereference: Boolean;
  public
    function GetDCPUSource(): string; override;
    property TargetVar: TVarDeclaration read FTargetVar write FTargetVar;
    property Dereference: Boolean read FDereference write FDereference;
  end;

implementation

uses
  StrUtils, Optimizer;
{ TAssignment }

function TAssignment.GetDCPUSource: string;
var
  LAccess: string;
begin
  Result := '';
  Result := Result + SubElements.Items[0].GetDCPUSource();
  Result := Result + 'set x, pop' + sLineBreak;
  if Dereference then
  begin
    Result := Result + 'set y, [' + FTargetVar.GetAccessIdentifier() + ']' + sLineBreak;
    Result := Result + 'set [y], x' + sLineBreak;
  end
  else
  begin
    LAccess := FTargetVar.GetAccessIdentifier;
    if AnsiIndexText(LAccess, ['a', 'b', 'c']) >= 0 then
    begin
      Result := Result + 'set ' + LAccess + ', x' + sLineBreak;
    end
    else
    begin
      Result := Result + 'set [' + LAccess + '], x' + sLineBreak;
    end;
  end;
  Result := OptimizeDCPUCode(Result);
end;

end.
