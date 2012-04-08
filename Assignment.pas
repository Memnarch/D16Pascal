unit Assignment;

interface

uses
  Classes, Types, CodeElement, VarDeclaration;

type
  TAssignment = class(TCodeElement)
  private
    FTargetVar: TVarDeclaration;
  public
    function GetDCPUSource(): string; override;
    property TargetVar: TVarDeclaration read FTargetVar write FTargetVar;
  end;

implementation

uses
  Optimizer;
{ TAssignment }

function TAssignment.GetDCPUSource: string;
begin
  Result := Result + SubElements.Items[0].GetDCPUSource();
  Result := Result + 'set x, pop' + sLineBreak;
  Result := Result + 'set [' + FTargetVar.Name + '], x' + sLineBreak;
  Result := OptimizeDCPUCode(Result);
end;

end.
