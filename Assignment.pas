unit Assignment;

interface

uses
  Classes, Types, CodeElement, VarDeclaration, WriterIntf;

type
  TAssignment = class(TCodeElement)
  private
    FTargetVar: TVarDeclaration;
    FDereference: Boolean;
  public
    procedure GetDCPUSource(AWriter: IWriter); override;
    property TargetVar: TVarDeclaration read FTargetVar write FTargetVar;
    property Dereference: Boolean read FDereference write FDereference;
  end;

implementation

uses
  StrUtils, Optimizer;
{ TAssignment }

procedure TAssignment.GetDCPUSource;
var
  LAccess: string;
begin
  SubElements.Items[0].GetDCPUSource(Self);
  if Dereference then
  begin
    Self.Write('set y, [' + FTargetVar.GetAccessIdentifier() + ']');
    Self.Write('set x, pop');
    Self.Write('set [y], x');
  end
  else
  begin
    LAccess := FTargetVar.GetAccessIdentifier;
    if AnsiIndexText(LAccess, ['a', 'b', 'c']) >= 0 then
    begin
      Self.Write('set x, pop');
      Self.Write('set ' + LAccess + ', x');
    end
    else
    begin
      Self.Write('set x, pop');
      Self.Write('set [' + LAccess + '], x');
    end;
  end;
  OptimizeDCPUCode(Self.FSource, Self.FSource);
  AWriter.WriteList(Self.FSource);
end;

end.
