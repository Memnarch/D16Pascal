unit Assignment;

interface

uses
  Classes, Types, Generics.Collections, CodeElement, OpElement, VarDeclaration, WriterIntf, DataType;

type
  TAssignment = class(TOpElement)
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
  LParts: TStringDynArray;
begin
  AWriter.AddMapping(Self);
  SubElements.Items[0].GetDCPUSource(Self);
  GetModifierAdressOffset(Self);
  if Dereference then
  begin
    Self.Write('set y, [' + FTargetVar.GetAccessIdentifier() + ']');
    if Modifiers.Count > 0 then
    begin
      Self.Write('add y, pop');
    end;
    Self.Write('set x, pop');
    Self.Write('set [y], x');
  end
  else
  begin
    LAccess := FTargetVar.GetAccessIdentifier;
    if AnsiIndexText(LAccess, ['a', 'b', 'c']) >= 0 then
    begin
      if Modifiers.Count > 0 then
      begin
        Self.Write('set y, pop');
        Self.Write('add y, ' + LAccess);
        LAccess := '[y]';
      end;
      Self.Write('set x, pop');
      Self.Write('set ' + LAccess + ', x');
    end
    else
    begin
      if Modifiers.Count > 0 then
      begin
        Self.Write('set y, pop');
        if (FTargetVar.IsLocal) and (not (FTargetVar.DataType.RawType = rtString)) then //strings are always given as pointer!
        begin
          LParts := SplitString(LAccess, '+');
          Self.Write('add y, ' + LParts[0]);
          if Length(LParts) > 1 then
          begin
            Self.Write('add y, ' + LParts[1]);
          end;
        end
        else
        begin
          Self.Write('add y, [' + LAccess + ']');
        end;
        LAccess := 'y';
      end;
      Self.Write('set x, pop');
      Self.Write('set [' + LAccess + '], x');
    end;
  end;
  OptimizeDCPUCode(Self.FSource, Self.FSource);
  AWriter.WriteList(Self.FSource);
end;

end.
