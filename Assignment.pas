unit Assignment;

interface

uses
  Classes, Types, SysUtils, Generics.Collections, CodeElement, OpElement, VarDeclaration, WriterIntf, DataType;

type
  TAssignment = class(TOpElement)
  private
    FTargetVar: TVarDeclaration;
    FDereference: Boolean;
    FTargetType: TDataType;
    FSourceType: TDataType;
    procedure GenerateArrayAssignment(AWriter: IWriter; ALeft, ARight: string);
  public
    procedure GetDCPUSource(AWriter: IWriter); override;
    property TargetVar: TVarDeclaration read FTargetVar write FTargetVar;
    property Dereference: Boolean read FDereference write FDereference;
    property TargetType: TDataType read FTargetType write FTargetType;
    property SourceType: TDataType read FSourceType write FSourceType;
  end;

implementation

uses
  StrUtils, Optimizer;
{ TAssignment }

procedure TAssignment.GenerateArrayAssignment(AWriter: IWriter; ALeft,
  ARight: string);
var
  LLabel: string;
  i: Integer;
begin
  if TargetType.IsStaticArray and (TargetType.GetRamWordSize() < 11) then
  begin
    for i := 0 to TargetType.GetRamWordSize() - 1 do
    begin
      AWriter.Write('set [' + ALeft + ' + ' + IntToStr(i) + '], [' + ARight + ' + ' + IntToStr(i) + ']');
    end;
  end
  else
  begin
    LLabel := GetUniqueID('CopyLoop');
    AWriter.Write('set push, j');
    AWriter.Write('set push, i');
    AWriter.Write('set z, [' + ALeft + ' + 0xFFFF]');//-1 to get length
    AWriter.Write('set i, ' + ALeft);
    AWriter.Write('set j, ' + ARight);
    AWriter.Write(':' + LLabel);
    AWriter.Write('sti [i], [j]');
    AWriter.Write('sub z, 1');
    AWriter.Write('ifg z, 0');
    AWriter.Write('set pc, ' + LLabel);
    AWriter.Write('set i, pop');
    AWriter.Write('set j, pop');
  end;
end;

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
    if ((TargetType.RawType = rtArray) and (SourceType.RawType = rtArray)) then
    begin
      GenerateArrayAssignment(Self, 'y', 'x');
    end
    else
    begin
      Self.Write('set [y], x');
    end;
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
        if TargetType.RawType = rtArray then
        begin
          LAccess := 'y';
        end
        else
        begin
          LAccess := '[y]';
        end;
      end;
      Self.Write('set x, pop');
      if ((TargetType.RawType = rtArray) and (SourceType.RawType = rtArray)) then
      begin
        GenerateArrayAssignment(Self, LAccess, 'x');
      end
      else
      begin
        Self.Write('set ' + LAccess + ', x');
      end;
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
      if ((TargetType.RawType = rtArray) and (SourceType.RawType = rtArray)) then
      begin
        GenerateArrayAssignment(Self, LAccess, 'x');
      end
      else
      begin
        Self.Write('set [' + LAccess + '], x');
      end;
    end;
  end;
  OptimizeDCPUCode(Self.FSource, Self.FSource);
  AWriter.WriteList(Self.FSource);
end;

end.
