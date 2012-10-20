unit Relation;

interface

uses
  Classes, Types, OpElement, WriterIntf;

type
  TRelation = class(TOpElement)
  private
    FInverse: Boolean;
    FDereference: Boolean;
  public
    procedure GetDCPUSource(AWriter: IWriter); override;
    property Inverse: Boolean read FInverse write FInverse;
    property Dereference: Boolean read FDereference write FDereference;
  end;

implementation

uses
  CodeElement, StrUtils;

{ TRelation }

procedure TRelation.GetDCPUSOurce;
var
  i: Integer;
  LTrue, LFalse: string;
begin
  for i := 0 to SubElements.Count - 1 do
  begin
    SubElements.Items[i].GetDCPUSource(AWriter);
    if i > 0 then
    begin
      LTrue := '0xffff';
      LFalse := '0';
      if (i = SubElements.Count - 1) and Inverse then
      begin
        LTrue := '0';
        LFalse := '0xffff';
      end;
      AWriter.Write('set z, pop');
      AWriter.Write('set y, pop');
      AWriter.Write('set x, ' + LFalse);

      AWriter.Write(Operations.Items[i-1].GetAssembly(['y', 'z', 'x', LTrue, LFalse]));
      AWriter.Write('set x, ' + LTrue);
      AWriter.Write('set push, x');
    end;
  end;
  if Dereference then
  begin
    AWriter.Write('set x, pop');
    AWriter.Write('set x, [x]');
    AWriter.Write('set push, x');
  end;
  if (SubElements.Count = 1) and Inverse then
  begin
    AWriter.Write('set x, pop');
    AWriter.Write('xor x, 0xffff');
    AWriter.Write('set push, x');
  end;

end;

end.
