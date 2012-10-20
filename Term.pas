unit Term;

interface

uses
  Classes, Types, OpElement, WriterIntf;

type
  TTerm = class(TOpElement)
  public
    procedure GetDCPUSource(AWriter: IWriter); override;
  end;

implementation

uses
  SysUtils, CodeElement;

{ TTerm }

procedure TTerm.GetDCPUSource;
var
  i: Integer;
begin
  for i := 0 to SubElements.Count - 1 do
  begin
    SubElements.Items[i].GetDCPUSource(AWriter);
    if i > 0 then
    begin
      AWriter.Write('set y, pop');
      AWriter.Write('set x, pop');
      AWriter.Write(Operations.Items[i-1].GetAssembly(['x', 'y']));
      AWriter.Write('set push, x');
    end;
  end;
end;

end.
