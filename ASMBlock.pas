unit ASMBlock;

interface

uses
  Classes, Types, SysUtils, CodeElement, WriterIntf;

type
  TASMBlock = class(TCodeElement)
  public
    procedure GetDCPUSource(AWriter: IWriter); override;
    property Source: TStringList read FSource;
  end;

implementation

{ TASMBlock }

procedure TASMBlock.GetDCPUSource;
var
  LLine: string;
  LOffset: Integer;
begin
  LOffset := 1;
  for LLine in FSource do
  begin
    if (LLine <> '') and (LLine[1] <> ':') then
    begin
      AWriter.AddMapping(Self, LOffset, True);
    end;
    AWriter.Write(LLine);
    Inc(LOffset);
  end;
end;

end.
