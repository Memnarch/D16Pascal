unit ASMBlock;

interface

uses
  Classes, Types, CodeElement, WriterIntf;

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
  LLIne: string;
  LOffset: Integer;
begin
  LOffset := 1;
  for LLine in FSource do
  begin
    AWriter.AddMapping(Self, LOffset);
    AWriter.Write(LLIne);
    Inc(LOffset);
  end;
end;

end.
