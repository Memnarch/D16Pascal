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
begin
  AWriter.WriteList(FSource);
end;

end.
