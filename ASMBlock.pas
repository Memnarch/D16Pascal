unit ASMBlock;

interface

uses
  Classes, Types, CodeElement, WriterIntf;

type
  TASMBlock = class(TCodeElement)
  private
    FSource: string;
  public
    procedure GetDCPUSource(AWriter: IWriter); override;
    property Source: string read FSource write FSource;
  end;

implementation

{ TASMBlock }

procedure TASMBlock.GetDCPUSource;
begin
  AWriter.Write(FSource);
end;

end.
