unit ASMBlock;

interface

uses
  Classes, Types, CodeElement;

type
  TASMBlock = class(TCodeElement)
  private
    FSource: string;
  published
  public
    function GetDCPUSource(): string; override;
    property Source: string read FSource write FSource;
  end;

implementation

{ TASMBlock }

function TASMBlock.GetDCPUSource: string;
begin
  Result := FSource;
end;

end.
