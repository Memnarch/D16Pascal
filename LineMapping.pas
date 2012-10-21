unit LineMapping;

interface

type
  TLineMapping = class
  private
    FMemoryAddress: Word;
    FASMLine: Integer;
    FD16UnitName: string;
    FElementName: string;
    FUnitLine: Integer;
  public
    property D16UnitName: string read FD16UnitName write FD16UnitName;
    property ElementName: string read FElementName write FElementName;
    property UnitLine: Integer read FUnitLine write FUnitLine;
    property ASMLine: Integer read FASMLine write FASMLine;
    property MemoryAddress: Word read FMemoryAddress write FMemoryAddress;
  end;

implementation

end.
