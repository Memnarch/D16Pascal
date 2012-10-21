unit ProcCall;

interface

uses
  Classes, Types, Generics.Collections, CodeElement, ProcDeclaration, WriterIntf;

type
  TProcCall = class(TCodeElement)
  private
    FParameters: TObjectList<TCodeElement>;
    FProcDeclaration: TProcDeclaration;
  public
    constructor Create(); 
    destructor Destroy(); override;
    procedure GetDCPUSource(AWriter: IWriter); override;
    function GetWordSizeOfLocals(): Integer;
    property ProcDeclaration: TProcDeclaration read FProcDeclaration write FProcDeclaration;
    property Parameters: TObjectList<TCodeElement> read FParameters write FParameters;
  end;

implementation

uses
  Optimizer, SysUtils, VarDeclaration;

{ TProcCall }

constructor TProcCall.Create;
begin
  inherited Create('');
  FParameters := TObjectList<TCodeElement>.Create();
end;

destructor TProcCall.Destroy;
begin
  FParameters.Free;
  inherited;
end;

procedure TProcCall.GetDCPUSource;
var
  i, LMax: Integer;
  LRegisters: string;
begin
  LRegisters := 'abc';
  LMax := FParameters.Count;
  if LMax > 3 then
  begin
    LMax := 3;
  end;

  for i := 1 to LMax do
  begin
    AWriter.Write('set push, ' + LRegisters[i]);
  end;

  for i := FParameters.Count - 1 downto 0 do
  begin
    Self.FSource.Clear();
    FParameters.Items[i].GetDCPUSource(Self);
    case i of
      0:
      begin
        Self.Write('set a, pop');
      end;
      1:
      begin
        Self.Write('set b, pop');
      end;
      2:
      begin
        Self.Write('set c, pop');
      end;
    end;
    OptimizeDCPUCode(Self.FSource, Self.FSource);
    AWriter.WriteList(Self.FSource);
  end;
  AWriter.Write('jsr ' + ProcDeclaration.Name);
  if FParameters.Count-3 > 0 then
  begin
    AWriter.Write('add sp, ' + IntToStr(FParameters.Count-3));
  end;
  if ProcDeclaration.IsFunction then
  begin
    AWriter.Write('set x, a');
  end;
  for i := 1 to LMax do
  begin
    AWriter.Write('set ' + LRegisters[i] + ', pop');
  end;
  if ProcDeclaration.IsFunction then
  begin
    AWriter.Write('set push, x');
  end;

end;

function TProcCall.GetWordSizeOfLocals: Integer;
var
  LElement: TCodeElement;
begin
  Result := 0;
  for LElement in FParameters do
  begin
    if TVarDeclaration(LElement).IsLocal then
    begin
      Result := Result + TVarDeclaration(LElement).DataType.GetRamWordSize();
    end;
  end;
end;

end.
