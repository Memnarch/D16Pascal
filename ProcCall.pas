unit ProcCall;

interface

uses
  Classes, Types, Generics.Collections, CodeElement, ProcDeclaration;

type
  TProcCall = class(TCodeElement)
  private
    FParameters: TObjectList<TCodeElement>;
    FProcDeclaration: TProcDeclaration;
  public
    constructor Create(); reintroduce;
    destructor Destroy(); override;
    function GetDCPUSource(): string; override;
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

function TProcCall.GetDCPUSource: string;
var
  LRelSource: string;
  i, LMax: Integer;
  LRegisters: string;
begin
  Result := '';
  LRegisters := 'abc';
  LMax := FParameters.Count;
  if LMax > 3 then
  begin
    LMax := 3;
  end;

  for i := 1 to LMax do
  begin
    Result := Result + 'set push, ' + LRegisters[i] + sLineBreak;
  end;

  for i := FParameters.Count - 1 downto 0 do
  begin
    LRelSource := FParameters.Items[i].GetDCPUSource();
    case i of
      0:
      begin
        LRelSource := LRelSource + 'set a, pop' + sLineBreak;
      end;
      1:
      begin
        LRelSource := LRelSource + 'set b, pop' + sLineBreak;
      end;
      2:
      begin
        LRelSource := LRelSource + 'set c, pop' + sLineBreak;
      end;
    end;
    Result := Result + OptimizeDCPUCode(LRelSource);
  end;
  Result := Result + 'jsr ' + ProcDeclaration.Name + sLineBreak;
  if FParameters.Count-3 > 0 then
  begin
    Result := Result + 'add sp, ' + IntToStr(FParameters.Count-3) + sLineBreak;
  end;
  if ProcDeclaration.IsFunction then
  begin
    Result := Result + 'set x, a' + sLineBreak;
  end;
  for i := 1 to LMax do
  begin
    Result := Result + 'set ' + LRegisters[i] + ', pop' + sLineBreak;;
  end;
  if ProcDeclaration.IsFunction then
  begin
    Result := Result + 'set push, x' + sLineBreak;
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
