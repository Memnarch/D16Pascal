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
    property ProcDeclaration: TProcDeclaration read FProcDeclaration write FProcDeclaration;
    property Parameters: TObjectList<TCodeElement> read FParameters write FParameters;
  end;

implementation

uses
  Optimizer, SysUtils;

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
  LElement: TCodeElement;
  LRelSource: string;
  i: Integer;
begin
  Result := '';
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

end;

end.
