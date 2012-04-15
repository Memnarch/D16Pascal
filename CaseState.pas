unit CaseState;

interface

uses
  Classes, Types, Generics.Collections, CodeElement;

type
  TCaseStatement = class(TCodeElement)
  private
    FCases: TObjectList<TCodeElement>;
    FRelation: TObjectList<TCodeElement>;
    FElseCase: TObjectList<TCodeElement>;
    FId: string;
    function GetJumpTable(): string;
  public
    constructor Create(); reintroduce;
    destructor Destroy(); override;
    function GetDCPUSource(): string; override;
    property Relation: TObjectList<TCodeElement> read FRelation;
    property Cases: TObjectList<TCodeElement> read FCases;
    property ElseCase: TObjectList<TCodeElement> read FElseCase;
  end;

  TCase = class(TCodeElement)
  private
    FConstValues: TObjectList<TCodeElement>;
  public
    constructor Create(); reintroduce;
    destructor Destroy(); override;
    property ConstValues: TObjectList<TCodeElement> read FConstValues;
  end;

implementation

uses
  SysUtils, Factor, Optimizer;

{ TCaseStatement }

constructor TCaseStatement.Create;
begin
  inherited Create('');
  FRelation := TObjectList<TCodeElement>.Create();
  FCases := TObjectList<TCodeElement>.Create();
  FElseCase := TObjectList<TCodeElement>.Create();
  FId := GetUniqueID();
end;

destructor TCaseStatement.Destroy;
begin
  FRelation.Free;
  FCases.Free;
  FElseCase.Free;
  inherited;
end;

function TCaseStatement.GetDCPUSource: string;
var
  LLabel: string;
  LCase: TCase;
  i: Integer;
begin
  Result := OptimizeDCPUCode(Relation.Items[0].GetDCPUSource());
  Result := Result + 'set x, pop' + sLineBreak;
  Result := Result + GetJumpTable();
  for i := 0 to Cases.Count - 1 do
  begin
    Result := Result + ':case' + IntToStr(i) + FId + sLineBreak;
    Result := Result + Cases.Items[i].GetDCPUSource();
    Result := Result + 'set pc, ' + 'end' + FId + sLineBreak;
  end;
  if ElseCase.Count > 0 then
  begin
    Result := Result + ':else' + FId + sLineBreak;
    Result := Result + ElseCase.Items[0].GetDCPUSource();
  end;
  Result := Result + ':end' + FId + sLineBreak;
end;

function TCaseStatement.GetJumpTable: string;
var
  LCase: TCase;
  LFactor: TFactor;
  i, k: Integer;
  LJumpLabel: string;
begin
  Result := '';
  for i := 0 to FCases.Count - 1 do
  begin
    LJumpLabel := 'case' + IntToStr(i) + FId;
    LCase := TCase(FCases.Items[i]);
    for k := 0 to LCase.ConstValues.Count - 1 do
    begin
      LFactor := TFactor(LCase.ConstValues.Items[k]);
      if LFactor.IsConstant then
      begin
        Result := Result + 'ife x, ' + LFactor.Value + sLineBreak;
      end
      else
      begin
        Result := Result + 'ife x, ' + LFactor.VarDeclaration.DefaultValue + sLineBreak;
      end;
      Result := Result + 'set pc, ' + LJumpLabel + sLineBreak;
    end;
  end;
  if ElseCase.Count > 0 then
  begin
    Result := Result + 'set pc, ' + 'else' + FId + sLineBreak;
  end
  else
  begin
    Result := Result + 'set pc, ' + 'end' + FId + sLineBreak;
  end;
end;

{ TCase }

constructor TCase.Create;
begin
  inherited Create('');
  FConstValues := TObjectList<TCodeElement>.Create();
end;

destructor TCase.Destroy;
begin
  FConstValues.Free;
  inherited;
end;

end.
