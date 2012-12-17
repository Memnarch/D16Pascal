unit CaseState;

interface

uses
  Classes, Types, Generics.Collections, CodeElement, WriterIntf;

type
  TCaseStatement = class(TCodeElement)
  private
    FCases: TObjectList<TCodeElement>;
    FRelation: TObjectList<TCodeElement>;
    FElseCase: TObjectList<TCodeElement>;
    FId: string;
    procedure GetJumpTable(AWriter: IWriter);
  public
    constructor Create(); reintroduce;
    destructor Destroy(); override;
    procedure GetDCPUSource(AWriter: IWriter); override;
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

procedure TCaseStatement.GetDCPUSource;
var
  i: Integer;
begin
  AWriter.AddMapping(Self);
  Relation.Items[0].GetDCPUSource(Self);
  OptimizeDCPUCode(Self.FSource, Self.FSource);
  AWriter.WriteList(Self.FSource);
  AWriter.Write('set x, pop');
  GetJumpTable(AWriter);
  for i := 0 to Cases.Count - 1 do
  begin
    AWriter.Write(':case' + IntToStr(i) + FId);
    Cases.Items[i].GetDCPUSource(AWriter);
    AWriter.Write('set pc, ' + 'end' + FId);
  end;
  if ElseCase.Count > 0 then
  begin
    AWriter.Write(':else' + FId);
    ElseCase.Items[0].GetDCPUSource(AWriter);
  end;
  AWriter.Write(':end' + FId);
end;

procedure TCaseStatement.GetJumpTable;
var
  LCase: TCase;
  LFactor: TFactor;
  i, k: Integer;
  LJumpLabel: string;
begin
  for i := 0 to FCases.Count - 1 do
  begin
    LJumpLabel := 'case' + IntToStr(i) + FId;
    LCase := TCase(FCases.Items[i]);
    for k := 0 to LCase.ConstValues.Count - 1 do
    begin
      LFactor := TFactor(LCase.ConstValues.Items[k]);
      if LFactor.IsConstant then
      begin
        AWriter.Write('ife x, ' + LFactor.Value);
      end
      else
      begin
        AWriter.Write('ife x, ' + LFactor.VarDeclaration.DefaultValue);
      end;
      AWriter.Write('set pc, ' + LJumpLabel);
    end;
  end;
  if ElseCase.Count > 0 then
  begin
    AWriter.Write('set pc, ' + 'else' + FId);
  end
  else
  begin
    AWriter.Write('set pc, ' + 'end' + FId);
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
