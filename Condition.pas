unit Condition;

interface

uses
  Classes, Types, Generics.Collections, CodeElement, WriterIntf;

type
  TCondition = class(TCodeElement)
  private
    FElseElements: TObjectList<TCodeElement>;
    FRelation: TObjectList<TCodeElement>;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure GetDCPUSource(AWriter: IWriter); override;
    property ElseElements: TObjectList<TCodeElement> read FElseElements;
    property Relation: TObjectList<TCodeElement> read FRelation;
  end;

implementation

uses
  Optimizer;

{ TCondition }

constructor TCondition.Create;
begin
  inherited Create('');
  FElseElements := TObjectList<TCodeElement>.Create();
  FRelation := TObjectList<TCodeElement>.Create();
end;

destructor TCondition.Destroy;
begin
  FElseElements.Free;
  FRelation.Free;
  inherited;
end;

procedure TCondition.GetDCPUSource;
var
  LID, LEnd, LElse: string;
  LElement: TCodeElement;
begin
  LID := GetUniqueID();
  LEnd := 'End' + LID;
  LElse := 'Else' +  LID;
  FRelation.Items[0].GetDCPUSource(Self);
  Self.Write('set x, pop');
  Self.Write('ife x, 0');
  OptimizeDCPUCode(Self.FSource, Self.FSource);
  AWriter.WriteList(Self.FSource);
  if FElseElements.Count > 0 then
  begin
    AWriter.Write('set pc, ' + LElse);
  end
  else
  begin
    AWriter.Write('set pc, ' + LEnd);
  end;
  inherited;
  if FElseElements.Count > 0 then
  begin
    AWriter.Write('set pc, ' + LEnd);
    AWriter.Write(':' + LElse);
    for LElement in FElseElements do
    begin
      LElement.GetDCPUSource(AWriter);
    end;
  end;
  AWriter.Write(':' + LEnd);
end;

end.
