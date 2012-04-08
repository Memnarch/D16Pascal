unit Condition;

interface

uses
  Classes, Types, Generics.Collections, CodeElement;

type
  TCondition = class(TCodeElement)
  private
    FElseElements: TObjectList<TCodeElement>;
    FRelation: TObjectList<TCodeElement>;
  public
    constructor Create(); reintroduce;
    destructor Destroy(); override;
    function GetDCPUSource(): string; override;
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

function TCondition.GetDCPUSource: string;
var
  LID, LEnd, LElse: string;
  LElement: TCodeElement;
begin
  LID := GetUniqueID();
  LEnd := 'End' + LID;
  LElse := 'Else' +  LID;
  Result := FRelation.Items[0].GetDCPUSource();
  Result := Result + 'set x, pop' + sLineBreak;
  Result := Result + 'ife x, 0' + sLineBreak;
  Result := OptimizeDCPUCode(Result);
  if FElseElements.Count > 0 then
  begin
    Result := Result + 'set pc, ' + LElse + sLineBreak;
  end
  else
  begin
    Result := Result + 'set pc, ' + LEnd + sLineBreak;
  end;
  Result := Result + inherited;
  if FElseElements.Count > 0 then
  begin
    Result := Result + 'set pc, ' + LEnd;
    Result := Result + ':' + LElse + sLineBreak;
    for LElement in FElseElements do
    begin
      Result := Result + LElement.GetDCPUSource();
    end;
  end;
  Result := Result + ':' + LEnd + sLineBreak;
end;

end.
