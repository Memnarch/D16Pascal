unit Loops;

interface

uses
  Classes, Types, Generics.Collections, CodeElement;

type
  TLoop = class(TCodeElement)
  private
    FRelation: TObjectList<TCodeElement>;
  published
  public
    constructor Create(); reintroduce;
    destructor Destroy(); override;
    property Relation: TObjectList<TCodeElement> read FRelation;
  end;

  TWhileLoop = class(TLoop)
  public
    function GetDCPUSOurce(): string; override;
  end;

  TRepeatLoop = class(TLoop)
  public
    function GetDCPUSOurce(): string; override;
  end;

implementation


uses
  Optimizer;

{ TLoop }

constructor TLoop.Create;
begin
  inherited Create('');
  FRelation := TObjectList<TCodeElement>.Create();
end;

destructor TLoop.Destroy;
begin
  FRelation.Free;
  inherited;
end;

{ TWhileLoop }

function TWhileLoop.GetDCPUSOurce: string;
var
  LID: string;
  LEnd: string;
  LWhile: string;
begin
  LID := GetUniqueID();
  LWhile := 'While' + LID;
  LEnd := 'End' + LID;
  Result := ':' + LWhile + sLineBreak;
  Result := Result + FRelation.Items[0].GetDCPUSource();
  Result := Result + 'set x, pop' + sLineBreak;
  Result := Result + 'ife x, 0' + sLineBreak;
  Result := Result + 'set pc, ' + LEnd + sLineBreak;
  Result := OptimizeDCPUCode(Result);
  Result := Result + inherited;
  Result := Result + 'set pc, ' + LWhile + sLineBreak;
  Result := Result + ':' + LEnd;
end;

{ TRepeatLoop }

function TRepeatLoop.GetDCPUSOurce: string;
var
  LID: string;
  LRepeat, LRelSource: string;
begin
  LID := GetUniqueID();
  LRepeat := 'Repeat' + LID;
  Result := ':' + LRepeat + sLineBreak;
  Result := Result + inherited;
  LRelSource := FRelation.Items[0].GetDCPUSource();
  LRelSource := LRelSource + 'set x, pop' + sLineBreak;
  LRelSource := LRelSource + 'ifn x, 0' + sLineBreak;
  LRelSource := LRelSource + 'set pc, ' + LRepeat + sLineBreak;
  Result := Result + OptimizeDCPUCode(LRelSource);
end;

end.
