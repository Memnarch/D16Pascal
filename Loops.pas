unit Loops;

interface

uses
  Classes, Types, Generics.Collections, CodeElement;

type
  TLoop = class(TCodeElement)
  private
    FRelation: TObjectList<TCodeElement>;
  public
    constructor Create();
    destructor Destroy(); override;
    property Relation: TObjectList<TCodeElement> read FRelation;
  end;

  TWhileLoop = class(TLoop)
  public
    function GetDCPUSource(): string; override;
  end;

  TRepeatLoop = class(TLoop)
  public
    function GetDCPUSource(): string; override;
  end;

  TForLoop = class(TLoop)
  private
    FAssignment: TObjectList<TCodeElement>;
  public
    constructor Create();
    destructor Destroy(); override;
    function GetDCPUSource(): string; override;
    property Assignment: TObjectList<TCodeElement> read FAssignment;
  end;

implementation


uses
  Optimizer, Assignment;

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
  //inherited;
  Result := Result + inherited;
  Result := Result + 'set pc, ' + LWhile + sLineBreak;
  Result := Result + ':' + LEnd + sLineBreak;
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

{ TForLoop }

constructor TForLoop.Create;
begin
  inherited;
  FAssignment := TObjectList<TCodeElement>.Create();
end;

destructor TForLoop.Destroy;
begin
  FAssignment.Free;
  inherited;
end;

function TForLoop.GetDCPUSource: string;
var
  LID: string;
  LFor: string;
  LEnd: string;
  LVar: string;
begin
  LID := GetUniqueID('');
  LVar := TAssignment(Assignment.Items[0]).TargetVar.GetAccessIdentifier();
  if (TAssignment(Assignment.Items[0]).TargetVar.ParamIndex > 3) or (TAssignment(Assignment.Items[0]).TargetVar.ParamIndex < 1) then
  begin
    LVar := '[' + LVar + ']';
  end;
  LFor := 'for' + LID;
  LEnd := 'end' + LID;
  Result := OptimizeDCPUCode(Assignment.Items[0].GetDCPUSource());
  Result := Result + OptimizeDCPUCode(Relation.Items[0].GetDCPUSource());
  Result := Result + ':' + LFor + sLineBreak;
  Result := Result + 'set x, pop' + sLineBreak;
  Result := Result + 'ifg ' + LVar +
            ', x' + sLineBreak;
  Result := Result + 'set pc, ' + LEnd + sLineBreak;
  Result := Result + 'set push, x' + sLineBreak;
  Result := Result + inherited;
  Result := Result + 'add ' + LVar + ', 1' + sLineBreak;
  Result := Result + 'set pc, ' + LFor + sLineBreak;
  Result := Result + ':' + LEnd + sLineBreak;
end;

end.
