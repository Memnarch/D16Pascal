unit Loops;

interface

uses
  Classes, Types, Generics.Collections, CodeElement, WriterIntf;

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
    procedure GetDCPUSource(AWriter: IWriter); override;
  end;

  TRepeatLoop = class(TLoop)
  public
    procedure GetDCPUSource(AWriter: IWriter); override;
  end;

  TForLoop = class(TLoop)
  private
    FAssignment: TObjectList<TCodeElement>;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure GetDCPUSource(AWriter: IWriter); override;
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

procedure TWhileLoop.GetDCPUSOurce;
var
  LID: string;
  LEnd: string;
  LWhile: string;
begin
  AWriter.AddMapping(Self);
  LID := GetUniqueID();
  LWhile := 'While' + LID;
  LEnd := 'End' + LID;
  Self.Write(':' + LWhile);
  FRelation.Items[0].GetDCPUSource(Self);
  Self.Write('set x, pop');
  Self.Write('ife x, 0');
  Self.Write('set pc, ' + LEnd);
  OptimizeDCPUCode(Self.FSource, Self.FSource);
  AWriter.WriteList(Self.FSource);
  inherited;
  AWriter.Write('set pc, ' + LWhile);
  AWriter.Write(':' + LEnd);
end;

{ TRepeatLoop }

procedure TRepeatLoop.GetDCPUSOurce;
var
  LID: string;
  LRepeat: string;
begin
  AWriter.AddMapping(Self);
  LID := GetUniqueID();
  LRepeat := 'Repeat' + LID;
  AWriter.Write(':' + LRepeat);
  inherited;
  FRelation.Items[0].GetDCPUSource(Self);
  Self.Write('set x, pop');
  Self.Write('ifn x, 0');
  Self.Write('set pc, ' + LRepeat);
  OptimizeDCPUCode(Self.FSource, Self.FSource);
  AWriter.WriteList(Self.FSource);
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

procedure TForLoop.GetDCPUSource;
var
  LID: string;
  LFor: string;
  LEnd: string;
  LVar: string;
begin
  AWriter.AddMapping(Self);
  LID := GetUniqueID('');
  LVar := TAssignment(Assignment.Items[0]).TargetVar.GetAccessIdentifier();
  if (TAssignment(Assignment.Items[0]).TargetVar.ParamIndex > 3) or (TAssignment(Assignment.Items[0]).TargetVar.ParamIndex < 1) then
  begin
    LVar := '[' + LVar + ']';
  end;
  LFor := 'for' + LID;
  LEnd := 'end' + LID;
  Assignment.Items[0].GetDCPUSource(Self);
  OptimizeDCPUCode(Self.FSource, Self.FSource);
  AWriter.WriteList(Self.FSource);
  Self.FSource.Clear; //clear it, because we are going to write a new statement to it
  Relation.Items[0].GetDCPUSource(Self);
  OptimizeDCPUCode(Self.FSource, Self.FSource);
  AWriter.WriteList(Self.FSource);
  AWriter.Write(':' + LFor);
  AWriter.Write('set x, pop');
  AWriter.Write('ifg ' + LVar + ', x');
  AWriter.Write('set pc, ' + LEnd);
  AWriter.Write('set push, x');
  inherited;
  AWriter.Write('add ' + LVar + ', 1');
  AWriter.Write('set pc, ' + LFor);
  AWriter.Write(':' + LEnd);
end;

end.
