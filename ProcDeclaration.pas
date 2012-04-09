unit ProcDeclaration;

interface

uses
  Classes, Types, CodeElement, DataType, VarDeclaration, Generics.Collections;

type
  TArguments = array of TVarDeclaration;

  TProcDeclaration = class(TCodeElement)
  private
    FResultType: TDataType;
    FParameters: TObjectList<TCodeElement>;
    FLocals: TObjectList<TCodeElement>;
    function GetIsFunction: Boolean;
  public
    constructor Create(AName: string); reintroduce;
    procedure AddResultValue();
    procedure AddLocal(AVAr: TVarDeclaration);
    function GetDCPUSource(): string; override;
    function GetElement(AName: string; AType: TCodeElementClass): TCodeElement;
    property IsFunction: Boolean read GetIsFunction;
    property ResultType: TDataType read FResultType write FResultType;
    property Parameters: TObjectList<TCodeElement> read FParameters;
    property Locals: TObjectList<TCodeElement> read FLocals;
  end;

implementation

uses
  SysUtils;

{ TProcDeclaration }


{ TProcDeclaration }

procedure TProcDeclaration.AddLocal(AVar: TVarDeclaration);
var
  LElement: TCodeElement;
begin
  for LElement in FParameters do
  begin
    if TVarDeclaration(LElement).ParamIndex > 2 then
    begin
      TVarDeclaration(LElement).ParamIndex := TVarDeclaration(LElement).ParamIndex +1;
    end;
  end;
  for LElement in FLocals do
  begin
    TVarDeclaration(LElement).ParamIndex := TVarDeclaration(LElement).ParamIndex  - 1;
  end;
  AVar.ParamIndex := -1;
  FLocals.Add(AVar);
end;

procedure TProcDeclaration.AddResultValue;
begin
  AddLocal(TVarDeclaration.Create('Result', ResultType));
end;

constructor TProcDeclaration.Create(AName: string);
begin
  inherited;
  FParameters := TObjectList<TCodeElement>.Create();
  FLocals := TObjectList<TCodeElement>.Create();
end;

function TProcDeclaration.GetDCPUSource: string;
begin
  Result := ':' + Name + sLineBreak;
  Result := Result + 'set push, j' + sLineBreak;
  if FLocals.Count > 0 then
  begin
    Result := Result + 'sub sp, ' + IntToStr(FLocals.Count) + sLineBreak;
  end;
  Result := Result + 'set j, sp' + sLineBreak;
  Result := Result + inherited GetDCPUSource();
  if IsFunction then
  begin
    Result := Result + 'set a, [' +
      TVarDeclaration(GetElement('Result', TVarDeclaration)).GetAccessIdentifier() + ']' + sLineBreak;
  end;
  Result := Result + 'set sp, j' + sLineBreak;
  if FLocals.Count > 0 then
  begin
    Result := Result + 'add sp, ' + IntToStr(FLocals.Count) + sLineBreak;
  end;
  Result := Result + 'set j, pop' + sLineBreak;
  Result := Result + 'set pc, pop' + sLineBreak;
end;

function TProcDeclaration.GetElement(AName: string;
  AType: TCodeElementClass): TCodeElement;
var
  LElement: TCodeElement;
begin
  Result := nil;
  for LElement in FParameters do
  begin
    if SameText(LElement.Name, AName) and LElement.InheritsFrom(AType) then
    begin
      Result := LElement;
      Exit;
    end;
  end;

  for LElement in FLocals do
  begin
    if SameText(LElement.Name, AName) and LElement.InheritsFrom(AType) then
    begin
      Result := LElement;
      Exit;
    end;
  end;
  if not Assigned(Result) then
  begin
    Result := inherited;
  end;
end;

function TProcDeclaration.GetIsFunction: Boolean;
begin
  Result := Assigned(FResultType);
end;

end.
