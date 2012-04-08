unit Compiler;

interface

uses
  Classes, Types, Generics.Collections, SysUtils, Lexer, Token, CodeElement, DataType,
  VarDeclaration, PascalUnit, ProcDeclaration;

type
  TCompiler = class
  private
    FLexer: TLexer;
    FUnits: TObjectList<TPascalUnit>;
    FCurrentUnit: TPascalUnit;
    procedure CompileUnit();
    procedure RegisterType(AName: string; ASize: Integer = 2; APrimitive: TDataPrimitive = dpInteger);
    procedure RegisterBasicTypes();
    function GetElement(AName: string; AType: TCodeElementClass): TCodeElement;
    function ExpectElement(AName: string; AType: TCodeElementClass): TCodeElement;
    function GetDataType(AName: string): TDataType;
    function GetVar(AName: string): TVarDeclaration;
    //parse functions
    procedure ParseUnitHeader(AUnit: TPascalUnit);
    procedure ParseUnitFooter();
    procedure ParseUses(AScope: TObjectList<TCodeElement>);
    procedure ParseVars(AScope: TObjectList<TCodeElement>);
    procedure ParseVarDeclaration(AScope: TObjectList<TCodeElement>; AIncludeEndMark: Boolean = True);
    procedure ParseConsts(AScope: TObjectList<TCodeElement>);
    procedure ParseRoutineDeclaration(AScope: TObjectList<TCodeElement>);
    procedure ParseRoutineParameters(AScope: TObjectList<TCodeElement>);
    procedure ParseRoutineContent(AScope: TObjectList<TCodeElement>);
    procedure ParseRoutineCall(AScope: TObjectList<TCodeElement>; AIncludeEndMark: Boolean = True);
    procedure ParseAssignment(AScope: TObjectList<TCodeElement>);
    procedure ParseCondition(AScope: TObjectList<TCodeElement>);
    procedure ParseWhileLoop(AScope: TObjectList<TCodeElement>);
    procedure ParseRepeatLoop(AScope: TObjectList<TCodeElement>);
    procedure ParseRelation(AScope: TObjectList<TCodeElement>);
    procedure ParseExpression(AScope: TObjectList<TCodeElement>);
    procedure ParseTerm(AScope: TObjectList<TCodeElement>);
    procedure ParseFactor(AScope: TObjectList<TCodeElement>);
    procedure Fatal(AMessage: string);
  public
    constructor Create();
    destructor Destroy(); reintroduce;
    function GetDCPUSource(): string;
    procedure CompileFile(AFile: string);
    procedure CompilerSource(ASource: string);
  end;

const
  CRegA = 'A';
  CRegB = 'B';
  CRegC = 'C';
  CRegI = 'I';
  CRegJ = 'J';
  CRegSP = 'SP';
  CRegPC = 'PC';

implementation

uses
  StrUtils, Relation, Expression, Term, Factor, Assignment, Condition, Loops, ProcCall;

{ TCompiler }

procedure TCompiler.CompileFile(AFile: string);
begin
  FLexer.LoadFromFile(AFile);
  CompileUnit();
end;

procedure TCompiler.CompilerSource(ASource: string);
begin
  FLexer.LoadFromString(ASource);
  CompileUnit();
end;

procedure TCompiler.CompileUnit;
var
  LUnit: TPascalUnit;
  LLastUnit: TPascalUnit;
begin
  LUnit := TPascalUnit.Create('');
  LLastUnit := FCurrentUnit;
  FUnits.Add(LUnit);
  FCurrentUnit := LUnit;
  if FUnits.Count = 1 then
  begin
    RegisterBasicTypes();
  end;
  ParseUnitHeader(LUnit);
  while True do
  begin
    case AnsiIndexText(FLexer.PeekToken.Content, ['uses', 'var', 'const', 'procedure', 'function']) of
      0:
      begin
        ParseUses(LUnit.SubElements);
      end;

      1:
      begin
        ParseVars(LUnit.SubElements);
      end;

      2:
      begin
        ParseConsts(LUnit.SubElements);
      end;

      3, 4:
      begin
        ParseRoutineDeclaration(LUnit.SubElements);
      end;

      else
        break;
    end;
  end;
  ParseUnitFooter();
  FCurrentUnit := LLastUnit;
end;

constructor TCompiler.Create;
begin
  FLexer := TLexer.Create();
  FUnits := TObjectList<TPascalUnit>.Create();
end;

destructor TCompiler.Destroy;
begin
  FLexer.Free;
  FUnits.Free;
  inherited;
end;

function TCompiler.ExpectElement(AName: string;
  AType: TCodeElementClass): TCodeElement;
begin
  Result := GetElement(AName, AType);
  if not Assigned(Result) then
  begin
    Fatal('Undeclared identifier ' + QuotedStr(AName));
  end;
end;

procedure TCompiler.Fatal(AMessage: string);
begin
  raise Exception.Create(AMessage);
end;

function TCompiler.GetDataType(AName: string): TDataType;
begin
  Result := TDataType(GetElement(AName, TDataType));
  if not Assigned(Result) then
  begin
    Fatal(QuotedStr(AName) + ' is not a declared datatype');
  end;
end;

function TCompiler.GetDCPUSource: string;
var
  LUnit: TPascalUnit;
begin
  Result := '';
  for LUnit in FUnits do
  begin
    Result := Result + LUnit.GetDCPUSource();
  end;
end;

function TCompiler.GetElement(AName: string;
  AType: TCodeElementClass): TCodeElement;
var
  LUnit: TPascalUnit;
  LElement: TCodeElement;
begin
  Result := nil;
  for LUnit in FUnits do
  begin
    LElement := LUnit.GetElement(AName, AType);
    if Assigned(LElement) then
    begin
      Result := LElement;
      Break;
    end;
  end;
end;

function TCompiler.GetVar(AName: string): TVarDeclaration;
begin
  Result := TVarDeclaration(GetElement(AName, TVarDeclaration));
  if not Assigned(Result) then
  begin
    Fatal(QuotedStr(AName) + ' is not a declared Var');
  end;
end;

procedure TCompiler.ParseAssignment(AScope: TObjectList<TCodeElement>);
var
  LAssignment: TAssignment;
begin
  LAssignment := TAssignment.Create('');
  LAssignment.TargetVar := GetVar(FLexer.GetToken('', ttIdentifier).Content);
  AScope.Add(LAssignment);
  FLexer.GetToken(':=');
  ParseRelation(LAssignment.SubElements);
  FLexer.GetToken(';');
end;

procedure TCompiler.ParseCondition(AScope: TObjectList<TCodeElement>);
var
  LCondition: TCondition;
begin
  LCondition := TCondition.Create();
  AScope.Add(LCondition);
  FLexer.GetToken('if');
  ParseRelation(LCondition.Relation);
  FLexer.GetToken('then');
  FLexer.GetToken('begin');
  ParseRoutineContent(LCondition.SubElements);
  FLexer.GetToken('end');
  if FLexer.PeekToken.IsContent('else') then
  begin
    FLexer.GetToken('else');
    FLexer.GetToken('begin');
    ParseRoutineContent(LCondition.ElseElements);
    FLexer.GetToken('end');
  end;
  FLexer.GetToken(';');
end;

procedure TCompiler.ParseConsts;
begin

end;

procedure TCompiler.ParseExpression(AScope: TObjectList<TCodeElement>);
var
  LExpression: TExpression;
begin
  LExpression := TExpression.Create();
  AScope.Add(LExpression);
  ParseTerm(LExpression.SubElements);
  while FLexer.PeekToken.IsType(ttTermOp) do
  begin
    LExpression.Operators.Add(FLexer.GetToken().Content);
    ParseTerm(LExpression.SubElements);
  end;
end;

procedure TCompiler.ParseFactor(AScope: TObjectList<TCodeElement>);
var
  LFactor: TFactor;
begin
  if FLexer.PeekToken.IsContent('(') then
  begin
    FLexer.GetToken('(');
    ParseRelation(AScope);
    FLexer.GetToken(')');
  end
  else
  begin
    if Assigned(GetElement(FLexer.PeekToken.Content, TProcDeclaration)) then
    begin
      ParseRoutineCall(AScope, False);
    end
    else
    begin
      LFactor := TFactor.Create();
      AScope.Add(LFactor);
      if FLexer.PeekToken.IsType(ttNumber) then
      begin
        LFactor.Value := FLexer.GetToken().Content;
      end
      else
      begin
        LFactor.VarDeclaration := GetVar(FLexer.GetToken('', ttIdentifier).Content);
      end;
    end;
  end;
end;

procedure TCompiler.ParseRelation(AScope: TObjectList<TCodeElement>);
var
  LRelation: TRelation;
begin
  LRelation := TRelation.Create();
  AScope.Add(LRelation);
  ParseExpression(LRelation.SubElements);
  if FLexer.PeekToken.IsType(ttRelOp) then
  begin
    LRelation.Operators.Add(FLexer.GetToken().Content);
    ParseExpression(LRelation.SubElements);
  end;
end;

procedure TCompiler.ParseRepeatLoop(AScope: TObjectList<TCodeElement>);
var
  LLoop: TRepeatLoop;
begin
  LLoop := TRepeatLoop.Create();
  AScope.Add(LLoop);
  FLexer.GetToken('repeat');
  ParseRoutineContent(LLoop.SubElements);
  FLexer.GetToken('until');
  ParseRelation(LLoop.Relation);
  FLexer.GetToken(';');
end;

procedure TCompiler.ParseRoutineCall(AScope: TObjectList<TCodeElement>; AIncludeEndMark: Boolean = True);
var
  LCall: TProcCall;
  i: Integer;
begin
  LCall := TProcCall.Create();
  AScope.Add(LCall);
  LCall.ProcDeclaration := TProcDeclaration(ExpectElement(FLexer.GetToken().Content, TProcDeclaration));
  FLexer.GetToken('(');
  for i := 0 to LCall.ProcDeclaration.Parameters.Count - 1 do
  begin
    ParseRelation(LCall.Parameters);
    if i < LCall.ProcDeclaration.Parameters.Count - 1 then
    begin
      FLexer.GetToken(',');
    end;
  end;
  FLexer.GetToken(')');
  if AIncludeEndMark then
  begin
    FLexer.GetToken(';');
  end;
end;

procedure TCompiler.ParseRoutineContent(AScope: TObjectList<TCodeElement>);
var
  LElement: TCodeElement;
begin
  while not FLexer.PeekToken.IsContent('end') do
  begin
    case FLexer.PeekToken.TokenType of
      ttIdentifier:
      begin
        case AnsiIndexText(FLexer.PeekToken.Content, ['if', 'while', 'repeat']) of
          0:
          begin
            ParseCondition(AScope);
          end;
          1:
          begin
            ParseWhileLoop(AScope);
          end;
          2:
          begin
            ParseRepeatLoop(AScope);
          end;
          else
            LElement := ExpectElement(FLexer.PeekToken.Content, TCodeElement);
            if LElement is TVarDeclaration then
            begin
              ParseAssignment(AScope);
            end
            else
            begin
              ParseRoutineCall(AScope);
            end;
        end;
      end;

      else
        Break;
    end;
  end;
end;

procedure TCompiler.ParseRoutineDeclaration;
var
  LProc: TProcDeclaration;
begin
  FLexer.GetToken('procedure');
  LProc := TProcDeclaration.Create(FLexer.GetToken('', ttIdentifier).Content);
  AScope.Add(LProc);
  ParseRoutineParameters(LProc.Parameters);
  FLexer.GetToken(';');
  if FLexer.PeekToken.IsContent('begin') then
  begin
    FLexer.GetToken('begin');
    ParseRoutineContent(LProc.SubElements);
  end;
  FLexer.GetToken('end');
  FLexer.GetToken(';');
end;

procedure TCompiler.ParseRoutineParameters(AScope: TObjectList<TCodeElement>);
begin
  FLexer.GetToken('(');
  while not FLexer.PeekToken.IsContent(')') do
  begin
    ParseVarDeclaration(AScope, False);
    if not FLexer.PeekToken.IsContent(')') then
    begin
      FLexer.GetToken(';');
    end;
  end;
  FLexer.GetToken(')');
end;

procedure TCompiler.ParseTerm(AScope: TObjectList<TCodeElement>);
var
  LTerm: TTerm;
begin
  LTerm := TTerm.Create();
  AScope.Add(LTerm);
  ParseFactor(LTerm.SubElements);
  while FLexer.PeekToken.IsType(ttFacOp) do
  begin
    LTerm.Operators.Add(FLexer.GetToken().Content);
    ParseFactor(LTerm.SubElements);
  end;
end;

procedure TCompiler.ParseUnitFooter;
begin
  FLexer.GetToken.MatchContent('end');
  FLexer.GetToken.MatchContent('.');
end;

procedure TCompiler.ParseUnitHeader;
begin
  FLexer.GetToken.MatchContent('unit');
  FLexer.GetToken.MatchType(ttIdentifier);
  FLexer.GetToken.MatchContent(';');
end;

procedure TCompiler.ParseUses;
begin

end;

procedure TCompiler.ParseVarDeclaration;
var
  LNames: TStringList;
  LName: string;
  LRepeat: Boolean;
  LType: TDataType;
begin
  LNames := TStringList.Create();
  LRepeat := False;
  while (not FLexer.PeekToken.IsContent(':')) or LRepeat do
  begin
    LNames.Add(FLexer.GetToken('', ttIdentifier).Content);
    LRepeat := False;
    if FLexer.PeekToken.IsContent(',') then
    begin
      FLexer.GetToken();
      LRepeat := True;
    end;
  end;
  FLexer.GetToken(':');
  LType := GetDataType(FLexer.GetToken('', ttIdentifier).Content);
  if AIncludeEndMark then
  begin
    FLexer.GetToken(';');
  end;
  for LName in LNames do
  begin
    //RegisterVar(LName, LType);
    AScope.Add(TVarDeclaration.Create(LName, LType));
  end;
  LNames.Free;
end;

procedure TCompiler.ParseVars;
begin
  FLexer.GetToken.MatchContent('var');
  while not FLexer.PeekToken.IsType(ttReserved) do
  begin
    ParseVarDeclaration(AScope);
  end;
end;

procedure TCompiler.ParseWhileLoop(AScope: TObjectList<TCodeElement>);
var
  LLoop: TWhileLoop;
begin
  LLoop := TWhileLoop.Create();
  AScope.Add(LLoop);
  FLexer.GetToken('while');
  ParseRelation(LLoop.Relation);
  FLexer.GetToken('do');
  FLexer.GetToken('begin');
  ParseRoutineContent(LLoop.SubElements);
  FLexer.GetToken('end');
  FLexer.GetToken(';');
end;


procedure TCompiler.RegisterBasicTypes;
begin
  RegisterType('word');
end;

procedure TCompiler.RegisterType(AName: string; ASize: Integer;
  APrimitive: TDataPrimitive);
begin
  FCurrentUnit.SubElements.Add(TDataType.Create(AName, ASize, APrimitive));
end;

end.
