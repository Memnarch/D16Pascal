unit Compiler;

interface

uses
  Classes, Types, Generics.Collections, SysUtils, Lexer, Token, CodeElement, DataType,
  VarDeclaration, PascalUnit, ProcDeclaration, ASMBlock;

type
  TMessageLevel = (mlNone, mlWarning, mlError, mlFatal);
  TOnMessage = procedure(AMessage, AUnitName: string; ALine: Integer; ALevel: TMessageLevel) of object;

  TCompiler = class
  private
    FLexer: TLexer;
    FUnits: TObjectList<TPascalUnit>;
    FCurrentUnit: TPascalUnit;
    FCurrentProc: TProcDeclaration;
    FSearchPath: TStringlist;
    FOnMessage: TOnMessage;
    procedure CompileUnit(AUnit: TPascalUnit);
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
    procedure ParseVarDeclaration(AScope: TObjectList<TCodeElement>; AIncludeEndMark: Boolean = True;
      AAsParameter: Boolean = False; AAsLocal: Boolean = False);
    procedure ParseConsts(AScope: TObjectList<TCodeElement>);
    procedure ParseRoutineDeclaration(AScope: TObjectList<TCodeElement>);
    procedure ParseRoutineParameters(AScope: TObjectList<TCodeElement>);
    procedure ParseRoutineLocals(AProc: TProcDeclaration);
    procedure ParseRoutineContent(AScope: TObjectList<TCodeElement>);
    procedure ParseRoutineCall(AScope: TObjectList<TCodeElement>; AIncludeEndMark: Boolean = True);
    procedure ParseAssignment(AScope: TObjectList<TCodeElement>);
    procedure ParseCondition(AScope: TObjectList<TCodeElement>);
    procedure ParseWhileLoop(AScope: TObjectList<TCodeElement>);
    procedure ParseRepeatLoop(AScope: TObjectList<TCodeElement>);
    procedure ParseRelation(AScope: TObjectList<TCodeElement>; AInverse: Boolean = False);
    procedure ParseExpression(AScope: TObjectList<TCodeElement>);
    procedure ParseTerm(AScope: TObjectList<TCodeElement>);
    procedure ParseFactor(AScope: TObjectList<TCodeElement>);
    procedure ParseASMBlock(AScope: TObjectList<TCodeElement>);
    procedure Fatal(AMessage: string);
    procedure DoMessage(AMessage, AUnit: string; ALine: Integer; ALevel: TMessageLevel);
    function GetPathForFile(AFile: string): string;
  public
    constructor Create();
    destructor Destroy(); reintroduce;
    function GetDCPUSource(): string;
    procedure CompileFile(AFile: string);
    procedure CompilerSource(ASource: string);
    property SearchPath: TStringlist read FSearchPath write FSearchPath;
    property OnMessage: TOnMessage read FOnMessage write FOnMessage;
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
  StrUtils, Relation, Expression, Term, Factor, Assignment, Condition, Loops, ProcCall, Optimizer;

{ TCompiler }

procedure TCompiler.CompileFile(AFile: string);
var
  LUnit: TPascalUnit;
begin
  DoMessage('Compiling ' + QuotedStr(AFile), '', 0, mlNone);
  LUnit := TPascalUnit.Create('');
  LUnit.Lexer.LoadFromFile(GetPathForFile(AFile));
  CompileUnit(LUnit);
end;

procedure TCompiler.CompilerSource(ASource: string);
var
  LUnit: TPascalUnit;
begin
  LUnit := TPascalUnit.Create('');
  LUnit.Lexer.LoadFromString(ASource);
  CompileUnit(LUnit);
end;

procedure TCompiler.CompileUnit;
var
  LLastUnit: TPascalUnit;
  LLastLexer: TLexer;
begin
  LLastUnit := FCurrentUnit;
  LLastLexer := FLexer;
  FUnits.Add(AUnit);
  FCurrentUnit := AUnit;
  FLexer := AUnit.Lexer;
  if FUnits.Count = 1 then
  begin
    RegisterBasicTypes();
  end;
  try
    try
      ParseUnitHeader(AUnit);
      while True do
      begin
        case AnsiIndexText(FLexer.PeekToken.Content, ['uses', 'var', 'const', 'procedure', 'function']) of
          0:
          begin
            ParseUses(AUnit.SubElements);
          end;

          1:
          begin
            ParseVars(AUnit.SubElements);
          end;

          2:
          begin
            ParseConsts(AUnit.SubElements);
          end;

          3, 4:
          begin
            ParseRoutineDeclaration(AUnit.SubElements);
          end;

          else
            break;
        end;
      end;
      if FLexer.PeekToken.IsContent('begin') then
      begin
        FLexer.GetToken();
        ParseRoutineContent(AUnit.InitSection);
      end;
      ParseUnitFooter();
    except
      on E: Exception do
      DoMessage(E.Message, AUnit.Name, AUnit.Lexer.PeekToken.FoundInLine, mlFatal);
    end;
  finally
    FCurrentUnit := LLastUnit;
    FLexer := LLastLexer;
  end;
end;

constructor TCompiler.Create;
begin
  FUnits := TObjectList<TPascalUnit>.Create();
  FSearchPath := TStringList.Create();
end;

destructor TCompiler.Destroy;
begin
  FUnits.Free;
  FSearchPath.Free;
  inherited;
end;

procedure TCompiler.DoMessage(AMessage, AUnit: string; ALine: Integer;
  ALevel: TMessageLevel);
begin
  if Assigned(FOnMessage) then
  begin
    FOnMessage(AMessage, AUnit, ALine, ALevel);
  end;
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
  if Assigned(FCurrentProc) then
  begin
    Result := TVarDeclaration(FCurrentProc.GetElement(AName, AType));
    if Assigned(Result) then
    begin
      Exit;
    end;
  end;
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

function TCompiler.GetPathForFile(AFile: string): string;
var
  LPath: string;
begin
  Result := AFile;
  if FileExists(AFile) then
  begin
    Exit;
  end;
  for LPath in FSearchPath do
  begin
    if FileExists(LPath + '\' + AFile) then
    begin
      Result := LPath + '\' + AFile;
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

procedure TCompiler.ParseASMBlock(AScope: TObjectList<TCodeElement>);
var
  LToken: TToken;
  LBlock: TASMBlock;
begin
  LBlock := TASMBlock.Create('');
  AScope.Add(LBlock);
  FLexer.GetToken('asm');
  while not (FLexer.PeekToken.IsContent('end') and FLexer.AHeadToken.IsContent(';')) do
  begin
    LToken := FLexer.GetToken();
    LBlock.Source := LBlock.Source + LToken.Content;
    if LToken.IsType(ttIdentifier) then
    begin
      LBlock.Source := LBlock.Source + ' ';
    end;
    if LToken.FollowedByNewLine then
    begin
      LBlock.Source := LBlock.Source + sLineBreak;
    end;
  end;
  FLexer.GetToken('end');
  FLexer.GetToken(';');
end;

procedure TCompiler.ParseAssignment(AScope: TObjectList<TCodeElement>);
var
  LAssignment: TAssignment;
begin
  LAssignment := TAssignment.Create('');
  LAssignment.TargetVar := GetVar(FLexer.GetToken('', ttIdentifier).Content);
  AScope.Add(LAssignment);
  if FLexer.PeekToken.IsContent('^') then
  begin
    LAssignment.Dereference := True;
    FLexer.GetToken();
  end;
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
  LInverse: Boolean;
  LTempID, LContent, LData: string;
begin
  LInverse := False;
  while FLexer.PeekToken.IsContent('not') do
  begin
    FLexer.GetToken('not');
    LInverse := not LInverse;
  end;
  if FLexer.PeekToken.IsContent('(') then
  begin
    FLexer.GetToken('(');
    ParseRelation(AScope, LInverse);
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
      LFactor.Inverse := LInverse;
      AScope.Add(LFactor);
      if FLexer.PeekToken.IsType(ttNumber) then
      begin
        LFactor.Value := FLexer.GetToken().Content;
      end
      else
      begin
        if FLexer.PeekToken.IsType(ttCharLiteral) then
        begin
          LTempID := FCurrentUnit.GetUniqueID('str');
          LFactor.Value := LTempID;
          LContent := FLexer.GetToken().Content;
          LData := ':' + LTempID + ' dat "' + LContent + '"';
          if Length(LContent) > 1 then
          begin
            LData := LData + ', 0x0';
          end;
          FCurrentUnit.FooterSource.Add(LData);
        end
        else
        begin
          if FLexer.PeekToken.IsContent('@') then
          begin
            FLexer.GetToken('@');
            LFactor.GetAdress := True;
          end;
          LFactor.VarDeclaration := GetVar(FLexer.GetToken('', ttIdentifier).Content);
          if LFactor.GetAdress and LFactor.VarDeclaration.IsParameter then
          begin
            Fatal('Cannot receive adress of Paremeter ' + QuotedStr(LFactor.VarDeclaration.Name));
          end;
        end;
      end;
      if FLexer.PeekToken.IsContent('^') then
      begin
        FLexer.GetToken();
        LFactor.Dereference := True;
        if LFactor.GetAdress then
        begin
          Fatal('Cannot get the adress of ' + QuotedStr(LFactor.VarDeclaration.Name) + ' and dereference it at the same time');
        end;
      end;
    end;
  end;
end;

procedure TCompiler.ParseRelation(AScope: TObjectList<TCodeElement>; AInverse: Boolean = False);
var
  LRelation: TRelation;
begin
  LRelation := TRelation.Create();
  LRelation.Inverse := AInverse;
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
      ttIdentifier, ttReserved:
      begin
        case AnsiIndexText(FLexer.PeekToken.Content, ['if', 'while', 'repeat', 'asm']) of
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
          3:
          begin
            ParseASMBlock(AScope);
          end

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
  LIsFunction: Boolean;
begin
  if FLexer.PeekToken.IsContent('function') then
  begin
    LIsFunction := True;
    FLexer.GetToken('function');
  end
  else
  begin
    LIsFunction := False;
    FLexer.GetToken('procedure');
  end;
  LProc := TProcDeclaration.Create(FLexer.GetToken('', ttIdentifier).Content);
  FCurrentProc := LProc;
  AScope.Add(LProc);
  ParseRoutineParameters(LProc.Parameters);
  if LIsFunction then
  begin
    FLexer.GetToken(':');
    LProc.ResultType := GetDataType(FLexer.GetToken('', ttIdentifier).Content);
    LProc.AddResultValue();
  end;
  FLexer.GetToken(';');
  if FLexer.PeekToken.IsContent('var') then
  begin
    ParseRoutineLocals(LProc);
  end;
  if FLexer.PeekToken.IsContent('asm') then
  begin
    ParseASMBlock(LProc.SubElements);
  end
  else
  begin
    FLexer.GetToken('begin');
    ParseRoutineContent(LProc.SubElements);
    FLexer.GetToken('end');
    FLexer.GetToken(';');
  end;
  FCurrentProc := nil;
end;

procedure TCompiler.ParseRoutineLocals(AProc: TProcDeclaration);
var
  LScope: TObjectList<TCodeElement>;
  LElement: TCodeElement;
begin
  LScope := TObjectList<TCodeElement>.Create(False);
  FLexer.GetToken('var');
  while not FLexer.PeekToken.IsType(ttReserved) do
  begin
    ParseVarDeclaration(LScope, True, False, True);
  end;
  for LElement in LScope do
  begin
    AProc.AddLocal(TVarDeclaration(LElement));
  end;
  LScope.Free;
end;

procedure TCompiler.ParseRoutineParameters(AScope: TObjectList<TCodeElement>);
begin
  FLexer.GetToken('(');
  while not FLexer.PeekToken.IsContent(')') do
  begin
    ParseVarDeclaration(AScope, False, True);
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
  AUnit.Name := FLexer.GetToken('', ttIdentifier).Content;
  FLexer.GetToken.MatchContent(';');
end;

procedure TCompiler.ParseUses;
begin
  FLexer.GetToken('uses');
  CompileFile(FLexer.GetToken('', ttIdentifier).Content + '.pas');
  while not FLexer.PeekToken.IsContent(';') do
  begin
    FLexer.GetToken(',');
    CompileFile(FLexer.GetToken('', ttIdentifier).Content + '.pas');
  end;
  FLexer.GetToken(';');
end;

procedure TCompiler.ParseVarDeclaration;
var
  LNames: TStringList;
  LName, LDef: string;
  LRepeat: Boolean;
  LType: TDataType;
  LIndex: Integer;
  LVarDec: TVarDeclaration;
begin
  LNames := TStringList.Create();
  LRepeat := False;
  LDef := '0x0';
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
  if AIncludeEndMark and (not (AAsParameter or AAsLocal)) and (FLexer.PeekToken.IsContent('=')) then
  begin
    FLexer.GetToken();
    LDef := FLexer.GetToken('', ttNumber).Content;
  end;
  if AIncludeEndMark then
  begin
    FLexer.GetToken(';');
  end;
  if AAsParameter then
  begin
    LIndex := 1;
  end;
  if AAsLocal then
  begin
    LIndex := -1;
  end;
  for LName in LNames do
  begin
    LVarDec := TVarDeclaration.Create(LName, LType);
    LVarDec.DefaultValue := LDef;
    if AAsParameter or AAsLocal then
    begin
      LVarDec.ParamIndex := LIndex;
      if AAsParameter then
      begin
        Inc(LIndex);
      end
      else
      begin
        if AAsLocal then
        begin
          Dec(LIndex);
        end;
      end;
    end;
    AScope.Add(LVarDec);
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
