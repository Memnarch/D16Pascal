unit D16Parser;

interface

uses
  Classes, Types, Generics.Collections, SysUtils, UncountedInterfacedObject, Lexer, Token, CodeElement, DataType,
  VarDeclaration, PascalUnit, ProcDeclaration, ASMBlock, Operation, Operations,
  Factor, CompilerDefines, LineMapping, UnitCache, WriterIntf;

type
  TSectionType = (tsInterface, tsImplementation);

  TD16Parser = class(TUncountedInterfacedObject, IOperations)
  private
    FLexer: TLexer;
    FUnits: TUnitCache;
    FOperations: TObjectList<TOperation>;
    FCurrentUnit: TPascalUnit;
    FCurrentProc: TProcDeclaration;
    FSearchPath: TStringlist;
    FOnMessage: TOnMessage;
    FWarning: Integer;
    FErrors: Integer;
    FFatals: Integer;
    FHints: Integer;
    FPeekMode: Boolean;
    FNilDataType: TDataType;
    FCurrentSectionType: TSectionType;
    procedure ParsePascalUnit(AUnit: TPascalUnit; ACatchException: Boolean = False;
      AParseAsProgramm: Boolean = False);
    procedure RegisterType(AName: string; ASize: Integer = 2; APrimitive: TRawType = rtUInteger;
      ABaseType: TDataType = nil);
    procedure RegisterOperation(AOpName: string; ALeftType, ARightType: TRawType; ALeftSize, ARightSize: Integer;
      AResultType:TDataType; AAssembler: string);
    procedure RegisterBasicTypes();
    procedure RegisterOperations();
    function GetElement(AName: string; AType: TCodeElementClass): TCodeElement;
    function ExpectElement(AName: string; AType: TCodeElementClass): TCodeElement;
    function GetDataType(AName: string): TDataType;
    function GetVar(AName: string): TVarDeclaration;
    function GetOperation(AOperation: string; ALeftType,
      ARightType: TDataType): TOperation;
    //parse functions
    procedure ParseUnit();
    procedure ParseUnitHeader();
    procedure ParseUnitFooter();
    procedure ParseUnitSectionContent(AScope: TObjectList<TCodeElement>);
    procedure ParseProgram();
    procedure ParseProgramHeader();
    procedure ParseUses(AScope: TObjectList<TCodeElement>);
    procedure ParseTypes(AScope: TObjectList<TCodeElement>);
    procedure ParseTypeDeclaration(AScope: TObjectList<TCodeElement>);
    procedure ParseVars(AScope: TObjectList<TCodeElement>);
    procedure ParseVarDeclaration(AScope: TObjectList<TCodeElement>; AIncludeEndMark: Boolean = True;
      AAsParameter: Boolean = False; AAsLocal: Boolean = False; AAsConst: Boolean = False; AIndexOffset: Integer = 0);
    procedure ParseConsts(AScope: TObjectList<TCodeElement>);
    procedure ParseRoutineDeclaration(AScope: TObjectList<TCodeElement>);
    procedure ParseRoutineParameters(AScope: TObjectList<TCodeElement>);
    procedure ParseRoutineLocals(AProc: TProcDeclaration);
    procedure ParseRoutineContent(AScope: TObjectList<TCodeElement>);
    function ParseRoutineCall(AScope: TObjectList<TCodeElement>; AIncludeEndMark: Boolean = True): TDataType;
    function ParseAssignment(AScope: TObjectList<TCodeElement>; AIncludeEndmark: Boolean = True): TDataType;
    procedure ParseCondition(AScope: TObjectList<TCodeElement>);
    procedure ParseWhileLoop(AScope: TObjectList<TCodeElement>);
    procedure ParseRepeatLoop(AScope: TObjectList<TCodeElement>);
    procedure ParseForLoop(AScope: TObjectList<TCodeElement>);
    procedure ParseCaseStatement(AScope: TObjectList<TCodeElement>);
    procedure ParseCase(AScope: TObjectList<TCodeElement>);
    function ParseRelation(AScope: TObjectList<TCodeElement>; ATryInverse: Boolean = False): TDataType;
    function ParseExpression(AScope: TObjectList<TCodeElement>): TDataType;
    function ParseTerm(AScope: TObjectList<TCodeElement>): TDataType;
    function ParseFactor(AScope: TObjectList<TCodeElement>): TDataType;
    function ParseConstantFactor(AFactor: TFactor): TDataType;
    function ParseArrayModifiers(AType: TDataType; AScope: TObjectList<TCodeElement>;
      AMaxList: TList<Integer>):TDataType;
    function ParsePostExpression(AType: TDataType; AScope: TObjectList<TCodeElement>;
      AMaxList: TList<Integer>):TDataType;
    procedure ParseASMBlock(AScope: TObjectList<TCodeElement>);
    procedure Fatal(AMessage: string);
    procedure Error(AMessage: string);
    procedure DoMessage(AMessage, AUnit: string; ALine: Integer; ALevel: TMessageLevel);
    function GetPathForFile(AFile: string): string;
    function GetCurrentLine(): Integer;
    procedure RegisterSysUnit();
    procedure Initialize();
  public
    constructor Create(AUnitCache: TUnitCache);
    destructor Destroy(); override;
    procedure ParseFile(AFile: string);
    procedure ParseSource(ASource: string; AAsProgram: Boolean);
    procedure Reset();
    procedure ResetResults();
    property SearchPath: TStringlist read FSearchPath write FSearchPath;
    property OnMessage: TOnMessage read FOnMessage write FOnMessage;
    property Errors: Integer read FErrors;
    property Warnings: Integer read FWarning;
    property Fatals: Integer read FFatals;
    property Hints: Integer read FHints;
    property PeekMode: Boolean read FPeekMode write FPeekMode;
    property Units: TUnitCache read FUnits;
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
  Math, StrUtils, Relation, Expression, Term, Assignment, Condition, Loops, ProcCall, CaseState, Optimizer;

const
  CSysUnit = 'System';

{ TCompiler }

procedure TD16Parser.ParseFile(AFile: string);
var
  LUnit: TPascalUnit;
begin
  DoMessage('Compiling ' + QuotedStr(AFile), '', 0, mlNone);
  LUnit := TPascalUnit.Create('');
  try
    if not FileExists(GetPathForFile(AFile)) then
    begin
      raise EAbort.Create('File not found:' + QuotedStr(AFile));
    end;
    LUnit.Lexer.LoadFromFile(GetPathForFile(AFile));
    ParsePascalUnit(LUnit, false, SameText(ExtractFileExt(AFile), '.d16r'));
  except
    on E: Exception do
    begin
      DoMessage(E.Message, ExtractFileName(AFile), 0, mlFatal);
      Inc(FFatals);
    end;
  end;
end;

procedure TD16Parser.ParseSource(ASource: string; AAsProgram: Boolean);
var
  LUnit: TPascalUnit;
begin
  LUnit := TPascalUnit.Create('');
  try
    LUnit.Lexer.LoadFromString(ASource);
    ParsePascalUnit(LUnit, False, AAsProgram);
  except
    on E: Exception do
    begin
      DoMessage(E.Message, '', 0, mlFatal);
      Inc(FFatals);
    end;
  end;
end;

procedure TD16Parser.ParsePascalUnit;
var
  LLastUnit: TPascalUnit;
  LLastLexer: TLexer;
begin
  LLastUnit := FCurrentUnit;
  LLastLexer := FLexer;
  FUnits.Add(AUnit);
  FCurrentUnit := AUnit;
  FCurrentUnit.UsedUnits.Add(CSysUnit);
  FLexer := AUnit.Lexer;
  try
    try
      if not AParseAsProgramm then
      begin
        ParseUnit();
      end
      else
      begin
        ParseProgram();
      end;
    except
      on E: Exception do
      begin
        DoMessage(E.Message, AUnit.Name, AUnit.Lexer.PeekToken.FoundInLine, mlFatal);
        if AUnit.Name = '' then
        begin
          FUnits.Delete(FUnits.IndexOf(AUnit));
        end;
      end;
    end;
  finally
    FCurrentUnit := LLastUnit;
    FLexer := LLastLexer;
  end;
end;

function TD16Parser.ParsePostExpression(AType: TDataType;
  AScope: TObjectList<TCodeElement>; AMaxList: TList<Integer>): TDataType;
begin
  Result := AType;
  if FLexer.PeekToken.IsContent('[') then
  begin
    if (Result.RawType = rtPointer) and (Result.BaseType.RawType = rtArray) then
    begin
      Result := Result.BaseType;
    end;
    if (Result.RawType = rtArray) or (Result.RawType = rtString) then
    begin
      Result := ParseArrayModifiers(Result, AScope, AMaxList);
    end;
  end;
end;

constructor TD16Parser.Create;
begin
  inherited Create();
  FUnits := AUnitCache;
  FOperations := TObjectList<TOperation>.Create();
  FSearchPath := TStringList.Create();
  FPeekMode := False;
  FNilDataType := TDataType.Create('NilType', 0, rtNilType);
  Initialize();
end;

destructor TD16Parser.Destroy;
begin
  FSearchPath.Free;
  FOperations.Free;
  inherited;
end;

procedure TD16Parser.DoMessage(AMessage, AUnit: string; ALine: Integer;
  ALevel: TMessageLevel);
begin
  case ALevel of
    mlError:
    begin
      Inc(FErrors);
    end;

    mlFatal:
    begin
      Inc(FFatals);
    end;
  end;
  if Assigned(FOnMessage) then
  begin
    FOnMessage(AMessage, AUnit, ALine, ALevel);
  end;
end;

procedure TD16Parser.Error(AMessage: string);
begin
  DoMessage(AMessage, FCurrentUnit.Name, FLexer.PeekToken.FoundInLine, mlError);
end;

function TD16Parser.ExpectElement(AName: string;
  AType: TCodeElementClass): TCodeElement;
begin
  Result := GetElement(AName, AType);
  if not Assigned(Result) then
  begin
    Fatal('Undeclared identifier ' + QuotedStr(AName));
  end;
end;

procedure TD16Parser.Fatal(AMessage: string);
begin
  raise EAbort.Create(AMessage);
end;

function TD16Parser.GetCurrentLine: Integer;
begin
  Result := FLexer.PeekToken.FoundInLine;
end;

function TD16Parser.GetDataType(AName: string): TDataType;
begin
  Result := TDataType(GetElement(AName, TDataType));
  if not Assigned(Result) then
  begin
    Fatal(QuotedStr(AName) + ' is not a declared datatype');
  end;
end;

function TD16Parser.GetElement(AName: string;
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
  Result := FCurrentUnit.GetElementFromAll(AName, AType);
  if Assigned(Result) then Exit;

  for LUnit in FUnits do
  begin
    if (LUnit <> FCurrentUnit) and (FCurrentUnit.UsedUnits.IndexOf(LUnit.Name) < 0) then Continue;

    LElement := LUnit.GetElement(AName, AType);
    if Assigned(LElement) then
    begin
      Result := LElement;
      Break;
    end;
  end;
end;

function TD16Parser.GetOperation(AOperation: string; ALeftType,
  ARightType: TDataType): TOperation;
var
  LOperation: TOperation;
begin
  Result := nil;
  for LOperation in FOperations do
  begin
    if SameText(LOperation.OpName, AOperation) and (LOperation.LeftRawType = ALeftType.RawType) and (LOperation.RightRawType = ARightType.RawType)
      and (LOperation.LeftSize = ALeftType.Size) and (LOperation.RightSize = ARightType.Size) then
    begin
      Result := LOperation;
      Break;
    end;
  end;
  if not Assigned(Result) then
  begin
    Fatal('Operator ' + QuotedStr(AOperation) + ' not applicable to ' +
      QuotedStr(ALeftType.Name) + ' and ' + QuotedStr(ARightType.Name));
  end;
end;

function TD16Parser.GetPathForFile(AFile: string): string;
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

function TD16Parser.GetVar(AName: string): TVarDeclaration;
begin
  Result := TVarDeclaration(GetElement(AName, TVarDeclaration));
  if not Assigned(Result) then
  begin
    Fatal(QuotedStr(AName) + ' is not a declared Var');
  end;
end;

procedure TD16Parser.Initialize;
begin
  FErrors := 0;
  FFatals := 0;
  FWarning := 0;
  FHints := 0;
  RegisterSysUnit();
end;

function TD16Parser.ParseArrayModifiers(AType: TDataType;
  AScope: TObjectList<TCodeElement>; AMaxList: TList<Integer>): TDataType;
var
  i: Integer;
  LResult: TDataType;
begin
  Result := AType;
  while (Result.RawType = rtArray) or (Result.RawType = rtString) do
  begin
    for i := 0 to Result.Dimensions.Count - 1 do
    begin
      FLexer.GetToken('[');
      LResult := ParseRelation(AScope, false);
      AMaxList.Add(Result.Dimensions.Items[i]);
      if LResult.RawType <> rtUInteger then
      begin
        Fatal('The result of a relation in an array modifier must be an unsigned integer');
      end;
      FLexer.GetToken(']');
    end;
    Result := Result.BaseType;
  end;
end;

procedure TD16Parser.ParseASMBlock(AScope: TObjectList<TCodeElement>);
var
  LToken: TToken;
  LBlock: TASMBlock;
  LContent, LLine: string;
  LVar: TVarDeclaration;
  LLineNumber: Integer;
  i: Integer;
begin
  LBlock := TASMBlock.Create('');
  LLine := '';
  LLineNumber := -1;
  AScope.Add(LBlock);
//  LBlock.Line := FLexer.PeekToken.FoundInLine;
  FLexer.GetToken('asm');
  while not ((FLexer.PeekToken.IsContent('end') and (not FLexer.PeekToken.FollowedByNewLine) and FLexer.AHeadToken.IsContent(';'))) do
  begin
    if FLexer.PeekToken.IsContent(';') then
    begin
      while not FLexer.EOF do
      begin
        LToken := FLexer.GetToken();
        if LToken.FollowedByNewLine then
        begin
          Break;
        end;
      end;
      if FLexer.EOF then
      begin
        Break;
      end;
      Continue;
    end;
    if LBlock.Line = -1 then
    begin
      LBlock.Line := FLexer.PeekToken.FoundInLine-1;
    end;
    if LLineNumber = -1 then
    begin
      LLineNumber := FLexer.PeekToken.FoundInLine - LBlock.Line;
    end;
    LToken := FLexer.GetToken();
    LContent := LToken.Content;
    if LToken.IsType(ttIdentifier) and (AnsiIndexText(LContent, ['a', 'b', 'c', 'x', 'y', 'z', 'i', 'j']) < 0) then
    begin
      LVar := TVarDeclaration(GetElement(LContent, TVarDeclaration));
      if Assigned(LVar) then
      begin
        LContent := LVar.GetAccessIdentifier();
        if ((LVar.ParamIndex < 1) or (LVar.ParamIndex > 3)) and (LVar.IsParameter or LVar.IsLocal)
        and (not FLexer.PeekToken.IsContent(']')) then
        begin
          LContent := '[' + LContent + ']';
        end;
      end;
    end
    else
    begin
      if LToken.IsType(ttCharLiteral) then
      begin
        LContent := '"' + LContent + '"';
      end;
    end;
    LLine := LLine + LContent + ' ';
    if LToken.FollowedByNewLine or ((not FLexer.EOF) and FLexer.PeekToken.IsContent(';')) then
    begin
      if LBlock.Source.Count > 0 then
      begin
        for i := 1 to LLineNumber - (LBlock.Source.Count + 1) do
        begin
          LBlock.Source.Add('');
        end;
      end;
      LBlock.Source.Add(LLine);
      LLine := '';
      LLineNumber := -1;
    end;
  end;
  if LLine <> '' then
  begin
    for i := 1 to LLineNumber - (LBlock.Source.Count + 1) do
    begin
      LBlock.Source.Add('');
    end;
    LBlock.Source.Add(LLine);
  end;
  FLexer.GetToken('end');
  FLexer.GetToken(';');
end;

function TD16Parser.ParseAssignment(AScope: TObjectList<TCodeElement>; AIncludeEndmark: Boolean = True): TDataType;
var
  LAssignment: TAssignment;
  LRelType: TDataType;
  LOverride: TDataType;
begin
  LOverride := nil;
  LAssignment := TAssignment.Create();
  LAssignment.Line := FLexer.PeekToken.FoundInLine;
  if FLexer.PeekToken.IsType(ttIdentifier) and FLexer.AHeadToken.IsContent('(') then
  begin
    LOverride := GetDataType(FLexer.GetToken('', ttIdentifier).Content);
    FLexer.GetToken('(');
  end;
  LAssignment.TargetVar := GetVar(FLexer.GetToken('', ttIdentifier).Content);
  if LAssignment.TargetVar.IsConst then
  begin
    Fatal('Cannot assign to a const value');
  end;
  AScope.Add(LAssignment);
  if FLexer.PeekToken.IsContent('^') then
  begin
    LAssignment.Dereference := True;
    FLexer.GetToken();
  end;
  Result := LAssignment.TargetVar.DataType;
  if Assigned(LOverride) then
  begin
    FLexer.GetToken(')');
    Result := LOverride;
  end;
  if LAssignment.Dereference then
  begin
    Result := Result.BaseType;
  end;
  Result := ParsePostExpression(Result, LAssignment.Modifiers, LAssignment.ModifierMax);
  LAssignment.TargetType := Result;
  FLexer.GetToken(':=');
  LRelType := ParseRelation(LAssignment.SubElements);
  LAssignment.SourceType := LRelType;
  if AIncludeEndmark then
  begin
    FLexer.GetToken(';');
  end;
  if (LRelType.RawType <> Result.RawType) or ((Result.RawType = rtArray) and (not SameText(Result.Name, LRelType.Name))) then
  begin
    Fatal('Cannot assign ' + QuotedStr(LRelType.Name) + ' to ' + QuotedStr(Result.Name));
  end;
end;

procedure TD16Parser.ParseCase(AScope: TObjectList<TCodeElement>);
var
  LCase: TCase;
  LFactor: TFactor;
  LRepeat: Boolean;
begin
  LCase := TCase.Create();
  AScope.Add(LCase);
  LRepeat := False;
  while (not FLexer.PeekToken.IsContent(':')) or LRepeat do
  begin
    LRepeat := False;
    if ParseFactor(LCase.ConstValues).RawType <> rtUInteger then
    begin
      Fatal('value must be of type unsigned Integer');
    end;
    LFactor := TFactor(LCase.ConstValues.Items[LCase.ConstValues.Count-1]);
    if (not LFactor.IsConstant) and ((Assigned(LFactor.VarDeclaration) and (not LFactor.VarDeclaration.IsConst))) then
    begin
      Fatal('Value must be of type const');
    end;
    if (LFactor.SubElements.Count > 0) or LFactor.Inverse or LFactor.GetAdress or LFactor.Dereference then
    begin
      Fatal('illegal statement');
    end;
    if FLexer.PeekToken.IsContent(',') then
    begin
      FLexer.GetToken(',');
      LRepeat := True;
    end;
  end;
  FLexer.GetToken(':');
  FLexer.GetToken('begin');
  ParseRoutineContent(LCase.SubElements);
  FLexer.GetToken('end');
  FLexer.GetToken(';');
end;

procedure TD16Parser.ParseCaseStatement(AScope: TObjectList<TCodeElement>);
var
  LStatement: TCaseStatement;
begin
  LStatement := TCaseStatement.Create();
  AScope.Add(LStatement);
  LStatement.Line := FLexer.PeekToken.FoundInLine;
  FLexer.GetToken('case');
  if ParseRelation(LStatement.Relation).RawType <> rtUInteger then
  begin
    Fatal('Relation of Case requires returntype of unsigned integer');
  end;
  FLexer.GetToken('of');
  while (not FLexer.PeekToken.IsContent('end')) and (not FLexer.PeekToken.IsContent('else')) do
  begin
    ParseCase(LStatement.Cases);
  end;
  if FLexer.PeekToken.IsContent('else') then
  begin
    FLexer.GetToken('else');
    FLexer.GetToken('begin');
    ParseRoutineContent(LStatement.ElseCase);
    FLexer.GetToken('end');
    FLexer.GetToken(';');
  end;
  FLexer.GetToken('end');
  FLexer.GetToken(';');
end;

procedure TD16Parser.ParseCondition(AScope: TObjectList<TCodeElement>);
var
  LCondition: TCondition;
begin
  LCondition := TCondition.Create();
  AScope.Add(LCondition);
  LCondition.Line := FLexer.PeekToken.FoundInLine;
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

function TD16Parser.ParseConstantFactor(AFactor: TFactor): TDataType;
var
  LTempID, LContent, LString, LData: string;
begin
  if FLexer.PeekToken.IsType(ttCharLiteral) then
  begin
    Result := GetDataType('string');
    LTempID := FCurrentUnit.GetUniqueID('str');
    AFactor.Value := LTempID;
    LContent := FLexer.GetToken().Content;
    LString := StringReplace(LContent,'\n', '",10,"',[]);
    LData := ':' + LTempID + ' dat ' + IntToStr(Length(LString)) + ', ' + '"' + LString + '"';
    FCurrentUnit.FooterSource.Add(LData);
  end
  else
  begin
    AFactor.Value := FLexer.GetToken().Content;
    Result := GetDataType('word');
  end;
end;

procedure TD16Parser.ParseConsts;
begin
  FLexer.GetToken('const');
  while not FLexer.PeekToken.IsType(ttReserved) do
  begin
    ParseVarDeclaration(AScope, True, False, False, True);
  end;
end;

function TD16Parser.ParseExpression(AScope: TObjectList<TCodeElement>): TDataType;
var
  LExpression: TExpression;
  LLastResult: TDataType;
  LOperation: TOperation;
  LOperator: string;
begin
  LExpression := TExpression.Create();
  AScope.Add(LExpression);
  Result := ParseTerm(LExpression.SubElements);
  while FLexer.PeekToken.IsType(ttTermOp) do
  begin
    LOperator := FLexer.GetToken().Content;
    //LExpression.Operators.Add(LOperation);
    LLastResult := Result;
    Result := ParseTerm(LExpression.SubElements);
    LOperation := GetOperation(LOperator, LLastResult, Result);
    Result := LOperation.ResultType;
    LExpression.Operations.Add(LOperation);
  end;
end;

function TD16Parser.ParseFactor(AScope: TObjectList<TCodeElement>): TDataType;
var
  LFactor: TFactor;
  LInverse: Boolean;
  LOverride: TDataType;
begin
  LInverse := False;
  LOverride := nil;
  LFactor := TFactor.Create();
  AScope.Add(LFactor);
  while FLexer.PeekToken.IsContent('not') do
  begin
    FLexer.GetToken('not');
    LInverse := not LInverse;
  end;
  if FLexer.PeekToken.IsType(ttIdentifier)
    and FLexer.AHeadToken.IsContent('(')
    and not Assigned(GetElement(FLexer.PeekToken.Content, TProcDeclaration))
  then
  begin
    LOverride := GetDataType(FLexer.GetToken('', ttIdentifier).Content);
  end;
  if FLexer.PeekToken.IsContent('(') then
  begin
    FLexer.GetToken('(');
    Result := ParseRelation(LFactor.SubElements, LInverse);
    if Result.RawType = rtBoolean then
    begin
      LInverse := False; //in case it was true when parsing the relation, the value of the relation is already inversed
    end;
    if Assigned(LOverride) then
    begin
      Result := LOverride;
    end;
    FLexer.GetToken(')');
  end
  else
  begin
    if Assigned(GetElement(FLexer.PeekToken.Content, TProcDeclaration)) then
    begin
      Result := ParseRoutineCall(LFactor.SubElements, False);
    end
    else
    begin
      if FLexer.PeekToken.IsType(ttNumber) or FLexer.PeekToken.IsType(ttCharLiteral) then
      begin
        Result := ParseConstantFactor(LFactor);
      end
      else
      begin
        if FLexer.PeekToken.IsContent('@') then
        begin
          FLexer.GetToken('@');
          LFactor.GetAdress := True;
        end;
        LFactor.VarDeclaration := GetVar(FLexer.GetToken('', ttIdentifier).Content);
        Result := LFactor.VarDeclaration.DataType;
        if LFactor.GetAdress and LFactor.VarDeclaration.IsParameter then
        begin
          Fatal('Cannot receive adress of Paremeter ' + QuotedStr(LFactor.VarDeclaration.Name));
        end;
      end;
    end;
  end;
  LFactor.Inverse := LInverse;
  if FLexer.PeekToken.IsContent('^') then
  begin
    FLexer.GetToken();
    LFactor.Dereference := True;
    if LFactor.GetAdress then
    begin
      Fatal('Cannot get the adress of ' + QuotedStr(LFactor.VarDeclaration.Name) + ' and dereference it at the same time');
    end;
  end;
  if LFactor.Dereference then
  begin
    Result := Result.BaseType;
  end;
  Result := ParsePostExpression(Result, LFactor.Modifiers, LFactor.ModifierMax);
  if LFactor.GetAdress then
  begin
    Result := GetDataType('pointer');
  end;
end;

procedure TD16Parser.ParseForLoop(AScope: TObjectList<TCodeElement>);
var
  LFor: TForLoop;
begin
  LFor := TForLoop.Create();
  AScope.Add(LFor);
  LFor.Line := FLexer.PeekToken.FoundInLine;
  FLexer.GetToken('for');
  if ParseAssignment(LFor.Assignment, False).RawType <> rtUInteger then
  begin
    Fatal('assignment must be of type unsigned integer');
  end;
  FLexer.GetToken('to');
  if ParseRelation(LFor.Relation).RawType <> rtUInteger then
  begin
    Fatal('Result of relation must be of type unsigned integer');
  end;
  FLexer.GetToken('do');
  FLexer.GetToken('begin');
  ParseRoutineContent(LFor.SubElements);
  FLexer.GetToken('end');
  FLexer.GetToken(';');
end;

procedure TD16Parser.ParseProgram;
begin
  FCurrentSectionType := tsImplementation;
  ParseProgramHeader();
  if Units.CountUnitName(FCurrentUnit.Name) > 1 then
  begin
    FUnits.Delete(FUnits.IndexOf(FCurrentUnit));
    Exit();
  end;
  ParseUnitSectionContent(FCurrentUnit.ImplementationSection);
  FLexer.GetToken('begin');
  ParseRoutineContent(FCurrentUnit.InitSection);
  ParseUnitFooter();
end;

procedure TD16Parser.ParseProgramHeader;
begin
  FLexer.GetToken('program', ttReserved);
  FCurrentUnit.Name := FLexer.GetToken('', ttIdentifier).Content;
  FLexer.GetToken.MatchContent(';');
end;

function TD16Parser.ParseRelation(AScope: TObjectList<TCodeElement>; ATryInverse: Boolean = False): TDataType;
var
  LRelation: TRelation;
  LLastResult: TDataType;
  LOperator: string;
  LOperation: TOperation;
begin
  //tryinverse indicates, that wem may already inverse the result if we can
  //we can inverse the result, when the result is of type bool
  //if its not bool, it might be, that the result is going to be dereferenced too
  //in this case we dont inverse here, which is then handled by the top layer(parsefactor)
  //this is required to not mix up the order.
  //first dereference THEN inverse it. Always inversing at this position might screw this
  //as we dereference AFTER inversing if this is not a bool
  LRelation := TRelation.Create();
  AScope.Add(LRelation);
  Result := ParseExpression(LRelation.SubElements);
  if FLexer.PeekToken.IsType(ttRelOp) then
  begin
    LOperator := FLexer.GetToken().Content;
    LLastResult := Result;
    Result := ParseExpression(LRelation.SubElements);
    LOperation := GetOperation(LOperator, LLastResult, Result);
    LRelation.Operations.Add(LOperation);
    Result := LOperation.ResultType;
    LRelation.Inverse := ATryInverse;//we may inverse here already, since since its not possible to dereference a bool value
  end;
end;

procedure TD16Parser.ParseRepeatLoop(AScope: TObjectList<TCodeElement>);
var
  LLoop: TRepeatLoop;
begin
  LLoop := TRepeatLoop.Create();
  AScope.Add(LLoop);
  LLoop.Line := FLexer.PeekToken.FoundInLine;
  FLexer.GetToken('repeat');
  ParseRoutineContent(LLoop.SubElements);
  FLexer.GetToken('until');
  ParseRelation(LLoop.Relation);
  FLexer.GetToken(';');
end;

function TD16Parser.ParseRoutineCall(AScope: TObjectList<TCodeElement>; AIncludeEndMark: Boolean = True): TDataType;
var
  LCall: TProcCall;
  i: Integer;
  LParamType: TDataType;
begin
  Result := FNilDataType;
  LCall := TProcCall.Create();
  AScope.Add(LCall);
  LCall.Line := FLexer.PeekToken.FoundInLine;
  LCall.ProcDeclaration := TProcDeclaration(ExpectElement(FLexer.GetToken().Content, TProcDeclaration));
  if LCall.ProcDeclaration.IsFunction then
  begin
    Result := LCall.ProcDeclaration.ResultType;
    LCall.IgnoreResult := AIncludeEndMark;//if we are not in an expression, returnvalue must be ignored or we screw the stack
  end
  else
  begin
    if not AIncludeEndMark then //we are inside an expression, no procedure allowed
    begin
      Fatal('Procedure not allowed inside an expression');
    end;
  end;
  FLexer.GetToken('(');
  for i := 0 to LCall.ProcDeclaration.Parameters.Count - 1 do
  begin
    LParamType := ParseRelation(LCall.Parameters);
    if  LParamType.RawType <> TVarDeclaration(LCall.ProcDeclaration.Parameters.Items[i]).DataType.RawType then
    begin
      Fatal('Cannot assign ' + QuotedStr(LParamType.Name) +
        ' to ' + QuotedStr(TVarDeclaration(LCall.ProcDeclaration.Parameters.Items[i]).Name));
    end;

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

procedure TD16Parser.ParseRoutineContent(AScope: TObjectList<TCodeElement>);
var
  LElement: TCodeElement;
begin
  while not FLexer.PeekToken.IsContent('end') do
  begin
    case FLexer.PeekToken.TokenType of
      ttIdentifier, ttReserved:
      begin
        case AnsiIndexText(FLexer.PeekToken.Content, ['if', 'while', 'repeat', 'asm', 'for', 'case']) of
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
          end;

          4:
          begin
            ParseForLoop(AScope);
          end;

          5:
          begin
            ParseCaseStatement(AScope);
          end

          else
            LElement := ExpectElement(FLexer.PeekToken.Content, TCodeElement);
            if LElement is TProcDeclaration then
            begin
              ParseRoutineCall(AScope);
            end
            else
            begin
              ParseAssignment(AScope);
            end;
        end;
      end;

      else
        Break;
    end;
  end;
end;

procedure TD16Parser.ParseRoutineDeclaration;
var
  LProc, LDummy: TProcDeclaration;
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
  LProc.Line := GetCurrentLine();
  FCurrentProc := LProc;
  AScope.Add(LProc);
  ParseRoutineParameters(LProc.Parameters);
  if LIsFunction then
  begin
    FLexer.GetToken(':');
    LProc.ResultType := GetDataType(FLexer.GetToken('', ttIdentifier).Content);
    if LProc.ResultType.RawType = rtArray then
    begin
      Fatal('You can not use a static array as resulttype');
    end;
  end;
  FLexer.GetToken(';');
  LProc.IsDummy := FCurrentSectionType = tsInterface;
  if LProc.IsDummy then Exit;//in interfacesection, we parse ONLY the header

  LDummy := TProcDeclaration(FCurrentUnit.GetElement(LProc.Name, TProcDeclaration));
  if Assigned(LDummy) then
  begin
    if LDummy.IsDummy then
    begin
      if not LProc.DeclarationMatches(LDummy) then
      begin
        Error('Previous declaration of ' + QuotedStr(LProc.Name) + ' differs from implementation declaration');
      end;
    end
    else
    begin
      if LDummy <> LProc then //oh, we haven't found ourself? its redeclared!
      begin
        Error('Can not redeclare routine ' + QuotedStr(LProc.Name) + ' in same unit');
      end;
    end;
  end;
  if FLexer.PeekToken.IsContent('var') then
  begin
    ParseRoutineLocals(LProc);
  end;
  LProc.StartLine := FLexer.PeekToken.FoundInLine;
  if FLexer.PeekToken.IsContent('asm') then
  begin
    ParseASMBlock(LProc.SubElements);
  end
  else
  begin
    if LIsFunction then
    begin
      LProc.AddResultValue();
    end;
    FLexer.GetToken('begin');
    ParseRoutineContent(LProc.SubElements);
    LProc.EndLine := FLexer.PeekToken.FoundInLine;
    FLexer.GetToken('end');
    FLexer.GetToken(';');
  end;
  FCurrentProc := nil;
end;

procedure TD16Parser.ParseRoutineLocals(AProc: TProcDeclaration);
var
  LScope: TObjectList<TCodeElement>;
  LElement: TCodeElement;
begin
  LScope := TObjectList<TCodeElement>.Create(False);
  FLexer.GetToken('var');
  while not FLexer.PeekToken.IsType(ttReserved) do
  begin
    ParseVarDeclaration(LScope, True, False, True, False, AProc.Parameters.Count + AProc.Locals.Count);
  end;
  for LElement in LScope do
  begin
    AProc.AddLocal(TVarDeclaration(LElement));
  end;
  LScope.Free;
end;

procedure TD16Parser.ParseRoutineParameters(AScope: TObjectList<TCodeElement>);
begin
  FLexer.GetToken('(');
  while not FLexer.PeekToken.IsContent(')') do
  begin
    ParseVarDeclaration(AScope, False, True, False, False, AScope.Count);
    if not FLexer.PeekToken.IsContent(')') then
    begin
      FLexer.GetToken(';');
    end;
  end;
  FLexer.GetToken(')');
end;

function TD16Parser.ParseTerm(AScope: TObjectList<TCodeElement>): TDataType;
var
  LTerm: TTerm;
  LLastResult: TDataType;
  LOperator: string;
  LOperation: TOperation;
begin
  LTerm := TTerm.Create();
  AScope.Add(LTerm);
  Result := ParseFactor(LTerm.SubElements);
  while FLexer.PeekToken.IsType(ttFacOp) do
  begin
    LOperator := FLexer.GetToken().Content;
    //LTerm.Operators.Add(LOperation);
    LLastResult := Result;
    Result := ParseFactor(LTerm.SubElements);
    LOperation := GetOperation(LOperator, LLastResult, Result);
    Result := LOperation.ResultType;
    LTerm.Operations.Add(LOperation);
  end;
end;

procedure TD16Parser.ParseTypeDeclaration(AScope: TObjectList<TCodeElement>);
var
  LType: TDataType;
  LName: string;
  LDimensions: TList<Integer>;
  LDimension: Integer;
begin
  LName := FLexer.GetToken('', ttIdentifier).Content;
  FLexer.GetToken('=');
  if FLexer.PeekToken.IsContent('^') then
  begin
    FLexer.GetToken('^');
    AScope.Add(TDataType.Create(LName, 2, rtPointer, GetDataType(FLexer.GetToken('', ttIdentifier).Content)));
  end;
  if FLexer.PeekToken.IsContent('array') then
  begin
    LDimensions := TList<Integer>.Create();
    while FLexer.PeekToken.IsContent('array') do
    begin
      FLexer.GetToken('array');
      FLexer.GetToken('[');
      LDimensions.Add(StrToInt(FLexer.GetToken('', ttNumber).Content));
      FLexer.GetToken(']');
      FLexer.GetToken('of');
    end;
    LType := TDataType.Create(LName, 2, rtArray, GetDataType(FLexer.GetToken('', ttIdentifier).Content));
    LType.Line := GetCurrentLine();
    for LDimension in LDimensions do
    begin
      LType.Dimensions.Add(LDimension);
    end;
    AScope.Add(LType);
    LDimensions.Free;
  end;
  FLexer.GetToken(';');
end;

procedure TD16Parser.ParseTypes(AScope: TObjectList<TCodeElement>);
begin
  FLexer.GetToken('type');
  while not FLexer.PeekToken.IsType(ttReserved) do
  begin
    ParseTypeDeclaration(AScope);
  end;
end;

procedure TD16Parser.ParseUnit;
var
  LLastSectionType: TSectionType;
begin
  LLastSectionType := FCurrentSectionType;
  ParseUnitHeader();
  if Units.CountUnitName(FCurrentUnit.Name) > 1 then
  begin
    FUnits.Delete(FUnits.IndexOf(FCurrentUnit));
    Exit();
  end;
  FLexer.GetToken('interface',ttReserved);
  FCurrentSectionType := tsInterface;
  ParseUnitSectionContent(FCurrentUnit.InterfaceSection);
  FLexer.GetToken('implementation',ttReserved);
  FCurrentSectionType := tsImplementation;
  ParseUnitSectionContent(FCurrentUnit.ImplementationSection);
  if FLexer.PeekToken.IsContent('initialization') then
  begin
    FLexer.GetToken();
    ParseRoutineContent(FCurrentUnit.InitSection);
  end;
  ParseUnitFooter();
  FCurrentSectionType :=LLastSectionType;
end;

procedure TD16Parser.ParseUnitFooter;
begin
  FLexer.GetToken.MatchContent('end');
  FLexer.GetToken.MatchContent('.');
end;

procedure TD16Parser.ParseUnitHeader;
begin
  FLexer.GetToken.MatchContent('unit');
  FCurrentUnit.Name := FLexer.GetToken('', ttIdentifier).Content;
  FLexer.GetToken.MatchContent(';');
end;

procedure TD16Parser.ParseUnitSectionContent(AScope: TObjectList<TCodeElement>);
begin
  while True do
  begin
    case AnsiIndexText(FLexer.PeekToken.Content, ['uses', 'var', 'const', 'procedure', 'function', 'type']) of
      0:
      begin
        ParseUses(AScope);
      end;

      1:
      begin
        ParseVars(AScope);
      end;

      2:
      begin
        ParseConsts(AScope);
      end;

      3, 4:
      begin
        ParseRoutineDeclaration(AScope);
      end;

      5:
      begin
        ParseTypes(AScope);
      end
      else
        break;
    end;
  end;
end;

procedure TD16Parser.ParseUses;
var
  LName: string;
  LErrors, LFatals: Integer;
begin
  FLexer.GetToken('uses');
  LErrors := FErrors;
  LFatals := FFatals;
  LName := FLexer.GetToken('', ttIdentifier).Content;
  FCurrentUnit.UsedUnits.Add(LName);
  ParseFile(LName + '.pas');
  while not FLexer.PeekToken.IsContent(';') do
  begin
    FLexer.GetToken(',');
    LName := FLexer.GetToken('', ttIdentifier).Content;
    FCurrentUnit.UsedUnits.Add(LName);
    ParseFile(LName + '.pas');
  end;
  if PeekMode then
  begin
    FFatals := LFatals;
    FErrors := LErrors;
  end;
  FLexer.GetToken(';');
end;

procedure TD16Parser.ParseVarDeclaration;
var
  LNames: TStringList;
  LLines: TList<Integer>;
  LName, LDef, LString: string;
  LRepeat: Boolean;
  LType: TDataType;
  i, LIndex: Integer;
  LVarDec: TVarDeclaration;
begin
  LNames := TStringList.Create();
  LLines := TList<Integer>.Create();
  LRepeat := False;
  LDef := '0x0';
  LIndex :=0;
  while (not FLexer.PeekToken.IsContent(':')) or LRepeat do
  begin
    LLines.Add(GetCurrentLine());
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
  if AIncludeEndMark and (not (AAsParameter or AAsLocal)) and (FLexer.PeekToken.IsContent('=') or AAsConst) then
  begin
    FLexer.GetToken();
    if LType.RawType = rtString then
    begin
      LString := StringReplace(FLexer.GetToken('', ttCharLiteral).Content, '\n', '",10,"', [rfReplaceAll, rfIgnoreCase]);
      LDef := IntToStr(Length(LString)) + ', "' + LString + '"';
    end
    else
    begin
      LDef := FLexer.GetToken('', ttNumber).Content;
    end;
  end;
  if AIncludeEndMark then
  begin
    FLexer.GetToken(';');
  end;
  if AAsParameter then
  begin
    LIndex := AIndexOffset + 1;//determining the ParameterIndex in the parameterlist
  end;
  if AAsLocal then
  begin
    LIndex := -1 - AIndexOffset;
  end;
  for i := 0 to LNames.Count - 1 do
  begin
    LName := LNames.Strings[i];
    LVarDec := TVarDeclaration.Create(LName, LType);
    LVarDec.Line := LLines.Items[i];
    LVarDec.IsConst := AAsConst;
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
  LLines.Free;
end;

procedure TD16Parser.ParseVars;
begin
  FLexer.GetToken.MatchContent('var');
  while not FLexer.PeekToken.IsType(ttReserved) do
  begin
    ParseVarDeclaration(AScope);
  end;
end;

procedure TD16Parser.ParseWhileLoop(AScope: TObjectList<TCodeElement>);
var
  LLoop: TWhileLoop;
begin
  LLoop := TWhileLoop.Create();
  AScope.Add(LLoop);
  LLoop.Line := FLexer.PeekToken.FoundInLine;
  FLexer.GetToken('while');
  ParseRelation(LLoop.Relation);
  FLexer.GetToken('do');
  FLexer.GetToken('begin');
  ParseRoutineContent(LLoop.SubElements);
  FLexer.GetToken('end');
  FLexer.GetToken(';');
end;

procedure TD16Parser.RegisterBasicTypes;
begin
  RegisterType('word');
  RegisterType('char', 2, rtUInteger);
  RegisterType('string', 2, rtString, GetDataType('char'));
  GetDataType('string').Dimensions.Add(1);
  RegisterType('boolean', 2, rtBoolean);
  RegisterType('pointer', 2, rtPointer, GetDataType('word'));
end;

procedure TD16Parser.RegisterOperation(AOpName: string; ALeftType,
  ARightType: TRawType; ALeftSize, ARightSize: Integer; AResultType: TDataType; AAssembler: string);
begin
  FOperations.Add(TOperation.Create(AOpName, ALeftType, ARightType, ALeftSize, ARightSize, AResultType, AAssembler));
end;

procedure TD16Parser.RegisterOperations;
begin
  //word operations
  RegisterOperation('+', rtUInteger, rtUInteger, 2, 2, GetDataType('word'), 'add $0, $1' + sLineBreak);
  RegisterOperation('-', rtUInteger, rtUInteger, 2, 2, GetDataType('word'), 'sub $0, $1' + sLineBreak);
  RegisterOperation('*', rtUInteger, rtUInteger, 2, 2, GetDataType('word'), 'mul $0, $1' + sLineBreak);
  RegisterOperation('/', rtUInteger, rtUInteger, 2, 2, GetDataType('word'), 'div $0, $1' + sLineBreak);
  RegisterOperation('mod', rtUInteger, rtUInteger, 2, 2, GetDataType('word'), 'mod $0, $1' + sLineBreak);
  RegisterOperation('shl', rtUInteger, rtUInteger, 2, 2, GetDataType('word'), 'shl $0, $1' + sLineBreak);
  RegisterOperation('shr', rtUInteger, rtUInteger, 2, 2, GetDataType('word'), 'shr $0, $1' + sLineBreak);
  RegisterOperation('and', rtUInteger, rtUInteger, 2, 2, GetDataType('word'), 'and $0, $1' + sLineBreak);
  RegisterOperation('or', rtUInteger, rtUInteger, 2, 2, GetDataType('word'), 'bor $0, $1' + sLineBreak);
  RegisterOperation('xor', rtUInteger, rtUInteger, 2, 2, GetDataType('word'), 'xor $0, $1' + sLineBreak);
  //word operations returning bool
  RegisterOperation('>', rtUInteger, rtUInteger, 2, 2, GetDataType('boolean'), 'ifg $0, $1' + sLineBreak);
  RegisterOperation('<', rtUInteger, rtUInteger, 2, 2, GetDataType('boolean'), 'ifg $1, $0' + sLineBreak);
  RegisterOperation('=', rtUInteger, rtUInteger, 2, 2, GetDataType('boolean'), 'ife $0, $1' + sLineBreak);
  RegisterOperation('<>', rtUInteger, rtUInteger, 2, 2, GetDataType('boolean'), 'ifn $0, $1' + sLineBreak);
  RegisterOperation('>=', rtUInteger, rtUInteger, 2, 2, GetDataType('boolean'), 'ifg $0, $1' + sLineBreak +
                                                                              'set $2, $3' + sLineBreak +
                                                                              'ife $0, $1' + sLineBreak);
  RegisterOperation('<=', rtUInteger, rtUInteger, 2, 2, GetDataType('word'), 'ifg $1, $0' + sLineBreak +
                                                                              'set $2, $3' + sLineBreak +
                                                                              'ife $0, $1' + sLineBreak);
  //bool operations
  RegisterOperation('and', rtBoolean, rtBoolean, 2, 2, GetDataType('boolean'), 'and $0, $1' + sLineBreak);
  RegisterOperation('or', rtBoolean, rtBoolean, 2, 2, GetDataType('boolean'), 'bor $0, $1' + sLineBreak);

  //pointer operations returning bool
  RegisterOperation('=', rtPointer, rtPointer, 2, 2, GetDataType('boolean'), 'ife $0, $1' + sLineBreak);
  RegisterOperation('<>', rtPointer, rtPointer, 2, 2, GetDataType('boolean'), 'ifn $0, $1' + sLineBreak);
end;

procedure TD16Parser.RegisterSysUnit;
var
  LUnit: TPascalUnit;
begin
  LUnit := TPascalUnit.Create('System');
  FUnits.Add(LUnit);
  FCurrentUnit := LUnit;
  FLexer := LUnit.Lexer;
  RegisterBasicTypes();
  RegisterOperations();
end;

procedure TD16Parser.RegisterType(AName: string; ASize: Integer;
  APrimitive: TRawType; ABaseType: TDataType);
begin
  FCurrentUnit.SubElements.Add(TDataType.Create(AName, ASize, APrimitive, ABaseType));
end;

procedure TD16Parser.Reset;
begin
  Initialize();
end;

procedure TD16Parser.ResetResults;
begin
  FFatals := 0;
  FErrors := 0;
  FWarning := 0;
  FHints := 0;
end;

end.
