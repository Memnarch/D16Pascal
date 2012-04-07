unit Compiler;

interface

uses
  Classes, Types, Generics.Collections, SysUtils, Lexer, Token, CodeElement, DataType,
  VarDeclaration;

type
  TCompiler = class
  private
    FLexer: TLexer;
    FOutput: TStringList;
    FElements: TObjectList<TCodeElement>;
    procedure CompileUnit();
    procedure PushValue(AName: string);
    procedure PopValue(AName: string);
    procedure CallProcedure(AName: string; AArgs: array of string);
    procedure ProcProlog(ALocalCount: Integer);
    procedure ProcEpilog();
    procedure AddOut(ALine: string);
    procedure RegisterType(AName: string; ASize: Integer = 2; APrimitive: TDataPrimitive = dpInteger);
    procedure RegisterBasicTypes();
    function GetElement(AName: string; AType: TCodeElementClass): TCodeElement;
    function GetDataType(AName: string): TDataType;
    function GetVar(AName: string): TVarDeclaration;
    procedure RegisterVar(AName: string; AType: TDataType);
    //parse functions
    procedure ParseUnitHeader();
    procedure ParseUnitFooter();
    procedure ParseUses();
    procedure ParseVars();
    procedure ParseVarDeclaration();
    procedure ParseConsts();
    procedure ParseRoutineDeclaration();
    procedure Fatal(AMessage: string);
  public
    constructor Create();
    destructor Destroy(); reintroduce;
    procedure CompileFile(AFile: string);
    procedure CompilerSource(ASource: string);
    property Output: TStringList read FOutput;
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
  StrUtils;

{ TCompiler }

procedure TCompiler.AddOut(ALine: string);
begin
  FOutput.Add(ALine);
end;

procedure TCompiler.CallProcedure(AName: string; AArgs: array of string);
var
  i: Integer;
begin
  for i := High(AArgs) downto 0 do
  begin
    if i > 2 then
    begin
      PushValue(AArgs[i]);
    end
    else
    begin
      case i of
        0:
        begin
          AddOut('set A, ' + AArgs[i]);
        end;

        1:
        begin
          AddOut('set B, ' + AArgs[i]);
        end;

        2:
        begin
          AddOut('set C, ' + AArgs[i]);
        end;
      end;
    end;
  end;
  AddOut('jsr ' + AName);
  if High(AArgs) - 3 > 0 then
  begin
    AddOut('add sp, ' + IntToStr(High(AArgs)-3));
  end;
end;

procedure TCompiler.CompileFile(AFile: string);
begin
  FOutput.Clear;
  FLexer.LoadFromFile(AFile);
  CompileUnit();
end;

procedure TCompiler.CompilerSource(ASource: string);
begin
  FOutput.Clear();
  FLexer.LoadFromString(ASource);
  CompileUnit();
end;

procedure TCompiler.CompileUnit;
begin
  ParseUnitHeader();
  while True do
  begin
    case AnsiIndexText(FLexer.PeekToken.Content, ['uses', 'var', 'const', 'procedure', 'function']) of
      0:
      begin
        ParseUses();
      end;

      1:
      begin
        ParseVars();
      end;

      2:
      begin
        ParseConsts();
      end;

      3, 4:
      begin
        ParseRoutineDeclaration();
      end;

      else
        break;
    end;
  end;
  ParseUnitFooter();
end;

constructor TCompiler.Create;
begin
  FLexer := TLexer.Create();
  FOutput := TStringList.Create();
  FElements := TObjectList<TCodeElement>.Create();
  RegisterBasicTypes();
end;

destructor TCompiler.Destroy;
begin
  FLexer.Free;
  FOutput.Free;
  FElements.Free;
  inherited;
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

function TCompiler.GetElement(AName: string;
  AType: TCodeElementClass): TCodeElement;
var
  LElement: TCodeElement;
begin
  Result := nil;
  for LElement in FElements do
  begin
    if SameText(AName, LElement.Name) and LElement.InheritsFrom(AType) then
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

procedure TCompiler.ParseConsts;
begin

end;

procedure TCompiler.ParseRoutineDeclaration;
begin

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
  FLexer.GetToken(';');
  for LName in LNames do
  begin
    RegisterVar(LName, LType);
    AddOut(':' + LName + ' dat 0x0');
  end;
  LNames.Free;
end;

procedure TCompiler.ParseVars;
begin
  FLexer.GetToken.MatchContent('var');
  while not FLexer.PeekToken.IsType(ttReserved) do
  begin
    ParseVarDeclaration();
  end;
end;

procedure TCompiler.PopValue(AName: string);
begin
  AddOut('set ' + AName + ', pop');
end;

procedure TCompiler.ProcEpilog();
begin
  AddOut('set sp, j');
  PopValue(CRegJ);
  PopValue(CRegPC);
end;

procedure TCompiler.ProcProlog(ALocalCount: Integer);
begin
  PopValue(CRegJ);
  AddOut('set j, sp');
  if ALocalCount > 0 then
  begin
    AddOut('sub sp, ' + IntToStr(ALocalCount));
  end;
end;

procedure TCompiler.PushValue(AName: string);
begin
  AddOut('set push, ' + AName);
end;

procedure TCompiler.RegisterBasicTypes;
begin
  RegisterType('word');
end;

procedure TCompiler.RegisterType(AName: string; ASize: Integer;
  APrimitive: TDataPrimitive);
begin
  FElements.Add(TDataType.Create(AName, ASize, APrimitive));
end;

procedure TCompiler.RegisterVar(AName: string; AType: TDataType);
begin
  FElements.Add(TVarDeclaration.Create(AName, AType));
end;

end.
