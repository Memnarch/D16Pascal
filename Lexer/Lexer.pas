unit Lexer;

interface

uses
  Classes, Types, SysUtils, Generics.Collections, Token;

type
  TLexer = class
  private
    FSource: string;
    FPos: Integer;
    FLine: Integer;
    FTokens: TObjectList<TToken>;
    procedure ParseSource();
    procedure ParseIdentifier();
    procedure ParseOperator();
    procedure ParseNumber();
    procedure ParseCharLiteral();
    procedure NextChar();
    procedure NewToken(AContent: string; AType: TTokenType);
    function GetChar(): Char;
    function IsNextChar(AChar: Char): Boolean;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure LoadFromFile(AFile: string);
    procedure LoadFromString(AString: string);
    property Tokens: TObjectList<TToken> read FTokens;
  end;

implementation

{ TLexer }

constructor TLexer.Create;
begin
  FTokens := TObjectList<TToken>.Create();
end;

destructor TLexer.Destroy;
begin
  FTokens.Free;
  inherited;
end;

function TLexer.GetChar: Char;
begin
  Result := FSource[FPos];
end;

function TLexer.IsNextChar(AChar: Char): Boolean;
begin
  Result := ((FPos+1) <= Length(FSource)) and (AChar = FSource[FPos+1]);
end;

procedure TLexer.LoadFromFile(AFile: string);
var
  LText: TStringList;
begin
  LText := TStringList.Create();
  try
    LText.LoadFromFile(AFile);
    LoadFromString(LText.Text);
  finally
    LText.Free;
  end;
end;

procedure TLexer.LoadFromString(AString: string);
begin
  FSource := Trim(AString);
  ParseSource();
end;

procedure TLexer.NewToken(AContent: string; AType: TTokenType);
begin
  FTokens.Add(TToken.Create(AContent, AType));
end;

procedure TLexer.NextChar;
begin
  Inc(FPos);
end;

procedure TLexer.ParseCharLiteral;
var
  LContent: string;
begin
  NextChar();
  LContent := '';
  while GetChar() <> '''' do
  begin
    LContent := LContent + GetChar();
    NextChar();
  end;
  NextChar();
end;

procedure TLexer.ParseIdentifier;
var
  LContent: string;
  LChar: Char;
begin
  LContent := GetChar();
  NextChar();
  LChar := GetChar();
  while CharInSet(LChar, ['A'..'Z','a'..'z','0'..'9', '_']) do
  begin
    LContent := LContent + LChar;
    NextChar();
    LChar := GetChar();
  end;
  NewToken(LContent, ttIdentifier);
end;

procedure TLexer.ParseNumber;
var
  LHasDot: Boolean;
  LContent: string;
  LChar: Char;
begin
  LHasDot := False;
  LContent := GetChar();
  NextChar();
  LChar := GetChar();
  while CharInSet(LChar, ['0'..'9','.']) do
  begin
    if LChar = '.' then
    begin
      if LHasDot then
      begin
        Break;
      end
      else
      begin
        LHasDot := true;
      end;
    end;
    LContent := LContent + LChar;
    NextChar();
    LChar := GetChar();
  end;
  NewToken(LContent, ttNumber);
end;

procedure TLexer.ParseOperator;
var
  LChar: Char;
  LContent: string;
begin
  LChar := GetChar();
  if CharInSet(LChar, ['+', '-']) then
  begin
    NewToken(LChar, ttTermOp);
    NextChar();
  end;
  if CharInSet(LChar, ['*', '/']) then
  begin
    NewToken(LChar, ttFacOp);
    NextChar();
  end;
  if CharInSet(LChar, ['<', '>', '=']) then
  begin
    LContent := LChar;
    if CharInSet(LChar, ['<', '>']) then
    begin
      if ((LChar = '<') and (IsNextChar('>'))) or IsNextChar('=') then
      begin
        NextChar();
        LContent := LContent + GetChar();
      end;
    end;
    NewToken(LContent, ttRelOp);
    NextChar();
  end;
end;

procedure TLexer.ParseSource;
begin
  FPos := 1;
  FLine := 0;
  while FPos <= Length(FSource) do
  begin
    case GetChar of

      'A'..'Z', 'a'..'z', '_':
      begin
        ParseIdentifier();
      end;

      '0'..'9':
      begin
        ParseNumber();
      end;

      '''':
      begin
        ParseCharLiteral();
      end;

      '+', '-', '*', '/', '=', '<', '>':
      begin
        ParseOperator();
      end;

      '(', ')', '[', ']', ',', '.', ';':
      begin
        NewToken(GetChar, ttDelimiter);
        NextChar();
      end;

      ':':
      begin
        if IsNextChar('=') then
        begin
          NextChar();
          NewToken(':=', ttAssignOp);
        end
        else
        begin
          NewToken(':', ttDelimiter);
        end;
        NextChar();
      end;

      #13, #10:
      begin
        if GetChar() = #13 then
        begin
          NextChar();
        end;
        if GetChar() = #10 then
        begin
          NextChar();
          Inc(FLine);
        end;
      end;

      else
        if CharInSet(GetChar(), [#0..#32, #127]) then
        begin
          NextChar();
        end
        else
        begin
          raise Exception.Create('Problem on Character ' + QuotedStr(GetChar()) + ' Ord(' +
            IntToStr(Integer(GetChar())) +  ') in line ' + IntToSTr(FLine));
        end;
    end;
  end;
end;

end.
