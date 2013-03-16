unit Lexer;

interface

uses
  Classes, Types, SysUtils, Generics.Collections, Token;// SiAuto, SmartInspect;

type
  TLexer = class
  private
    FSource: string;
    FPos: Integer;
    FLine: Integer;
    FTokenIndex: Integer;
    FTokens: TObjectList<TToken>;
    FReserved: TStringList;
    FSimpleTokensOnly: Boolean;
    procedure ParseSource();
    procedure ParseIdentifier();
    procedure ParseOperator();
    procedure ParseNumber();
    procedure ParseCharLiteral();
    procedure ParseSingleComment();
    procedure ParseMultiComment();
    procedure NextChar();
    procedure NewToken(AContent: string; AType: TTokenType);
    procedure InitReserved();
    procedure InsertLineBreak();
    function GetChar(): Char;
    function PeekChar(AOffset: Integer): Char;
    function CanPeek(AOffset: Integer): Boolean;
    function IsNextChar(AChar: Char): Boolean;
    function GetEOF: Boolean;
    function IsParseEOF(): Boolean;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure LoadFromFile(AFile: string);
    procedure LoadFromString(AString: string);
    procedure RemoveComments();
    function GetToken(AContent: string = ''; AType: TTokenType = ttNone): TToken;
    function PeekToken(): TToken;
    function PreviousToken(): TToken;
    function AHeadToken(): TToken;
    property Tokens: TObjectList<TToken> read FTokens;
    property EOF: Boolean read GetEOF;
    property SimpleTokensOnly: Boolean read FSimpleTokensOnly write FSimpleTokensOnly;
  end;

implementation

uses
  StrUtils;

{ TLexer }

function TLexer.AHeadToken: TToken;
begin
  Result := FTokens.Items[FTokenIndex+1];
end;

function TLexer.CanPeek(AOffset: Integer): Boolean;
begin
  Result := (FPos + AOffset) < Length(FSource);
end;

constructor TLexer.Create;
begin
  FTokens := TObjectList<TToken>.Create();
  FReserved := TStringList.Create();
  FSimpleTokensOnly := False;
  InitReserved();
end;

destructor TLexer.Destroy;
begin
  FTokens.Free;
  FReserved.Free;
  inherited;
end;

function TLexer.GetChar: Char;
begin
  //Result := #0;
  //if FPos <= Length(FSource) then
  //begin
    Result := FSource[FPos];
  //end;
end;

function TLexer.GetEOF: Boolean;
begin
  Result := FTokenIndex = FTokens.Count-1;
end;

function TLexer.GetToken(AContent: string = ''; AType: TTokenType = ttNone): TToken;
begin
  Result := PeekToken();
  if AContent <> '' then
  begin
    Result.MatchContent(AContent);
  end;
  if AType <> ttNone then
  begin
    Result.MatchType(AType);
  end;
  Inc(FTokenIndex);
end;

procedure TLexer.InitReserved;
begin
  FReserved.Add('program');
  FReserved.Add('unit');
  FReserved.Add('end');
  FReserved.Add('begin');
  FReserved.Add('var');
  FReserved.Add('type');
  FReserved.Add('uses');
  FReserved.Add('const');
  FReserved.Add('asm');
  FReserved.Add('function');
  FReserved.Add('procedure');
  FReserved.Add('interface');
  FReserved.Add('implementation');
end;

procedure TLexer.InsertLineBreak;
begin
  if FTokens.Count > 0 then
  begin
    FTokens.Items[FTokens.Count-1].FollowedByNewLine := True;
  end;
end;

function TLexer.IsNextChar(AChar: Char): Boolean;
begin
  Result := ((FPos+1) <= Length(FSource)) and (AChar = FSource[FPos+1]);
end;

function TLexer.IsParseEOF: Boolean;
begin
  Result := FPos > Length(FSource);
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
  FTokens.Clear;
  FTokenIndex := 0;
  ParseSource();
  RemoveComments();
end;

procedure TLexer.NewToken(AContent: string; AType: TTokenType);
var
  LToken: TToken;
begin
  LToken := TToken.Create(AContent, AType);
  LToken.FoundInLine := FLine;
  FTokens.Add(LToken);
end;

procedure TLexer.NextChar;
begin
  Inc(FPos);
  if FPos > Length(FSource) then
  begin
  //  FPos := Length(FSource);
  end;
end;

procedure TLexer.ParseCharLiteral;
var
  LContent: string;
  LCloseChar: Char;
begin
  LCloseChar := GetChar();
  NextChar();
  LContent := '';
  while (GetChar() <> LCloseChar) and (GetChar() <> #10) and (GetChar() <> #13) do
  begin
    LContent := LContent + GetChar();
    NextChar();
  end;
  NewToken(LContent, ttCharLiteral);
  if (GetChar() <> #10) and (GetChar() <> #13) then
  begin
    NextChar();
  end;
end;

procedure TLexer.ParseIdentifier;
var
  LContent: string;
begin
  LContent := '';
  //LContent := GetChar();
  //NextChar();
  while (not IsParseEOF) and CharInSet(GetChar(), ['A'..'Z','a'..'z','0'..'9', '_']) do
  begin
    LContent := LContent + GetChar();
    NextChar();
    //LChar := GetChar();
  end;
  if FSimpleTokensOnly then
  begin
    NewToken(LContent, ttIdentifier);
  end
  else
  begin
    if FReserved.IndexOf(LContent) < 0 then
    begin
      if AnsiIndexText(LContent, ['and', 'mod', 'shl', 'shr']) >= 0 then
      begin
        NewToken(LContent, ttFacOp);
      end
      else
      begin
        if AnsiIndexText(LContent, ['or', 'xor']) >= 0 then
        begin
          NewToken(LContent, ttTermOp);
        end
        else
        begin
          NewToken(LContent, ttIdentifier);
        end;
      end;
    end
    else
    begin
      NewToken(LContent, ttReserved);
    end;
  end;
end;

procedure TLexer.ParseMultiComment;
var
  LContent: string;
begin
  LContent := '';
  while not IsParseEOF do
  begin
    LContent := LContent + GetChar();
    if GetChar() = '}' then
    begin
      NextChar();
      Break;
    end;
    NextChar();
  end;
  NewToken(LContent, ttComment);
end;

procedure TLexer.ParseNumber;
var
  LHasDot, LIsHexa: Boolean;
  LContent: string;
  LChar: Char;
begin
  LHasDot := False;
  LIsHexa := False;
  LContent := GetChar();
  NextChar();
  LChar := GetChar();
  while (CharInSet(LChar, ['0'..'9','.', 'x'])) or (LIsHexa and CharInSet(LChar, ['0'..'9','x', 'X', 'a'..'f', 'A'..'F'])) do
  begin
    if LChar = '.' then
    begin
      if LHasDot or LIsHexa then
      begin
        Break;
      end
      else
      begin
        LHasDot := true;
      end;
    end;
    if (SameText(LChar, 'x')) then
    begin
      if SameText(LContent, '0') and not (LIsHexa) then
      begin
        LIsHexa := True;
      end
      else
      begin
        Break;
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
  if CharInSet(LChar, ['*', '/', '@', '^']) then
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

procedure TLexer.ParseSingleComment;
var
  LContent: string;
begin
  LContent := '';
  while not IsParseEOF do
  begin
    if (GetChar() = #10) or ((GetChar() = #13) and CanPeek(1) and (PeekChar(1) = #10))  then
    begin
      Break;
    end
    else
    begin
      LContent := GetChar();
    end;
    NextChar();
  end;
  NewToken(LContent, ttComment);
  FTokens.Items[FTokens.Count - 1].FollowedByNewLine := True;
end;

procedure TLexer.ParseSource;
var
  LChar: Char;
begin
  FPos := 1;
  FLine := 1;
  while FPos <= Length(FSource) do
  begin
    if FPos >= Length(FSource) then
    begin
//      SiMain.LogMessage('fail');
//      raise Exception.Create('Fail in Lexer');
      LChar := GetChar();
    end;
    case GetChar of

      'A'..'Z', 'a'..'z', '_':
      begin
        ParseIdentifier();
      end;

      '0'..'9':
      begin
        ParseNumber();
      end;

      '''', '"':
      begin
        ParseCharLiteral();
      end;

      '+', '-', '*', '=', '<', '>', '@', '^':
      begin
        ParseOperator();
      end;

      '/':
      begin
        if (GetChar() = '/') and CanPeek(1) and (PeekChar(1) = '/') then
        begin
          ParseSingleComment();
        end
        else
        begin
          ParseOperator();
        end;
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
          InsertLineBreak();
        end;
      end;

      '{':
      begin
        ParseMultiComment();
      end

      else
        if CharInSet(GetChar(), [#0..#32, #127]) then
        begin
          NextChar();
        end
        else
        begin
          NewToken(GetChar, ttNone);
          NextChar();
//          raise Exception.Create('Problem on Character ' + QuotedStr(GetChar()) + ' Ord(' +
//            IntToStr(Integer(GetChar())) +  ') in line ' + IntToSTr(FLine));
        end;
    end;
  end;
  NewToken('EOF', ttEOF);
end;

function TLexer.PeekChar(AOffset: Integer): Char;
begin
  Result := FSource[FPos + AOffset];
end;

function TLexer.PeekToken: TToken;
begin
  if FTokenIndex > (FTokens.Count-1) then
  begin
    raise EAbort.Create('Unexpected EOF');
  end;
  Result := FTokens.Items[FTokenIndex];
end;

function TLexer.PreviousToken: TToken;
begin
  if FTokenIndex > 0 then
  begin
    Result := FTokens.Items[FTokenIndex-1];
  end
  else
  begin
    Result := FTokens.Items[0];
  end;
end;

procedure TLexer.RemoveComments;
var
  i: Integer;
begin
  for i := FTokens.Count - 1 downto 0 do
  begin
    if FTokens.Items[i].IsType(ttComment) then
    begin
      if ((i-1) >= 0) and FTokens.Items[i].FollowedByNewLine then
      begin
        FTokens.Items[i-1].FollowedByNewLine := True;
      end;
      FTokens.Delete(i);
    end;
  end;
end;

end.
