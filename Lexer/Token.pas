unit Token;

interface

uses
  Classes, Types;

type
  TTokenType = (ttNone, ttIdentifier, ttNumber, ttTermOp, ttFacOp, ttRelOp, ttCharLiteral,
    ttDelimiter, ttAssignOp, ttEOF, ttReserved);
  TTokenTypes = set of TTokenType;

  TToken = class
  private
    FTokenType: TTokenType;
    FContent: string;
    FFollowedByNewLine: Boolean;
    FFoundInLine: Integer;
  public
    constructor Create(AContent: string; AType: TTokenType);
    function IsContent(AContent: string): Boolean;
    function IsType(AType: TTokenType): Boolean;
    function ToString(): string;
    procedure MatchContent(AContent: string);
    procedure MatchType(AType: TTokenType);
    property Content: string read FContent;
    property TokenType: TTokenType read FTokenType;
    property FollowedByNewLine: Boolean read FFollowedByNewLine write FFollowedByNewLine;
    property FoundInLine: Integer read FFoundInLine write FFoundInLine;
  end;

  function GetTokenName(AType: TTokenType): string;

const
  COperators = [ttTermOp, ttFacOp, ttRelOp, ttAssignOp];

implementation

uses
  StrUtils, SysUtils, TypInfo;

  function GetTokenName(AType: TTokenType): string;
  begin
    Result := GetEnumName(TypeInfo(TTokenType), Integer(AType));
  end;

{ TToken }

constructor TToken.Create(AContent: string; AType: TTokenType);
begin
  FContent := AContent;
  FTokenType := AType;
  FFoundInLine := -1;
end;

function TToken.IsContent(AContent: string): Boolean;
begin
  Result := SameText(AContent, FContent);
end;

function TToken.IsType(AType: TTokenType): Boolean;
begin
  Result := AType = FTokenType;
end;

procedure TToken.MatchContent(AContent: string);
begin
  if not IsContent(AContent) then
  begin
    raise Exception.Create('Expected ' + QuotedStr(AContent) + ' but found ' + QuotedStr(FContent));
  end;
end;

procedure TToken.MatchType(AType: TTokenType);
begin
  if not IsType(AType) then
  begin
    raise Exception.Create('Expected ' + QuotedStr(GetTokenName(AType)) + ' but found ' + QuotedStr(GetTokenName(FTokenType)) + ' ' + QuotedStr(FContent));
  end;
end;

function TToken.ToString: string;
begin
  Result := 'Content: ' + QuotedStr(FContent) + ' Type: ' + QuotedStr(GetTokenName(FTokenType));
end;

end.
