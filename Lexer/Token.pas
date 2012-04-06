unit Token;

interface

uses
  Classes, Types;

type
  TTokenType = (ttIdentifier, ttNumber, ttOperator, ttCharLiteral);

  TToken = class
  private
    FTokenType: TTokenType;
    FContent: string;
  public
    constructor Create(AContent: string; AType: TTokenType);
    function IsContent(AContent: string): Boolean;
    function IsType(AType: TTokenType): Boolean;
    procedure MatchContent(AContent: string);
    procedure MatchType(AType: TTokenType);
    property Content: string read FContent;
    property TokenType: TTokenType read FTokenType;
  end;

  function GetTokenName(AType: TTokenType): string;

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
    raise Exception.Create('Expected ' + QuotedStr(GetTokenName(AType)) + ' but found ' + QuotedStr(GetTokenName(FTokenType)));
  end;
end;

end.
