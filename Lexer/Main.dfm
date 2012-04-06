object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 537
  ClientWidth = 919
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Source: TMemo
    Left = 448
    Top = 0
    Width = 471
    Height = 537
    Align = alRight
    Lines.Strings = (
      'unit Token;'
      ''
      'interface'
      ''
      'uses'
      '  Classes, Types;'
      ''
      'type'
      
        '  TTokenType = (ttIdentifier, ttNumber, ttTermOp, ttFacOp, ttRel' +
        'Op, ttCharLiteral, ttDelimiter, ttAssignOp);'
      '  TTokenTypes = set of TTokenType;'
      ''
      '  TToken = class'
      '  private'
      '    FTokenType: TTokenType;'
      '    FContent: string;'
      '  public'
      '    constructor Create(AContent: string; AType: TTokenType);'
      '    function IsContent(AContent: string): Boolean;'
      '    function IsType(AType: TTokenType): Boolean;'
      '    procedure MatchContent(AContent: string);'
      '    procedure MatchType(AType: TTokenType);'
      '    property Content: string read FContent;'
      '    property TokenType: TTokenType read FTokenType;'
      '  end;'
      ''
      '  function GetTokenName(AType: TTokenType): string;'
      ''
      'const'
      '  COperators = [ttTermOp, ttFacOp, ttRelOp, ttAssignOp];'
      ''
      'implementation'
      ''
      'uses'
      '  StrUtils, SysUtils, TypInfo;'
      ''
      '  function GetTokenName(AType: TTokenType): string;'
      '  begin'
      '    Result := GetEnumName(TypeInfo(TTokenType), Integer(AType));'
      '  end;'
      ''
      ''
      'constructor TToken.Create(AContent: string; AType: TTokenType);'
      'begin'
      '  FContent := AContent;'
      '  FTokenType := AType;'
      'end;'
      ''
      'function TToken.IsContent(AContent: string): Boolean;'
      'begin'
      '  Result := SameText(AContent, FContent);'
      'end;'
      ''
      'function TToken.IsType(AType: TTokenType): Boolean;'
      'begin'
      '  Result := AType = FTokenType;'
      'end;'
      ''
      'procedure TToken.MatchContent(AContent: string);'
      'begin'
      '  if not IsContent(AContent) then'
      '  begin'
      
        '    raise Exception.Create('#39'Expected '#39' + QuotedStr(AContent) + '#39 +
        ' but found '#39' + QuotedStr(FContent));'
      '  end;'
      'end;'
      ''
      'procedure TToken.MatchType(AType: TTokenType);'
      'begin'
      '  if not IsType(AType) then'
      '  begin'
      
        '    raise Exception.Create('#39'Expected '#39' + QuotedStr(GetTokenName(' +
        'AType)) + '#39' but found '#39' + QuotedStr(GetTokenName(FTokenType)));'
      '  end;'
      'end;'
      ''
      'end.')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Target: TMemo
    Left = 0
    Top = 0
    Width = 361
    Height = 537
    Align = alLeft
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Lex: TButton
    Left = 367
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Lex'
    TabOrder = 2
    OnClick = LexClick
  end
end
