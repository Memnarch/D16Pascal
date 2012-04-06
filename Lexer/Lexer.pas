unit Lexer;

interface

uses
  Classes, Types, SysUtils;

type
  TLexer = class
  private
    FSource: string;
    FPos: Integer;
    procedure ParseSource();
    procedure ParseIdentifier();
    procedure ParseOperator();
    procedure ParseNumber();
    procedure ParseCharLiteral();
  public
    procedure LoadFromFile(AFile: string);
    procedure LoadFromString(AString: string);
  end;

implementation

{ TLexer }

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

procedure TLexer.ParseCharLiteral;
begin

end;

procedure TLexer.ParseIdentifier;
begin

end;

procedure TLexer.ParseNumber;
begin

end;

procedure TLexer.ParseOperator;
begin

end;

procedure TLexer.ParseSource;
begin
  FPos := 1;
  while FPos <= Length(FSource) do
  begin
    case FSource[FPos] of

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
    end;
  end;
end;

end.
