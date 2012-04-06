unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Source: TMemo;
    Target: TMemo;
    Lex: TButton;
    procedure LexClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Lexer, Token;

{$R *.dfm}

procedure TForm1.LexClick(Sender: TObject);
var
  LLexer: TLexer;
  LToken: TToken;
begin
  LLexer := TLexer.Create();
  LLexer.LoadFromString(Source.Text);
  Target.Clear;
  for LToken in LLexer.Tokens do
  begin
    Target.Lines.Add(LToken.ToString);
  end;
end;

end.
