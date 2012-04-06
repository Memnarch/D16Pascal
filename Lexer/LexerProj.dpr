program LexerProj;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  Lexer in 'Lexer.pas',
  Token in 'Token.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
