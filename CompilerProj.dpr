program CompilerProj;

uses
  Forms,
  Main in 'Main.pas' {Form2},
  Compiler in 'Compiler.pas',
  Lexer in 'Lexer\Lexer.pas',
  Token in 'Lexer\Token.pas',
  DataType in 'DataType.pas',
  VarDeclaration in 'VarDeclaration.pas',
  CodeElement in 'CodeElement.pas',
  ProcDeclaration in 'ProcDeclaration.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
