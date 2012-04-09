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
  ProcDeclaration in 'ProcDeclaration.pas',
  PascalUnit in 'PascalUnit.pas',
  OpElement in 'OpElement.pas',
  Relation in 'Relation.pas',
  Expression in 'Expression.pas',
  Term in 'Term.pas',
  Factor in 'Factor.pas',
  Assignment in 'Assignment.pas',
  Optimizer in 'Optimizer.pas',
  Condition in 'Condition.pas',
  Loops in 'Loops.pas',
  ProcCall in 'ProcCall.pas',
  ASMBlock in 'ASMBlock.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
