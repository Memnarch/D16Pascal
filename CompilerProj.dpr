program CompilerProj;

uses
  Forms,
  Main in 'Main.pas' {Form2},
  D16Parser in 'D16Parser.pas',
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
  ASMBlock in 'ASMBlock.pas',
  D16Assembler in 'D16Assembler.pas',
  Operation in 'Operation.pas',
  Operations in 'Operations.pas',
  HeaderMessage in 'HeaderMessage.pas',
  CaseState in 'CaseState.pas',
  Opcode in 'Opcode.pas',
  CompilerUtil in 'CompilerUtil.pas',
  CompilerDefines in 'CompilerDefines.pas',
  WriterIntf in 'WriterIntf.pas',
  UncountedInterfacedObject in 'UncountedInterfacedObject.pas',
  LineMapping in 'LineMapping.pas',
  Compiler in 'Compiler.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
