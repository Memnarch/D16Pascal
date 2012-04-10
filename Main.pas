unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SynEdit, SynHighlighterAsm, SynEditHighlighter,
  SynHighlighterPas;

type
  TForm2 = class(TForm)
    btnCompile: TButton;
    Source: TSynEdit;
    Target: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    SynAsmSyn1: TSynAsmSyn;
    procedure btnCompileClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure RefreshTargetIdent();
  end;

var
  Form2: TForm2;

implementation

uses
  Compiler;

{$R *.dfm}

procedure TForm2.btnCompileClick(Sender: TObject);
var
  LCompiler: TCompiler;
begin
  LCompiler := TCompiler.Create();
  LCompiler.CompilerSource(Source.Text);
  Target.Text := Trim(LCompiler.GetDCPUSource());
  LCompiler.Free;
  RefreshTargetIdent();
end;

procedure TForm2.RefreshTargetIdent;
var
  i: Integer;
  LSpace: string;
begin
  LSpace := '               ';
  for i := 0 to Target.Lines.Count - 1 do
  begin
    if Target.Lines.Strings[i][1] <> ':' then
    begin
      Target.Lines.Strings[i] := LSpace + Target.Lines.Strings[i];
    end;
  end;
end;

end.
