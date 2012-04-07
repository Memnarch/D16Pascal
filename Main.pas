unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm2 = class(TForm)
    Target: TMemo;
    Source: TMemo;
    btnCompile: TButton;
    procedure btnCompileClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
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
  Target.Text := LCompiler.Output.Text;
end;

end.
