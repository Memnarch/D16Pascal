unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SynEdit, SynHighlighterAsm, SynEditHighlighter,
  SynHighlighterPas, ComCtrls, ToolWin, Compiler, CompilerDefines, ImgList, ExtCtrls, SiAuto, SmartInspect;

type
  TForm2 = class(TForm)
    Log: TSynEdit;
    ToolBar1: TToolBar;
    btnCompile: TToolButton;
    ToolImages: TImageList;
    Panel1: TPanel;
    cbOptimize: TCheckBox;
    OpenDialog: TOpenDialog;
    cbAssemble: TCheckBox;
    cbModule: TCheckBox;
    cbUseBigEndian: TCheckBox;
    procedure btnCompileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbAssembleClick(Sender: TObject);
  private
    { Private declarations }
    FErrors: Integer;
    procedure OnMessage(AMessage, AUnit: string; ALine: Integer; ALevel: TMessageLevel);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation
uses
  HeaderMessage, D16Assembler, CompilerUtil;

{$R *.dfm}

procedure TForm2.btnCompileClick(Sender: TObject);

begin
  if not OpenDialog.Execute then
  begin
    Exit;
  end;
  Log.Clear;
  CompileFile(OpenDialog.FileName, cbOptimize.Checked, cbAssemble.Checked,
    cbModule.Checked, cbUseBigEndian.Checked, OnMessage);
end;

procedure TForm2.cbAssembleClick(Sender: TObject);
begin
  cbModule.Enabled := cbAssemble.Checked;
  cbUseBigEndian.Enabled := cbAssemble.Checked;
end;

procedure TForm2.FormCreate(Sender: TObject);
//var
//  LCompiler: TCompiler;
begin
  Log.Text := CWelcomeMessage;
//  Log.Clear;
//  Log.Gutter.ShowLineNumbers := True;
//  LCompiler := TCompiler.Create();
//  LCompiler.OnMessage := OnMessage;
//  LCompiler.SearchPath.Add('E:\Git\D16Pascal\DemoSource\');
//  LCompiler.CompileFile('Demo.pas');
//  Log.Lines.Text := Trim(LCompiler.GetDCPUSource());
//  RefreshTargetIdent(Log.Lines);
//  LCompiler.Free;
end;

procedure TForm2.OnMessage(AMessage, AUnit: string; ALine: Integer;
  ALevel: TMessageLevel);
begin
  if ALevel = mlNone then
  begin
    Log.Lines.Add(AMessage);
  end
  else
  begin
    Log.Lines.Add('Error in ' + QuotedStr(AUnit) + ' line ' + IntToStr(ALine) + ': ' + AMessage);
    Inc(FErrors);
  end;
end;


initialization
Si.Enabled := True;

end.
