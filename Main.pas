unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SynEdit, SynHighlighterAsm, SynEditHighlighter,
  SynHighlighterPas, ComCtrls, ToolWin, Compiler, ImgList, ExtCtrls;

type
  TForm2 = class(TForm)
    Log: TSynEdit;
    ToolBar1: TToolBar;
    btnCompile: TToolButton;
    ToolImages: TImageList;
    Panel1: TPanel;
    cbOptimize: TCheckBox;
    OpenDialog: TOpenDialog;
    procedure btnCompileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FErrors: Integer;
    procedure OnMessage(AMessage, AUnit: string; ALine: Integer; ALevel: TMessageLevel);
  public
    { Public declarations }
    procedure RefreshTargetIdent(ALines: TStrings);
  end;

var
  Form2: TForm2;

implementation
uses
  Optimizer;

{$R *.dfm}

procedure TForm2.btnCompileClick(Sender: TObject);
var
  LCompiler: TCompiler;
  LMainPath, LSavePath: string;
  LFile: string;
  LOut: TStringList;
begin
  if not OpenDialog.Execute then
  begin
    Exit;
  end;
  DoOptimization := cbOptimize.Checked;
  FErrors := 0;
  Log.Clear;
  LCompiler := TCompiler.Create();
  LCompiler.OnMessage := OnMessage;
  LOut := TStringList.Create();
  LMainPath := ExtractFilePath(OpenDialog.FileName);
  LFile := ExtractFileName(OpenDialog.FileName);
  LCompiler.SearchPath.Add(LMainPath);
  LCompiler.CompileFile(LFile);
  LOut.Text := Trim(LCompiler.GetDCPUSource());
  RefreshTargetIdent(LOut);
  if FErrors = 0 then
  begin
    LSavePath := LMainPath + ChangeFileExt(LFile, '.asm');
    LOut.SaveToFile(LSavePath);
    Log.Lines.Add('Saved to: ' + LSavePath);
  end;
  Log.Lines.Add('Errors: ' + IntToSTr(FErrors));
  Log.Lines.Add('finished');
  LCompiler.Free;
  LOut.Free;
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  LCompiler: TCompiler;
begin
  Log.Clear;
  LCompiler := TCompiler.Create();
  LCompiler.OnMessage := OnMessage;
  LCompiler.SearchPath.Add('E:\Git\D16Pascal\DemoSource\');
  LCompiler.CompileFile('Demo.pas');
  Log.Lines.Text := Trim(LCompiler.GetDCPUSource());
  RefreshTargetIdent(Log.Lines);
  LCompiler.Free;
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

procedure TForm2.RefreshTargetIdent;
var
  i: Integer;
  LSpace: string;
begin
  LSpace := '               ';
  for i := 0 to ALines.Count - 1 do
  begin
    if ALines.Strings[i][1] <> ':' then
    begin
      ALines.Strings[i] := LSpace + ALines.Strings[i];
    end;
  end;
end;

end.
