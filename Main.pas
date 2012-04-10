unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SynEdit, SynHighlighterAsm, SynEditHighlighter,
  SynHighlighterPas, ComCtrls, ToolWin, Compiler, ImgList;

type
  TForm2 = class(TForm)
    Log: TSynEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    FileOpenDialog: TFileOpenDialog;
    ToolImages: TImageList;
    procedure btnCompileClick(Sender: TObject);
  private
    { Private declarations }
    procedure OnMessage(AMessage, AUnit: string; ALine: Integer; ALevel: TMessageLevel);
  public
    { Public declarations }
    procedure RefreshTargetIdent(ALines: TStrings);
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.btnCompileClick(Sender: TObject);
var
  LCompiler: TCompiler;
  LMainPath: string;
  LFile: string;
  LOut: TStringList;
begin
  if not FileOpenDialog.Execute then
  begin
    Exit;
  end;
  Log.Clear;
  LCompiler := TCompiler.Create();
  LCompiler.OnMessage := OnMessage;
  LOut := TStringList.Create();
  LMainPath := ExtractFilePath(FileOpenDialog.FileName);
  LFile := ExtractFileName(FileOpenDialog.FileName);
  LCompiler.SearchPath.Add(LMainPath);
  LCompiler.CompileFile(LFile);
  LOut.Text := Trim(LCompiler.GetDCPUSource());
  RefreshTargetIdent(LOut);
  LOut.SaveToFile(LMainPath + '\' + ChangeFileExt(LFile, '.asm'));
  LCompiler.Free;
  LOut.Free;
  Log.Lines.Add('finished');
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
