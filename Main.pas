unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SynEdit, SynHighlighterAsm, SynEditHighlighter,
  SynHighlighterPas, ComCtrls, ToolWin, Compiler, ImgList, ExtCtrls, SiAuto, SmartInspect;

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
    procedure RefreshTargetIdent(ALines: TStrings);
  end;

var
  Form2: TForm2;

implementation
uses
 Optimizer, HeaderMessage, D16Assembler;

{$R *.dfm}

procedure TForm2.btnCompileClick(Sender: TObject);
var
  LCompiler: TCompiler;
  LAssembler: TD16Assembler;
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
  LAssembler := TD16Assembler.Create();
  LCompiler := TCompiler.Create();
  LCompiler.OnMessage := OnMessage;
  LOut := TStringList.Create();
  LMainPath := ExtractFilePath(OpenDialog.FileName);
  LFile := ExtractFileName(OpenDialog.FileName);
  LCompiler.SearchPath.Add(LMainPath);
  Log.Refresh;
  LCompiler.CompileFile(LFile);
  LOut.Text := Trim(LCompiler.GetDCPUSource());
  RefreshTargetIdent(LOut);
  if FErrors = 0 then
  begin
    LSavePath := LMainPath + ChangeFileExt(LFile, '.asm');
    LOut.SaveToFile(LSavePath);
    Log.Lines.Add('Saved to: ' + LSavePath);
    Log.Refresh;
    if cbAssemble.Checked then
    begin
      try
        Log.Lines.Add('Assembling...');
        Log.Refresh;
        LAssembler.UseBigEdian := cbUseBigEndian.Checked;
        if cbUseBigEndian.Checked then
        begin
          Log.Lines.Add('Using BigEndian');
          Log.Refresh;
        end;
        LAssembler.AssembleFile(LSavePath);
        LAssembler.SaveTo(ChangeFileExt(LSavePath, '.d16'));
        Log.Lines.Add('Assembled to: '  + ChangeFileExt(LSavePath, '.d16'));
        Log.Refresh;
        if cbModule.Checked then
        begin
          LAssembler.SaveAsModuleTo(ChangeFileExt(LSavePath, '.d16m'));
          Log.Lines.Add('Saved as module to: ' + ChangeFileExt(LSavePath, '.d16m'));
          Log.Refresh;
        end;
      except
        on e: Exception do
        begin
          OnMessage(e.Message, ChangeFileExt(LFile, '.asm'), LAssembler.Lexer.PeekToken.FoundInLine, mlFatal);
        end;
      end;
    end;
  end;
  Log.Lines.Add('Errors: ' + IntToSTr(FErrors));
  Log.Lines.Add('finished');
  Log.Refresh;
  LCompiler.Free;
  LAssembler.Free;
  LOut.Free;
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

initialization
Si.Enabled := True;

end.
