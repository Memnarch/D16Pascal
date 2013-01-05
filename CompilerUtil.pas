unit CompilerUtil;

interface

uses
  CompilerDefines;

procedure CompileFile(AFile: string; AOptimize, AAssemble, ABuildModule, AUseBigEndiang: Boolean; AOnMessage: TOnMessage);

implementation

uses
  Classes, Types, SysUtils, Compiler, D16Assembler, Optimizer, LineMapping;



procedure RefreshTargetIdent(ALines: TStrings);
var
  i: Integer;
  LSpace: string;
begin
  LSpace := '               ';
  for i := 0 to ALines.Count - 1 do
  begin
    if (Length(ALines.Strings[i]) > 0) and (ALines.Strings[i][1] <> ':') then
    begin
      ALines.Strings[i] := LSpace + ALines.Strings[i];
    end;
  end;
end;

procedure CompileFile(AFile: string; AOptimize, AAssemble, ABuildModule, AUseBigEndiang: Boolean; AOnMessage: TOnMessage);
var
  LCompiler: TCompiler;
  LAssembler: TD16Assembler;
  LMainPath, LSavePath: string;
  LFile: string;
  LOut: TStringList;
  LMapping: TLineMapping;
  LDebugMapping: TSTringList;
begin
  LAssembler := TD16Assembler.Create();
  LCompiler := TCompiler.Create();
  LCompiler.OnMessage := AOnMessage;
  DoOptimization := AOptimize;
  LOut := TStringList.Create();
  LMainPath := ExtractFilePath(AFile);
  LFile := ExtractFileName(AFile);
  LCompiler.SearchPath.Add(LMainPath);
  LCompiler.CompileFile(LFile);
  if LCompiler.Errors+LCompiler.Fatals = 0 then
  begin
    LOut.Text := Trim(LCompiler.GetDCPUSource());
    RefreshTargetIdent(LOut);
    LSavePath := LMainPath + ChangeFileExt(LFile, '.asm');
    LOut.SaveToFile(LSavePath);
    AOnMessage('Saved to: ' + LSavePath, '', 0, mlNone);
    if AAssemble then
    begin
      try
        AOnMessage('Assembling...', '', 0, mlNone);
        LAssembler.UseBigEdian := AUseBigEndiang;
        if AUseBigEndiang then
        begin
          AOnMessage('Using BigEndian', '', 0, mlNone);
        end;
        LAssembler.UseLineMappings(LCompiler.LineMapping);
        LAssembler.AssembleFile(LSavePath);
        LAssembler.SaveTo(ChangeFileExt(LSavePath, '.d16'));
        AOnMessage('Assembled to: '  + ChangeFileExt(LSavePath, '.d16'), '', 0, mlNone);
        if ABuildModule then
        begin
          LAssembler.SaveAsModuleTo(ChangeFileExt(LSavePath, '.d16m'));
          AOnMessage('Saved as module to: ' + ChangeFileExt(LSavePath, '.d16m'), '', 0, mlNone);
        end;
      except
        on e: Exception do
        begin
          LMapping := LCompiler.GetMappingByASMLine(LAssembler.ErrorLine);
          if Assigned(LMapping)  then
          begin
            AOnMessage(e.Message, ChangeFileExt(LMapping.D16UnitName, '.pas'), LMapping.UnitLine, mlFatal);
          end
          else
          begin
            AOnMessage(e.Message, ChangeFileExt(LFile, '.asm'), LAssembler.ErrorLine, mlFatal);
          end;
        end;
      end;
    end;
  end;
  AOnMessage('Fatals: ' + IntToSTr(LCompiler.Fatals), '', 0, mlNone);
  AOnMessage('Errors: ' + IntToSTr(LCompiler.Errors), '', 0, mlNone);
  AOnMessage('Warnings: ' + IntToSTr(LCompiler.Warnings), '', 0, mlNone);
  AOnMessage('finished', '', 0, mlNone);
  LDebugMapping := TStringList.Create();
  for LMapping in LCompiler.LineMapping do
  begin
    LDebugMapping.Add(LMapping.AsText);
  end;
  LDebugMapping.SaveToFile(ExtractFilePath(AFile) + '\Mapping.txt');
  LCompiler.Free;
  LAssembler.Free;
  LOut.Free;
end;

end.
