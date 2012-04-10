unit Optimizer;

interface

function OptimizeDCPUCode(AIn: string): string;

implementation

uses
  Classes, Types, SysUtils, StrUtils;


procedure SplitLine(ALine: string; var AOp, ATarget, ASource: string);
var
  LPos, LPosB: Integer;
begin
  LPos := Pos(' ', ALine);
  LPosB := Pos(',', ALine);
  AOp := Trim(Copy(ALine, 1, LPos-1));
  ATarget := Trim(Copy(ALine, LPos+1, LPosB-LPos-1));
  ASource := Trim(Copy(ALine, LPosB+1, Length(ALine)-LPosB));
end;

procedure RemoveEmptyLines(ALines: TStrings);
var
  i: Integer;
begin
  for i := ALines.Count - 1 downto 0 do
  begin
    if Trim(ALines.Strings[i]) = '' then
    begin
      ALines.Delete(i);
    end;
  end;
end;

procedure OptimizePushPop(ALines: TStrings);
var
  i, k, m, LCount: Integer;
  LOpA, LOpB, LTargetA, LTargetB, LSourceA, LSourceB: string;
begin
  k := 0;
  for i := 0 to ALines.Count - 2 do
  begin
    if StartsText('set push', ALines.Strings[i]) then
    begin
      k := i;
      while StartsText('set push', ALines.Strings[k]) do
      begin
        Inc(k);
      end;
      LCount := 0;
      for m := k-1 downto i do
      begin
        SplitLine(ALines.Strings[m], LOpA, LTargetA, LSourceA);
        SplitLine(ALines.Strings[k+LCount], LOpB, LTargetB, LSourceB);
        if SameText(LOpA, 'set') and SameText(LOpB, 'set')
          and SameText(LTargetA, 'push') and SameText(LSourceB, 'pop') then
        begin
          if not SameText(LTargetB, LSourceA) then
          begin
            ALines.Strings[k+LCount] := 'set ' + LTargetB + ', ' + LSourceA;
          end
          else
          begin
            ALines.Strings[k+LCount] := '';
          end;
          ALines.Strings[m] := '';
        end
        else
        begin
          Break;
        end;
        Inc(LCount);
      end;
    end;
  end;
  RemoveEmptyLines(ALines);
end;

procedure OptimizePushNopPop(ALines: TStrings);
var
  LOpA, LOpB, LTargetA, LTargetB, LSourceA, LSourceB: string;
  i, k: Integer;
  LLevel: Integer;
begin
  for i := 0  to ALines.Count-1 do
  begin
    SplitLine(ALines.Strings[i], LOpA, LTargetA, LSourceA);
    if SameText(LTargetA, 'push') then
    begin
      LLevel := 1;
      for k := i+1 to ALines.Count-1 do
      begin
        SplitLine(ALines.Strings[k], LOpB, LTargetB, LSourceB);
        if SameText(LTargetB, 'push') then
        begin
          Inc(LLevel);
        end;
        if SameText(LSourceB, 'pop') then
        begin
          Dec(LLevel);
          if LLevel = 0 then
          begin
            ALines.Strings[i] := '';
            if not SameText(LTargetB, LSourceA) then
            begin
              ALines.Strings[k] := LOpB + ' ' + LTargetB + ', ' + LSourceA;
            end
            else
            begin
              ALines.Strings[k] := '';
            end;
            Break;
          end;
        end;
        if SameText(LTargetB, LSourceA) then
        begin
          Break;
        end;
      end;
    end;
  end;
  RemoveEmptyLines(ALines);
end;

procedure OptimizeMoveOP(ALines: TStrings);
var
  LOpA, LOpB, LTargetA, LTargetB, LSourceA, LSourceB: string;
  i: Integer;
begin
  for i := 0 to ALines.Count - 2 do
  begin
    SplitLine(ALines.Strings[i], LOpA, LTargetA, LSourceA);
    SplitLine(ALines.Strings[i+1], LOpB, LTargetB, LSourceB);
    if SameText(LOpA, 'set') and (not SameText(LOpB, 'set')) then
    begin
      if SameText(LTargetA, LSourceB) then
      begin
        ALines.Strings[i] := '';
        ALines.Strings[i+1] := LOpB + ' ' + LTargetB + ', ' + LSourceA;
      end;
    end;
  end;
  RemoveEmptyLines(ALines);
end;

function IsWriteAbleTarget(AIn: string): Boolean;
begin
  Result := not CharInSet(AIn[1], ['0'..'9']);
end;

procedure OptimizeMoveOpMove(ALines: TStrings);
var
  LOpA, LOpB, LOpC, LTargetA, LTargetB, LTargetC, LSourceA, LSourceB, LSourceC: string;
  i: Integer;
begin
  for i := 0 to ALines.Count - 3 do
  begin
    SplitLine(ALines.Strings[i], LOpA, LTargetA, LSourceA);
    SplitLine(ALines.Strings[i+1], LOpB, LTargetB, LSourceB);
    SplitLine(ALines.Strings[i+2], LOpC, LTargetC, LSourceC);
    if SameText(LOpA, 'set') and SameText(LOpC, 'set') and (not SameText(LOpB, 'set')) then
    begin
      if SameText(LTargetA, LTargetB) and SameText(LTargetB, LSourceC) then
      begin
        if SameText(LSourceA, LTargetC) then
        begin
          ALines.Strings[i] := '';
          ALines.Strings[i+1] := LOpB + ' ' + LTargetC + ', ' + LSourceB;
          ALines.Strings[i+2] := '';
        end
        else
        begin
          ALines.Strings[i] := LOpA + ' ' + LTargetC + ', ' + LSourceA;
          ALines.Strings[i+1] := LOpB + ' ' + LTargetC + ', ' + LSourceB;
          ALines.Strings[i+2] := '';
        end;
      end;
    end;
  end;
  RemoveEmptyLines(ALines);
end;

procedure OptimizeMoveMove(ALines: TStrings);
var
  LOpA, LOpB, LTargetA, LTargetB, LSourceA, LSourceB: string;
  i: Integer;
  LSkipNext: Boolean;
begin
  for i := 0 to ALines.Count-2 do
  begin
    if LSkipNext then
    begin
      LSkipNext := False;
      continue;
    end;
    SplitLine(ALines.Strings[i], LOpA, LTargetA,LSourceA);
    if StartsText('if', LOpA) then
    begin
      LSkipNext := True;
      continue;
    end;
    SplitLine(ALines.Strings[i+1], LOpB, LTargetB, LSourceB);
    if SameText(LOpA, 'set') and SameText(LOpA, LOpB) and SameText(LTargetA, LSourceB) then
    begin
      ALines.Strings[i] := '';
      ALines.Strings[i+1] := LOpB + ' ' + LTargetB + ', ' + LSourceA;
    end;
  end;
  RemoveEmptyLines(ALines);
end;

procedure OptimizeMovNopMov(ALines: TStrings);
var
  LOpA, LOpB, LTargetA, LTargetB, LSourceA, LSourceB: string;
  i, k: Integer;
  LSkipNext: Boolean;
begin
  for i := 0 to ALines.Count - 1 do
  begin
    if LSkipNext then
    begin
      LSkipNext := False;
      continue;
    end;
    SplitLine(ALines.Strings[i], LOpA, LTargetA,LSourceA);
    if StartsText('if', LOpA) then
    begin
      LSkipNext := True;
      continue;
    end;
    if SameText(LOpA, 'set') then
    begin
      for k := i+1 to ALines.Count-1 do
      begin
        SplitLine(ALines.Strings[k], LOpB, LTargetB, LSourceB);
        if  SameText(LSourceA, LTargetB) then
        begin
          Break;
        end;
        if SameText(LTargetA, LSourceB)then
        begin
          ALines.Strings[i] := '';
          ALines.Strings[k] := LOpB + ' ' + LTargetB + ', ' + LSourceA;
          Break;
        end;
        if not SameText(LOpB, 'set') then
        begin
          Break;
        end;
      end;
    end;
  end;
  RemoveEmptyLines(ALines);
end;

procedure OptimizeMovNopIf(ALines: TStrings);
var
  LOpA, LOpB, LTargetA, LTargetB, LSourceA, LSourceB: string;
  i, k: Integer;
  LSkipNext: Boolean;
begin
  for i := 0 to ALines.Count - 1 do
  begin
    if LSkipNext then
    begin
      LSkipNext := False;
      continue;
    end;
    SplitLine(ALines.Strings[i], LOpA, LTargetA,LSourceA);
    if StartsText('if', LOpA) then
    begin
      LSkipNext := True;
      continue;
    end;
    if SameText(LOpA, 'set') then
    begin
      for k := i+1 to ALines.Count-1 do
      begin
        SplitLine(ALines.Strings[k], LOpB, LTargetB, LSourceB);
        if SameText(LSourceA, LTargetB) or
          ((not SameText(LOpB, 'set')) and SameText(LTargetA, LTargetB) and (not StartsText('if', LOpB))) then
        begin
          Break;
        end;
        if StartsText('if', LOpB) then
        begin
          if SameText(LTargetA, LSourceB)then
          begin
            ALines.Strings[i] := '';
            ALines.Strings[k] := LOpB + ' ' + LTargetB + ', ' + LSourceA;
            Break;
          end;
          if SameText(LTargetA, LTargetB)then
          begin
            ALines.Strings[i] := '';
            ALines.Strings[k] := LOpB + ' ' + LSourceA + ', ' + LSourceB;
            Break;
          end;
          Break;
        end;
      end;
    end;
  end;
  RemoveEmptyLines(ALines);
end;

function OptimizeDCPUCode(AIn: string): string;
var
  LLines: TStringList;
begin
  LLines := TStringList.Create();
  LLines.Text := AIn;
  OptimizePushPop(LLines);
  OptimizePushNopPop(LLines);
  OptimizeMoveMove(LLines);
  ////OptimizeMoveOpMove(LLines);//added as last
  OptimizeMoveOP(LLines);
  OptimizeMovNopMov(LLines);
  OptimizeMovNopIf(LLines);
  OptimizeMoveOpMove(LLines);
  Result := LLines.Text;
  LLines.Free;
end;

end.
