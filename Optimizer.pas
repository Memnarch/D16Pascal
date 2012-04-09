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

function OptimizeDCPUCode(AIn: string): string;
var
  LLines: TStringList;
begin
  LLines := TStringList.Create();
  LLines.Text := AIn;
  OptimizePushPop(LLines);
  Result := LLines.Text;
  LLines.Free;
end;

end.
