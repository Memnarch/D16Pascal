unit Relation;

interface

uses
  Classes, Types, OpElement;

type
  TRelation = class(TOpElement)
  public
    function GetDCPUSOurce(): string; override;
  end;

implementation

uses
  CodeElement, StrUtils;

{ TRelation }

function TRelation.GetDCPUSOurce: string;
var
  LElement: TCodeElement;
  i: Integer;
  LOp, LOpStr: string;
begin
  Result := '';
  for i := 0 to SubElements.Count - 1 do
  begin
    Result := Result + SubElements.Items[i].GetDCPUSource();
    if i > 0 then
    begin
      Result := Result + 'set y, pop' + sLineBreak;
      Result := Result + 'set z, pop' + sLineBreak;
      Result := Result + 'set x, 0' + sLineBreak;
      LOpStr := Operators.Strings[i-1];
      case AnsiIndexText(LOpStr, ['>', '<', '>=', '<=', '=', '<>']) of
        0:
        begin
          LOp := 'ifg y, z';
        end;
        1:
        begin
          LOp := 'ifg z, y';
        end;
        2:
        begin
          LOp := 'ifg y, z' + sLineBreak;
          LOp := LOp + 'set x, 1' + sLineBreak;
          LOp := LOp + 'ife y, z' + sLineBreak;
        end;
        3:
        begin
          LOp := 'ifg z, y' + sLineBreak;
          LOp := LOp + 'set x, 1' + sLineBreak;
          LOp := LOp + 'ife y, z' + sLineBreak;
        end;
        4:
        begin
          LOp := 'ife y, z';
        end;
        5:
        begin
          LOp := 'ifn y, z';
        end;
      end;
      Result := Result + LOp + sLineBreak;
      Result := Result + 'set x, 1' + sLineBreak;
      Result := Result + 'set push, x' + sLineBreak;
    end;
  end;
end;

end.
