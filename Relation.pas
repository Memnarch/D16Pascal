unit Relation;

interface

uses
  Classes, Types, OpElement;

type
  TRelation = class(TOpElement)
  private
    FInverse: Boolean;
  public
    function GetDCPUSOurce(): string; override;
    property Inverse: Boolean read FInverse write FInverse;
  end;

implementation

uses
  CodeElement, StrUtils;

{ TRelation }

function TRelation.GetDCPUSOurce: string;
var
  LElement: TCodeElement;
  i: Integer;
  LOp, LOpStr, LTrue, LFalse: string;
begin
  Result := '';
  for i := 0 to SubElements.Count - 1 do
  begin
    Result := Result + SubElements.Items[i].GetDCPUSource();
    if i > 0 then
    begin
      LTrue := '0xffff';
      LFalse := '0';
      if (i = SubElements.Count - 1) and Inverse then
      begin
        LTrue := '0';
        LFalse := '0xffff';
      end;
      Result := Result + 'set z, pop' + sLineBreak;
      Result := Result + 'set y, pop' + sLineBreak;
      Result := Result + 'set x, ' + LFalse + sLineBreak;
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
          LOp := LOp + 'set x, ' + LTrue + sLineBreak;
          LOp := LOp + 'ife y, z' + sLineBreak;
        end;
        3:
        begin
          LOp := 'ifg z, y' + sLineBreak;
          LOp := LOp + 'set x, ' + LTrue + sLineBreak;
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
      Result := Result + 'set x, ' + LTrue + sLineBreak;
      Result := Result + 'set push, x' + sLineBreak;
    end;
  end;
  if (SubElements.Count = 1) and Inverse then
  begin
    Result := Result + 'set x, pop' + sLineBreak;
    Result := Result + 'xor x, 0xffff' + sLineBreak;
    Result := Result + 'set push, x' + sLineBreak;
  end;
end;

end.
