unit Term;

interface

uses
  Classes, Types, OpElement;

type
  TTerm = class(TOpElement)
  public
    function GetDCPUSource(): string; override;
  end;

implementation

uses
  CodeElement;

{ TTerm }

function TTerm.GetDCPUSource: string;
var
  LElement: TCodeElement;
  i: Integer;
  LOp: string;
begin
  Result := '';
  for i := 0 to SubElements.Count - 1 do
  begin
    Result := Result + SubElements.Items[i].GetDCPUSource();
    if i > 0 then
    begin
      Result := Result + 'set x, pop' + sLineBreak;
      Result := Result + 'set y, pop' + sLineBreak;
      case Operators.Strings[i-1][1] of
        '*':
        begin
          LOp := 'mul';
        end;
        '/':
        begin
          LOp := 'div';
        end;
        'a':
        begin
          LOp := 'and';
        end;
      end;
      Result := Result + LOp + ' x, y' + sLineBreak;
      Result := Result + 'set push, x' + sLineBreak;
    end;
  end;
end;

end.
