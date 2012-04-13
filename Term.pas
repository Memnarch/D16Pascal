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
  SysUtils, CodeElement;

{ TTerm }

function TTerm.GetDCPUSource: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to SubElements.Count - 1 do
  begin
    Result := Result + SubElements.Items[i].GetDCPUSource();
    if i > 0 then
    begin
      Result := Result + 'set y, pop' + sLineBreak;
      Result := Result + 'set x, pop' + sLineBreak;
//      case Operators.Strings[i-1][1] of
//        '*':
//        begin
//          LOp := 'mul';
//        end;
//        '/':
//        begin
//          LOp := 'div';
//        end;
//        'a':
//        begin
//          LOp := 'and';
//        end;
//        'm':
//        begin
//          LOp := 'mod';
//        end;
//        's':
//        begin
//          if SameText(Operators.Strings[i-1], 'shl') then
//          begin
//            LOp := 'shl';
//          end;
//          if SameText(Operators.Strings[i-1], 'shr') then
//          begin
//            LOp := 'shr';
//          end;
//        end;
//      end;
      Result := Result + Operations.Items[i-1].GetAssembly(['x', 'y']); //LOp + ' x, y' + sLineBreak;
      Result := Result + 'set push, x' + sLineBreak;
    end;
  end;
end;

end.
