unit Expression;

interface

uses
  Classes, Types, OpElement;

type
  TExpression = class(TOpElement)
  public
    function GetDCPUSource(): string; override;
  end;

implementation

uses
  SysUtils, CodeElement;

{ TExpression }

function TExpression.GetDCPUSOurce: string;
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
//        '+':
//        begin
//          LOp := 'add';
//        end;
//
//        '-':
//        begin
//          LOp := 'sub';
//        end;
//
//        'o':
//        begin
//          if SameText(Operators.Strings[i-1], 'or') then
//          begin
//            LOp := 'bor';
//          end;
//          if SameText(Operators.Strings[i-1], 'xor') then
//          begin
//            LOp := 'xor';
//          end;
//        end;
//      end;
      Result := Result + Operations.Items[i-1].GetAssembly(['x', 'y']);
      Result := Result + 'set push, x' + sLineBreak;
    end;
  end;
end;

end.
