unit PascalUnit;

interface

uses
  Classes, Types, CodeElement;

type
  TPascalUnit = class(TCodeElement)
  public
    function GetDCPUSource(): string; override;
  end;

implementation

uses
  VarDeclaration;

{ TPascalUnit }

function TPascalUnit.GetDCPUSource: string;
var
  LElement: TCodeElement;
  LData: string;
begin
  Result := '';
  LData := '';
  for LElement in SubElements do
  begin
    if not (LElement is TVarDeclaration) then
    begin
      Result := Result + LElement.GetDCPUSource();
    end
    else
    begin
      LData := LData + LElement.GetDCPUSource();
    end;
  end;
  Result := Result + LData;
end;

end.
