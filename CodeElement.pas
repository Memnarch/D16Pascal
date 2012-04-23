unit CodeElement;

interface

uses
  Classes, Types, Generics.Collections;

type

  TCodeElementClass = class of TCodeElement;

  TCodeElement = class
  private
    FName: string;
    FSubElements: TObjectList<TCodeElement>;
    FLine: Integer;
  public
    constructor Create(AName: string);
    function GetElement(AName: string; AType: TCodeElementClass): TCodeElement;
    function GetUniqueID(APrefix: string = ''): string;
    function GetDCPUSource(): string; virtual;
    property Name: string read FName write FName;
    property SubElements: TObjectList<TCodeElement> read FSubElements;
    property Line: Integer read FLine write FLine;
  end;


implementation

uses
  SysUtils;

var
  GID: Integer = 0;

{ TCodeElement }

constructor TCodeElement.Create(AName: string);
begin
  FName := AName;
  FLine := -1;
  FSubElements := TObjectList<TCodeElement>.Create();
end;

function TCodeElement.GetDCPUSource: string;
var
  LElement: TCodeElement;
begin
  Result := '';
  for LElement in FSubElements do
  begin
    Result := Result + LElement.GetDCPUSource();
  end;
end;

function TCodeElement.GetElement(AName: string;
  AType: TCodeElementClass): TCodeElement;
var
  LElement: TCodeElement;
begin
  Result := nil;
  for LElement in FSubElements do
  begin
    if SameText(AName, LElement.Name) and LElement.InheritsFrom(AType) then
    begin
      Result := LElement;
      Break;
    end;
  end;
end;

function TCodeElement.GetUniqueID(APrefix: string = ''): string;
begin
  Result := APrefix+IntToHex(GID, 4);
  Inc(GID);
end;

end.
