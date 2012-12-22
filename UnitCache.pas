unit UnitCache;

interface

uses
  Classes, Types, SysUtils, Generics.Collections, PascalUnit;

type
  TUnitCache = class(TObjectList<TPascalUnit>)
  public
    function GetUnitByName(AName: string): TPascalUnit;
    function CountUnitName(AName: string): Integer;
    procedure ClearUnitCache(AName: string);
  end;

implementation

{ TUnitCache }

procedure TUnitCache.ClearUnitCache(AName: string);
var
  LUnit: TPascalUnit;
begin
  LUnit := GetUnitByName(AName);
  while Assigned(LUnit) do
  begin
    Delete(IndexOf(LUnit));
    LUnit := GetUnitByName(AName);
  end;
end;

function TUnitCache.CountUnitName(AName: string): Integer;
var
  LUnit: TPascalUnit;
begin
  Result := 0;
  for LUnit in Self do
  begin
    if SameText(AName, LUnit.Name) then
    begin
      Inc(Result);
    end;
  end;
end;

function TUnitCache.GetUnitByName(AName: string): TPascalUnit;
var
  LUnit: TPascalUnit;
begin
  Result := nil;
  for LUnit in Self do
  begin
    if SameText(LUnit.Name, AName) then
    begin
      Result := LUnit;
    end;
  end;
end;

end.
