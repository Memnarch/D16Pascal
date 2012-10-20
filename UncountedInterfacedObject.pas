unit UncountedInterfacedObject;

interface

uses
  Classes, Types;

type
  TUncountedInterfacedObject = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

implementation

{ TUncountedInterfacedObject }

function TUncountedInterfacedObject.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TUncountedInterfacedObject._AddRef: Integer;
begin
  Result := -1;//signal we are not counting
end;

function TUncountedInterfacedObject._Release: Integer;
begin
  Result := -1;//signal we are not counting
end;

end.
