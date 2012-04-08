unit OpElement;

interface

uses
  Classes, Types, CodeElement;

type
  TOpElement = class(TCodeElement)
  private
    FOperators: TStringList;
  public
    constructor Create(); reintroduce;
    property Operators: TStringList read FOperators;
  end;

implementation

{ TOpElement }

constructor TOpElement.Create;
begin
  inherited Create('');
  FOperators := TStringList.Create();
end;

end.
