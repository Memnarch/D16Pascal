unit OpElement;

interface

uses
  Classes, Types, Generics.Collections, CodeElement, Operation;

type
  TOpElement = class(TCodeElement)
  private
    FOperations: TObjectList<TOperation>;
  public
    constructor Create();
    property Operations: TObjectList<TOperation> read FOperations;
  end;

implementation

{ TOpElement }

constructor TOpElement.Create;
begin
  inherited Create('');
  FOperations := TObjectList<TOperation>.Create(False);
end;

end.
