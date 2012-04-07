unit CodeElement;

interface

uses
  Classes, Types;

type
  TCodeElement = class
  private
    FName: string;
  published
  public
    constructor Create(AName: string);
    property Name: string read FName;
  end;

  TCodeElementClass = class of TCodeElement;

implementation

{ TCodeElement }

constructor TCodeElement.Create(AName: string);
begin
  FName := AName;
end;

end.
