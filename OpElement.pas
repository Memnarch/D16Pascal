unit OpElement;

interface

uses
  Classes, Types, SysUtils, Generics.Collections, CodeElement, Operation, WriterIntf;

type
  TOpElement = class(TCodeElement)
  private
    FOperations: TObjectList<TOperation>;
    FModifiers: TObjectList<TCodeElement>;
    FModifierMax: TList<Integer>;
  protected
    procedure GetModifierSource(AWriter: IWriter);
    procedure GetModifierAdressOffset(AWriter: IWriter);
    function GetMultiplierForArrayModifier(AIndex: Integer): Integer;
  public
    constructor Create();
    destructor Destroy(); override;
    property Operations: TObjectList<TOperation> read FOperations;
    property Modifiers: TObjectList<TCodeElement> read FModifiers;
    property ModifierMax: TList<Integer> read FModifierMax;
  end;

implementation

{ TOpElement }

constructor TOpElement.Create;
begin
  inherited Create('');
  FOperations := TObjectList<TOperation>.Create(False);
  FModifiers := TObjectList<TCodeElement>.Create();
  FModifierMax := TList<Integer>.Create();
end;

destructor TOpElement.Destroy;
begin
  FOperations.Free;
  FModifiers.Free;
  FModifierMax.Free;
  inherited;
end;

procedure TOpElement.GetModifierAdressOffset(AWriter: IWriter);
var
  LElement: TCodeElement;
  i: Integer;
begin
  for i := 0 to Modifiers.Count-1 do
  begin
    LElement := Modifiers.Items[i];
    LElement.GetDCPUSource(AWriter);
    if (Modifiers.Count > 0) then
    begin
      if i < Self.Modifiers.Count-1 then
      begin
        AWriter.Write('set x, pop');
        AWriter.Write('mul x, ' + IntToStr(GetMultiplierForArrayModifier(i)));
        AWriter.Write('set push, x');
      end;
    end;
    if i > 0 then
    begin
      AWriter.Write('set y, pop');
      AWriter.Write('set x, pop');
      AWriter.Write('add x, y');
      AWriter.Write('set push, x');
    end;
  end;
end;

procedure TOpElement.GetModifierSource(AWriter: IWriter);
var
  LElement: TCodeElement;
  i: Integer;
begin
  GetModifierAdressOffset(AWriter);
  if Modifiers.Count > 0 then  //this will add the adress to the calculated offset
  begin
    AWriter.Write('set y, pop');
    AWriter.Write('set x, pop');
    AWriter.Write('add y, x');
    AWriter.Write('set push, [y]');
  end;
end;

function TOpElement.GetMultiplierForArrayModifier(AIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := 1;
  for i := AIndex + 1 to Modifiers.Count - 1 do
  begin
    Result := Result * ModifierMax.Items[i];
  end;
end;

end.
