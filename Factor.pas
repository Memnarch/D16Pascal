unit Factor;

interface

uses
  Classes, Types, Generics.Collections,CodeElement, OpElement, VarDeclaration;

type
  TFactor = class(TOpElement)
  private
    FVarDeclaration: TVarDeclaration;
    FValue: string;
    FGetAdress: Boolean;
    FInverse: Boolean;
    FDereference: Boolean;
    FModiviers: TObjectList<TCodeElement>;
    FModifierMax: TList<Integer>;
    function GetPushValue(): string;
    function GetModifierSource(): string;
    function GetMultiplierForArrayModifier(AIndex: Integer): Integer;
  public
    constructor Create(); 
    destructor Destroy(); override;
    function IsConstant(): Boolean;
    function GetDCPUSource(): string; override;
    property VarDeclaration: TVarDeclaration read FVarDeclaration write FVarDeclaration;
    property Value: string read FValue write FValue;
    property GetAdress: Boolean read FGetAdress write FGetAdress;
    property Inverse: Boolean read FInverse write FInverse;
    property Dereference: Boolean read FDereference write FDereference;
    property Modifiers: TObjectList<TCodeElement> read FModiviers;
    property ModifierMax: TList<Integer> read FModifierMax;
  end;

implementation

uses
  StrUtils, SysUtils;

{ TFactor }

constructor TFactor.Create;
begin
  inherited;
  FModiviers := TObjectList<TCodeElement>.Create();
  FModifierMax := TList<Integer>.Create();
end;

destructor TFactor.Destroy;
begin
  FModiviers.Free;
  FModifierMax.Free;
  inherited;
end;

function TFactor.GetDCPUSource: string;
begin
  Result := '';
  if SubElements.Count > 0 then
  begin
    Result := inherited;
  end
  else
  begin
    Result := GetPushValue();
  end;
  if Dereference then
  begin
    Result := Result + 'set x, pop' + sLineBreak;
    Result := Result + 'set x, [x]' + sLineBreak;
    Result := Result + 'set push, x' + sLineBreak;
//    if not (LValToPush[1] = '[') then
//    begin
//      LValToPush := '[' + LValToPush + ']';
//    end;
//    Result := Result + 'set x, ' + LValToPush + sLineBreak;
//    if AnsiIndexText(LValToPush, ['[a]', '[b]', '[c]']) = -1 then
//    begin
//      Result := Result + 'set x, [x]' + sLineBreak;
//    end;
//    LValToPush := 'x';
  end;
  Result := Result + GetModifierSource();
  if Inverse then
  begin
    Result := Result + 'set x, pop' + sLineBreak;
    Result := Result + 'xor x, 0xffff' + sLineBreak;
    Result := Result + 'set push, x' + sLineBreak;
  end;
end;

function TFactor.GetModifierSource: string;
var
  LElement: TCodeElement;
  i: Integer;
begin
  Result := '';
  for i := 0 to FModiviers.Count-1 do
  begin
    LElement := FModiviers.Items[i];
    Result := Result + LElement.GetDCPUSource();
    if (FModiviers.Count > 0) then
    begin
      if i < Self.FModiviers.Count-1 then
      begin
        Result := Result + 'set x, pop' + sLineBreak;
        Result := Result + 'mul x, ' + IntToStr(GetMultiplierForArrayModifier(i)) + sLineBreak;
        Result := Result + 'set push, x' + sLineBreak;
      end;
    end;
    if i > 0 then
    begin
      Result := Result + 'set y, pop' + sLineBreak;
      Result := Result + 'set x, pop' + sLineBreak;
      Result := Result + 'add x, y' + sLineBreak;
      Result := Result + 'set push, x' + sLineBreak;
    end;
  end;
  if FModiviers.Count > 0 then  //this will add the adress to the calculated offset
  begin
    Result := Result + 'set x, pop' + sLineBreak;
    Result := Result + 'set y, pop' + sLineBreak;
    Result := Result + 'add x, y' + sLineBreak;
    Result := Result + 'set push, [x]' + sLineBreak;
  end;
end;

function TFactor.GetMultiplierForArrayModifier(AIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := 1;
  for i := AIndex + 1 to FModiviers.Count - 1 do
  begin
    Result := Result * FModifierMax.Items[i];
  end;
end;

function TFactor.GetPushValue: string;
var
  LValToPush: string;
  LParts: TStringDynArray;
begin
  if IsConstant then
  begin
    LValToPush := FValue;
  end
  else
  begin
    if FGetAdress or ((FVarDeclaration.ParamIndex > 0) and (FVarDeclaration.ParamIndex <= 3))
      or ((Length(FVarDeclaration.DefaultValue) > 0) and (FVarDeclaration.DefaultValue[1] = '"')) then
    begin
      if VarDeclaration.IsLocal then
      begin
        LParts := SplitString(FVarDeclaration.GetAccessIdentifier, '+');
        if Length(LParts) = 1 then
        begin
          LValToPush := Trim(LParts[0]);
        end
        else
        begin
          Result := Result + 'set x, ' + Trim(LParts[1]) + sLineBreak;
          Result := Result + 'add x, ' + Trim(LParts[0]) + sLineBreak;
          LValToPush := 'x';
        end;
      end
      else
      begin
        LValToPush := FVarDeclaration.GetAccessIdentifier();
      end;
      //Result := 'set push, ' +  + sLineBreak;
    end
    else
    begin
      //Result := 'set push, [' + FVarDeclaration.GetAccessIdentifier() + ']' + sLineBreak;
      LValToPush := '[' + FVarDeclaration.GetAccessIdentifier() + ']';
    end;
  end;
  Result := Result + 'set push, ' + LValToPush + sLineBreak;
end;

function TFactor.IsConstant: Boolean;
begin
  Result := not Assigned(FVarDeclaration);
end;

end.
