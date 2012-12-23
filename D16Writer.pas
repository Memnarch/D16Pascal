unit D16Writer;

interface

uses
  Classes, Types, Generics.Collections, UncountedInterfacedObject, LineMapping, VarMapping, WriterIntf,
  VarDeclaration, CodeELement, RoutineMapping, ProcDeclaration;

type
  TD16Writer = class(TUncountedInterfacedObject, IWriter)
  private
    FLineMapping: TObjectList<TLineMapping>;
    FSource: TStringList;
    FCurrentUnit: string;
    procedure AddRoutineMapping(AProc: TProcDeclaration; AOffset: Integer =0);
    procedure AddVarMapping(AVar: TVarDeclaration; AOffset: Integer = 0);
    procedure AddLineMapping(AElement: TCodeElement; AOffset: Integer = 0);
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Write(ALine: string);
    procedure WriteList(AList: TStrings);
    procedure AddMapping(AElement: TObject; AOffset: Integer = 0; AHideName: Boolean = False);
    function GetMappingByASMLine(ALine: Integer): TLineMapping;
    property LineMapping: TObjectList<TLineMapping> read FLineMapping;
    property Source: TStringList read FSource;
    property CurrentUnit: string read FCurrentUnit write FCurrentUnit;
  end;

implementation

{ TD16Writer }

procedure TD16Writer.AddLineMapping(AElement: TCodeElement; AOffset: Integer);
var
  LMapping: TLineMapping;
begin
  LMapping := TLineMapping.Create();
  try
    LMapping.UnitLine := AElement.Line + AOffset;
    LMapping.ASMLine := FSource.Count;
    LMapping.D16UnitName := FCurrentUnit;
  finally
    FLineMapping.Add(LMapping);
  end;
end;

procedure TD16Writer.AddMapping(AElement: TObject; AOffset: Integer;
  AHideName: Boolean);
begin
  if AElement is TVarDeclaration then
  begin
    AddVarMapping(TVarDeclaration(AElement), AOffset);
  end
  else
  begin
    if (AElement is TProcDeclaration) and (not AHideName) then
    begin
      AddRoutineMapping(TProcDeclaration(AElement), AOffset);
    end
    else
    begin
      AddLineMapping(TCodeElement(AElement), AOffset);
    end;
  end;
end;

procedure TD16Writer.AddRoutineMapping(AProc: TProcDeclaration; AOffset: Integer = 0);
var
  LMapping: TRoutineMapping;
  LParameter: TParameter;
  LELement: TCodeElement;
  LVar: TVarDeclaration;
begin
  LMapping := TRoutineMapping.Create();
  try
    LMapping.UnitLine := AProc.Line + AOffset;
    LMapping.ASMLine := FSource.Count;
    LMapping.D16UnitName := FCurrentUnit;
    LMapping.ElementName := AProc.Name;
    if AProc.IsFunction then
    begin
      LMapping.TypeName := AProc.ResultType.Name;
    end
    else
    begin
      LMapping.TypeName := '';
    end;
    for LElement in AProc.Parameters do
    begin
      LVar := TVarDeclaration(LELement);
      LParameter := TParameter.Create();
      try
        LParameter.Name := LVar.Name;
        LParameter.TypeName := LVar.DataType.Name;
        LParameter.Access := LVar.GetAccessIdentifier;
      finally
        LMapping.Parameters.Add(LParameter);
      end;
    end;

    for LElement in AProc.Locals do
    begin
      LVar := TVarDeclaration(LELement);
      LParameter := TParameter.Create();
      try
        LParameter.Name := LVar.Name;
        LParameter.TypeName := LVar.DataType.Name;
        LParameter.Access := LVar.GetAccessIdentifier;
      finally
        LMapping.Locals.Add(LParameter);
      end;
    end;
  finally
    FLineMapping.Add(LMapping);
  end;
end;

procedure TD16Writer.AddVarMapping(AVar: TVarDeclaration; AOffset: Integer);
var
  LMapping: TVarMapping;
begin
  LMapping := TVarMapping.Create();
  try
    LMapping.UnitLine := AVar.Line + AOffset;
    LMapping.ASMLine := FSource.Count;
    LMapping.D16UnitName := FCurrentUnit;
    LMapping.ElementName := AVar.Name;
    LMapping.TypeName := AVar.DataType.Name;
  finally
    FLineMapping.Add(LMapping);
  end;
end;

constructor TD16Writer.Create;
begin
  FSource := TStringList.Create();
  FLineMapping := TObjectList<TLineMapping>.Create();
end;

destructor TD16Writer.Destroy;
begin
  FSource.Free();
  FLineMapping.Free();
  inherited;
end;

function TD16Writer.GetMappingByASMLine(ALine: Integer): TLineMapping;
var
  LMapping: TLineMapping;
begin
  Result := nil;
  for LMapping in FLineMapping do
  begin
    if LMapping.ASMLine = ALine then
    begin
      Result := LMapping;
      Break;
    end;
  end;
end;

procedure TD16Writer.Write(ALine: string);
begin
  FSource.Add(ALine);
end;

procedure TD16Writer.WriteList(AList: TStrings);
begin
  FSource.AddStrings(AList);
end;

end.
