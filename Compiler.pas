unit Compiler;

interface

uses
  Classes, Types, Generics.Collections, SysUtils, D16Parser, LineMapping, CompilerDefines, PascalUnit;

type
  TCompiler = class(TObject)
  private
    FParser: TD16Parser;
    function GetErrors: Integer;
    function GetFatals: Integer;
    function GetHints: Integer;
    function GetLineMapping: TObjectList<TLineMapping>;
    function GetOnMessage: TOnMessage;
    function GetPeekMode: Boolean;
    function GetSearchPath: TStringlist;
    function GetUnits: TObjectList<TPascalUnit>;
    function GetWarning: Integer;
    procedure SetOnMessage(const Value: TOnMessage);
    procedure SetPeekMode(const Value: Boolean);
  public
    constructor Create();
    destructor Destroy(); override;
    procedure CompileFile(AFile: string);
    procedure CompilerSource(ASource: string; AAsProgram: Boolean);
//    procedure Initialize();
    procedure Reset();
    function PeekCompile(ASource, AUnitName: string; AAsProgram: Boolean; var AUnit: TPascalUnit): Boolean;
    function GetDCPUSource(): string;
    function GetMappingByASMLine(ALine: Integer): TLineMapping;
    function GetUnitByName(AName: string): TPascalUnit;
    property SearchPath: TStringlist read GetSearchPath;
    property OnMessage: TOnMessage read GetOnMessage write SetOnMessage;
    property Errors: Integer read GetErrors;
    property Warnings: Integer read GetWarning;
    property Fatals: Integer read GetFatals;
    property Hints: Integer read GetHints;
    property PeekMode: Boolean read GetPeekMode write SetPeekMode;
    property Units: TObjectList<TPascalUnit> read GetUnits;
    property LineMapping: TObjectList<TLineMapping> read GetLineMapping;
  end;

implementation

uses
  StrUtils;

{ TCompiler }

procedure TCompiler.CompileFile(AFile: string);
begin
  FParser.ParseFile(AFile);
end;

procedure TCompiler.CompilerSource(ASource: string; AAsProgram: Boolean);
begin
  FParser.ParseSource(ASource, AAsProgram);
end;


constructor TCompiler.Create;
begin
  inherited;
  FParser := TD16Parser.Create();
end;

destructor TCompiler.Destroy;
begin
  FParser.Free;
  inherited;
end;

function TCompiler.GetDCPUSource: string;
begin
  Result := FParser.GetDCPUSource();
end;

function TCompiler.GetErrors: Integer;
begin
  Result := FParser.Errors;
end;

function TCompiler.GetFatals: Integer;
begin
  Result := FParser.Fatals;
end;

function TCompiler.GetHints: Integer;
begin
  Result := FParser.Hints;
end;

function TCompiler.GetLineMapping: TObjectList<TLineMapping>;
begin
  Result := FParser.LineMapping;
end;

function TCompiler.GetMappingByASMLine(ALine: Integer): TLineMapping;
begin
  Result := FParser.GetMappingByASMLine(ALine);
end;

function TCompiler.GetOnMessage: TOnMessage;
begin
  Result := FParser.OnMessage;
end;

function TCompiler.GetPeekMode: Boolean;
begin
  Result := FParser.PeekMode;
end;

function TCompiler.GetSearchPath: TStringlist;
begin
  Result := FParser.SearchPath;
end;

function TCompiler.GetUnitByName(AName: string): TPascalUnit;
begin
  Result := FParser.GetUnitByName(AName);
end;

function TCompiler.GetUnits: TObjectList<TPascalUnit>;
begin
  Result := FParser.Units;
end;

function TCompiler.GetWarning: Integer;
begin
  Result := FParser.Warnings;
end;

function TCompiler.PeekCompile(ASource, AUnitName: string; AAsProgram: Boolean; var AUnit: TPascalUnit): Boolean;
begin
  try
    FParser.ClearUnitCache(AUnitName);
    FParser.ParseSource(ASource, AAsProgram);
    AUnit := FParser.GetUnitByName(AUnitName);
    Result := Assigned(AUnit) and (FParser.Fatals = 0) and (FParser.Errors = 0);
  except
    Result := False;
  end;
end;


procedure TCompiler.Reset;
begin
  FParser.Reset();
end;


procedure TCompiler.SetOnMessage(const Value: TOnMessage);
begin
  FParser.OnMessage := Value;
end;

procedure TCompiler.SetPeekMode(const Value: Boolean);
begin
  FParser.PeekMode := Value;
end;

end.
