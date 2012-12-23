unit RoutineMapping;

interface

uses
  Classes, Types, SysUtils, Generics.Collections, VarMapping;

type
  TParameter = class
  private
    FName: string;
    FTypeName: string;
    FAccess: string;
  public
    property Name: string read FName write FName;
    property TypeName: string read FTypeName write FTypeName;
    property Access: string read FAccess write FAccess;
  end;

  TRoutineMapping = class(TVarMapping)
  private
    FParameters: TObjectList<TParameter>;
    FLocals: TObjectList<TParameter>;
    procedure ReadParametersFromLine(ALine: string);
    procedure ReadLocalsFromLine(ALine: string);
  protected
    function GetText(): string; override;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure ReadFromLine(ALine: string); override;
    property Parameters: TObjectList<TParameter> read FParameters;
    property Locals: TObjectList<TParameter> read FLocals;
  end;

implementation

uses
  StrUtils;

{ TRoutineMapping }

constructor TRoutineMapping.Create;
begin
  inherited;
  FIsPossibleBreakPoint := True;
  FParameters := TObjectList<TParameter>.Create();
  FLocals := TObjectList<TParameter>.Create();
end;

destructor TRoutineMapping.Destroy;
begin
  FParameters.Free;
  FLocals.Free;
  inherited;
end;

function TRoutineMapping.GetText: string;
var
  LParameter: TParameter;
  i: Integer;
begin
  Result := inherited + '%';
  for i := 0 to FParameters.Count - 1 do
  begin
    LParameter := FParameters.Items[i];
    Result := Result + LParameter.Name + ',' + LParameter.TypeName + ',' + LParameter.Access;
    if i < FParameters.Count - 1 then
    begin
      Result := Result + '#';
    end;
  end;
  Result := Result + '%';
  for i := 0 to FLocals.Count - 1 do
  begin
    LParameter := FLocals.Items[i];
    Result := Result + LParameter.Name + ',' + LParameter.TypeName + ',' + LParameter.Access;
    if i < FLocals.Count - 1 then
    begin
      Result := Result + '#';
    end;
  end;
end;

procedure TRoutineMapping.ReadFromLine(ALine: string);
var
  LLines: TStringDynArray;
begin
  LLines := SplitString(ALine, '%');
  if Length(LLines) = 3 then
  begin
    inherited ReadFromLine(LLines[0]);
    ReadParametersFromLine(LLines[1]);
    ReadLocalsFromLine(LLines[2]);
  end
  else
  begin
    raise Exception.Create('Expected 3 Toplevel elements in TRoutineMapping but found ' + IntToStr(Length(LLines)));
  end;
end;

procedure TRoutineMapping.ReadLocalsFromLine(ALine: string);
var
  LLines, LData: TStringDynArray;
  LLine: string;
  LParameter: TParameter;
begin
  LLines := SplitString(ALine, '#');
  if Length(LLines) > 0 then
  begin
    for LLine in LLines do
    begin
      LParameter := TParameter.Create();
      try
        LData := SplitString(LLine, ',');
        if Length(LData) = 3 then
        begin
          LParameter.Name := LData[0];
          LParameter.TypeName := LData[1];
          LParameter.Access := LData[2];
        end
        else
        begin
          raise Exception.Create('Expected 3 elements for reading TParameter as Local but found ' + IntToStr(Length(LData)));
        end;
      finally
        FLocals.Add(LParameter);
      end;
    end;
  end;
end;

procedure TRoutineMapping.ReadParametersFromLine(ALine: string);
var
  LLines, LData: TStringDynArray;
  LLine: string;
  LParameter: TParameter;
begin
  LLines := SplitString(ALine, '#');
  if Length(LLines) > 0 then
  begin
    for LLine in LLines do
    begin
      LParameter := TParameter.Create();
      try
        LData := SplitString(LLine, ',');
        if Length(LData) = 3 then
        begin
          LParameter.Name := LData[0];
          LParameter.TypeName := LData[1];
          LParameter.Access := LData[2];
        end
        else
        begin
          raise Exception.Create('Expected 3 elements for reading TParameter as Parameter but found ' + IntToStr(Length(LData)));
        end;
      finally
        FParameters.Add(LParameter);
      end;
    end;
  end;
end;

end.
