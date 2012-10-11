unit D16Assembler;

interface

uses
  Classes, Types, SysUtils, Generics.Collections, Lexer, OpCode, SiAuto, SmartInspect;

type
  TD16Ram = array[0..$FFFE] of Word;

  TParameter = class
  private
    FLabelName: string;
    FValue: Word;
    FRegCode: Byte;
    FHasValue: Boolean;
    function GetHasLabel: Boolean;
  public
    constructor Create();
    procedure Reset();
    property RegCode: Byte read FRegCode write FRegCode;
    property Value: Word read FValue write FValue;
    property HasValue: Boolean read FHasValue write FHasValue;
    property LabelName: string read FLabelName write FLabelName;
    property HasLabel: Boolean read GetHasLabel;
  end;

  TD16Assembler = class
  private
    FMemory: TD16Ram;
    FLexer: TLexer;
    FPC: Word;
    FOpCodes: TObjectlist<TOpCode>;
    FAdressStack: TStringList;
    FRelocTable: TStringList;
    FLabels: TStringList;
    FUseBigEdian: Boolean;
    FHexDumpPath: string;
    procedure ParseLabel();
    procedure ParseDat();
    procedure ParseOp();
    procedure ParseComment();
    procedure InitOpcodes();
    procedure ParseParameter(AParam: TParameter; AIsFirst: Boolean = True);
    function GetRegisterCode(ALeft, ARight: string; AIsPointer: Boolean; AIsFirst: Boolean = True): Integer;
    function GetOpCode(AName: string): TOpCode;
    function IsRegister(AName: string): Boolean;
    function LabelExists(AName: string): Boolean;
    procedure ReplaceAllLabels(ASilent: Boolean = False);
    procedure SwapEndianForAll();
    procedure HexDump(AFile: string);
    procedure AddAdressToRelocTable(AWord: Word);
  public
    constructor Create();
    destructor Destroy(); override;
    procedure AssembleSource(ASource: string);
    procedure AssembleFile(ASource: string);
    procedure WriteWord(AWord: Word);
    procedure ToStream(AStream: TStream);
    procedure WriteRelocationTableToStream(AStream: TStream);
    procedure SaveTo(AFile: string);
    procedure SaveAsModuleTo(AFile: string);
    procedure RegisterLabel(AName: string);
    procedure PushAdressForLabel(AAdress: Word; ALabel: string);
    function GetAdressForLabel(ALabel: string): Word;
    property Memory: TD16Ram read FMemory;
    property Lexer: TLexer read FLexer;
    property PC: Word read FPC;
    property OpCodes: TObjectlist<TOpCode> read FOpCodes;
    property AdressStack: TStringList read FAdressStack;
    property Labels: TStringList read FLabels;
    property UseBigEdian: Boolean read FUseBigEdian write FUseBigEdian;
    property HexDumpPath: string read FHexDumpPath write FHexDumpPath;
  end;

implementation

uses
  Token, StrUtils;

type
  TWordRec = record
    Low: Byte;
    High: Byte;
  end;

function ChangeEndian16(X: WORD): WORD; register; //oder auch Swap
asm
  xchg AL, AH
end;

{ TD16Assembler }

procedure TD16Assembler.AddAdressToRelocTable(AWord: Word);
begin
  FRelocTable.Add(IntToStr(AWord));
end;

procedure TD16Assembler.AssembleFile(ASource: string);
var
  LText: TStringList;
begin
  LText := TStringList.Create();
  FHexDumpPath := ChangeFileExt(ASource, '.hex');
  LText.LoadFromFile(ASource);
  AssembleSource(LText.Text);
  LText.Free;
end;

procedure TD16Assembler.AssembleSource(ASource: string);
begin
  FLexer.LoadFromString(ASource);
  FAdressStack.Clear;
  FLabels.Clear;
  FPC := 0;
  while not FLexer.EOF do
  begin
    if FLexer.PeekToken.IsContent(':') then
    begin
      ParseLabel();
    end
    else
    begin
      if FLexer.PeekToken.IsContent('dat') then
      begin
        ParseDat();
      end
      else
      begin
        if FLexer.PeekToken.IsContent(';') then
        begin
          ParseComment();
        end
        else
        begin
          ParseOp();
        end;
      end;
    end;
  end;
  ReplaceAllLabels();
  HexDump(FHexDumpPath);
  if UseBigEdian then
  begin
    SwapEndianForAll();
  end;
end;

constructor TD16Assembler.Create;
begin
  FLexer := TLexer.Create;
  FLexer.SimpleTokensOnly := True;
  FOpCodes := TObjectList<TOpCode>.Create();
  FAdressStack := TStringList.Create();
  FLabels := TStringList.Create();
  FRelocTable := TStringList.Create();
  FUseBigEdian := False;
  InitOpcodes();
end;

destructor TD16Assembler.Destroy;
begin
  FLexer.Free;
  FOpCodes.Free;
  FLabels.Free;
  FAdressStack.Free;
  FRelocTable.Free;
  inherited;
end;

function TD16Assembler.GetAdressForLabel(ALabel: string): Word;
begin
  AddAdressToRelocTable(FPC);
  Result := StrToInt(FLabels.Values[ALabel]);
end;

function TD16Assembler.GetOpCode(AName: string): TOpCode;
var
  LOpCode: TOpCode;
begin
  Result := nil;
  for LOpCode in FOpCodes do
  begin
    if SameText(LOpCode.Name, AName) then
    begin
      Result := LOpCode;
      Break;
    end;
  end;
  if not Assigned(Result) then
  begin
    raise EAbort.Create('Unknown opcode Identifier ' + QuotedStr(AName));
  end;
end;

function TD16Assembler.GetRegisterCode(ALeft, ARight: string;
  AIsPointer: Boolean; AIsFirst: Boolean = True): Integer;
var
  LRightCode, LVal: Integer;
begin
  LRightCode := -1;
  Result := AnsiIndexText(ALeft, ['a', 'b', 'c', 'x', 'y', 'z','i', 'j']);
  if ARight <> '' then
  begin
    LRightCode := GetRegisterCode(ARight, '', False);
  end;
  if (Result >= 0 ) and AIsPointer and (LRightCode < 0) then
  begin
    Result := Result + 8; //[register]
  end;

  if Result < 0 then
  begin
    if SameText(ALeft, 'pop') and (not AIsFirst) then Result := $18;
    if SameText(ALeft, 'push') and AIsFirst then Result := $18;
    if SameText(ALeft, 'peek') then Result := $19;
    if SameText(ALeft, 'sp')then
    begin
      if(LRightCode = $1f) then
      begin
        Result := $1a;
        LRightCode := -1;
      end
      else
      begin
        if LRightCode < 0 then Result := $1b
      end;
    end;

    if SameText(ALeft, 'pc') then Result := $1c;
    if SameText(ALeft, 'ex') then Result := $1d;
  end;

  if (LRightCode >= 0) then
  begin
    Result := $10 + Result;
    if (LRightCode <> $1f) or (Result > $17) then
    begin
      raise Exception.Create('Invalid combination [' + QuotedStr(ALeft) + ' + ' + QuotedStr(ARight) + ']');
    end;
  end;

  if Result < 0 then
  begin
    if AIsPointer then
    begin
      Result := $1e;//[next word]
    end
    else
    begin
      Result := $1f; // next word
    end;
    if TryStrToInt(ALeft, LVal) and (not AIsFirst) then
    begin
      if (LVal >= -1) and (LVal <= 30)  then
      begin
        Result := $20 + LVal+1;
      end;
    end;
  end;

  if Result < 0 then
  begin
    raise Exception.Create('can not get regcode for ' + QuotedStr(ALeft));
  end;
end;

procedure TD16Assembler.HexDump;
var
  LList: TStringList;
  i: Integer;
begin
  if Trim(AFile) = '' then
  begin
    Exit;
  end;
  LLIst := TStringList.Create();
  for i := 0 to FPC-1 do
  begin
    LList.Add('dat 0x' + IntToHex(FMemory[i], 4));
  end;
  LList.SaveToFile(AFile);
  LLIst.Free;
end;

procedure TD16Assembler.InitOpcodes;
begin
  // the 1.7 table
  FOpCodes.Add(TOpCode.Create('set', $1, 2));
  FOpCodes.Add(TOpCode.Create('add', $2, 2));
  FOpCodes.Add(TOpCode.Create('sub', $3, 2));
  FOpCodes.Add(TOpCode.Create('mul', $4, 2));
  FOpCodes.Add(TOpCode.Create('mli', $5, 2));
  FOpCodes.Add(TOpCode.Create('div', $6, 2));
  FOpCodes.Add(TOpCode.Create('dvi', $7, 2));
  FOpCodes.Add(TOpCode.Create('mod', $8, 2));
  FOpCodes.Add(TOpCode.Create('mdi', $9, 2));
  FOpCodes.Add(TOpCode.Create('and', $a, 2));
  FOpCodes.Add(TOpCode.Create('bor', $b, 2));
  FOpCodes.Add(TOpCode.Create('xor', $c, 2));
  FOpCodes.Add(TOpCode.Create('shr', $d, 2));
  FOpCodes.Add(TOpCode.Create('asr', $e, 2));
  FOpCodes.Add(TOpCode.Create('shl', $f, 2));
  FOpCodes.Add(TOpCode.Create('ifb', $10, 2));
  FOpCodes.Add(TOpCode.Create('ifc', $11, 2));
  FOpCodes.Add(TOpCode.Create('ife', $12, 2));
  FOpCodes.Add(TOpCode.Create('ifn', $13, 2));
  FOpCodes.Add(TOpCode.Create('ifg', $14, 2));
  FOpCodes.Add(TOpCode.Create('ifa', $15, 2));
  FOpCodes.Add(TOpCode.Create('ifl', $16, 2));
  FOpCodes.Add(TOpCode.Create('ifu', $17, 2));

  FOpCodes.Add(TOpCode.Create('adx', $1a, 2));
  FOpCodes.Add(TOpCode.Create('sbx', $1b, 2));

  FOpCodes.Add(TOpCode.Create('sti', $1e, 2));
  FOpCodes.Add(TOpCode.Create('std', $1f, 2));

  //non basic opcodes
  FOpCodes.Add(TOpCode.Create('jsr', $1, 1, False));

  FOpCodes.Add(TOpCode.Create('hcf', $7, 1, False)); //undocumented but still working

  FOpCodes.Add(TOpCode.Create('int', $8, 1, False));
  FOpCodes.Add(TOpCode.Create('iag', $9, 1, False));
  FOpCodes.Add(TOpCode.Create('ias', $a, 1, False));
  FOpCodes.Add(TOpCode.Create('rfi', $b, 1, False));
  FOpCodes.Add(TOpCode.Create('iaq', $c, 1, False));

  FOpCodes.Add(TOpCode.Create('hwn', $10, 1, False));
  FOpCodes.Add(TOpCode.Create('hwq', $11, 1, False));
  FOpCodes.Add(TOpCode.Create('hwi', $12, 1, False));
end;

function TD16Assembler.IsRegister(AName: string): Boolean;
begin
  Result := AnsiIndexText(AName, ['a', 'b', 'c', 'x', 'y', 'z','i', 'j']) >= 0;
  Result := Result or (AnsiIndexText(AName, ['push', 'pop', 'peek', 'sp', 'pc', 'ex','i', 'j']) >= 0);
end;

function TD16Assembler.LabelExists(AName: string): Boolean;
begin
  Result := FLabels.IndexOfName(AName) >= 0;
end;

procedure TD16Assembler.ParseComment;
begin
  while (not FLexer.EOF) and (not FLexer.PeekToken.FollowedByNewLine) do
  begin
    FLexer.GetToken();
  end;
  if not FLexer.EOF then
  begin
    FLexer.GetToken();
  end;
end;

procedure TD16Assembler.ParseDat;
var
  LRepeat: Boolean;
  LText: AnsiString;
  i: Integer;
  LWord: word;
begin
  FLexer.GetToken('dat');
  LRepeat := True;
  while LRepeat and (not FLexer.PeekToken.IsContent(';')) and (not FLexer.PeekToken.IsType(ttEOF)) do
  begin
    LRepeat := not FLexer.PeekToken.FollowedByNewLine;
    if FLexer.PeekToken.IsType(ttCharLiteral) then
    begin
      LText := FLexer.GetToken('', ttCharLiteral).Content;
      for i := 1 to Length(LText) do
      begin
        LWord := Word(LText[i]);
        WriteWord(LWord);
      end;
    end
    else
    begin
      if FLexer.PeekToken.IsType(ttIdentifier) then
      begin
        if LabelExists(FLexer.PeekToken.Content) then
        begin
          WriteWord(GetAdressForLabel(FLexer.GetToken('', ttIdentifier).Content));
        end
        else
        begin
          PushAdressForLabel(FPC, FLexer.GetToken('', ttIdentifier).Content);
          WriteWord($fcfc);
        end;
      end
      else
      begin
        WriteWord(StrToInt(FLexer.GetToken('', ttNumber).Content));
      end;
    end;

    if FLexer.PeekToken.IsContent(',') then
    begin
      FLexer.GetToken(',');
    end;
  end;
end;

procedure TD16Assembler.ParseLabel;
begin
  FLexer.GetToken(':');
  RegisterLabel(FLexer.GetToken('', ttIdentifier).Content);
end;

procedure TD16Assembler.ParseOp;
var
  LOp: TOpCode;
  LParamA, LParamB: TParameter;
  LFullOpCode: Word;
begin
  LParamA := TParameter.Create();
  LParamB := TParameter.Create();
  LOp := GetOpCode(FLexer.GetToken('', ttIdentifier).Content);
  if FLexer.PeekToken.IsContent(',') then
  begin
    FLexer.GetToken(','); // who had the idea to place commas AFTER a fucking mnemonic? WHO?!
  end;
  ParseParameter(LParamA);
  if LOP.ArgCount = 2 then
  begin
    if FLexer.PeekToken.IsContent(',') then
    begin
      FLexer.GetToken(',');
    end;
    ParseParameter(LParamB, False);
  end;
  if LOp.IsBasic then
  begin
    LFullOpCode := LOp.Value + (LParamB.FRegCode shl 10) + (LParamA.FRegCode shl 5);
  end
  else
  begin
    LFullOpCode := (LOp.Value shl 5) + (LParamA.FRegCode shl 10);
  end;
  WriteWord(LFullOpCode);




  if LParamB.HasLabel then
  begin
    if not LabelExists(LParamB.LabelName) then
    begin
      PushAdressForLabel(FPC, LParamB.LabelName);
    end
    else
    begin
      LParamB.Value := GetAdressForLabel(LParamB.LabelName);
    end;
  end;
  if (LParamB.FRegCode = $1e) or (LParamB.FRegCode = $1f) or (LParamB.FRegCode = $1a)
    or ((LParamB.FRegCode >= $10) and (LParamB.FRegCode <= $17) ) then
  begin
    WriteWord(LParamB.Value);
  end;

  if LParamA.HasLabel then
  begin
    if not LabelExists(LParamA.LabelName) then
    begin
      PushAdressForLabel(FPC, LParamA.LabelName);
    end
    else
    begin
      LParamA.Value := GetAdressForLabel(LParamA.LabelName);
    end;
  end;
  if (LParamA.FRegCode = $1e) or (LParamA.FRegCode = $1f) or (LParamA.FRegCode = $1a)
    or ((LParamA.FRegCode >= $10) and (LParamA.FRegCode <= $17) ) then
  begin
    WriteWord(LParamA.Value);
  end;

  LParamA.Free;
  LParamB.Free;
end;

procedure TD16Assembler.ParseParameter(AParam: TParameter; AIsFirst: Boolean = True);
var
  LLeftSide, LRightSide: string;
  LIsPointer: Boolean;
begin
  AParam.Reset();
  LLeftSide := '';
  LRightSide := '';
  LIsPointer := False;
  if FLexer.PeekToken.IsContent('[') then
  begin
    LIsPointer := True;
    FLexer.GetToken('[');
  end;
  if FLexer.PeekToken.IsType(ttNumber) then
  begin
    AParam.FValue := StrToInt(FLexer.GetToken().Content);
    AParam.HasValue := True;
    LLeftSide := IntToStr(AParam.FValue);
  end
  else
  begin
    LLeftSide := FLexer.GetToken('', ttIdentifier).Content;
    if LIsPointer and FLexer.PeekToken.IsContent('+') then
    begin
      FLexer.GetToken('+');
      AParam.HasValue := True;
      LRightSide := FLexer.GetToken('', ttNumber).Content;
      AParam.FValue := StrToInt(LRightSide);
    end;
  end;

  AParam.FRegCode := GetRegisterCode(LLeftSide, LRightSide, LIsPointer, AIsFirst);
  if not IsRegister(LLeftSide) and (not AParam.HasValue) then
  begin
    AParam.LabelName := LLeftSide;
  end;
  if LIsPointer then
  begin
    FLexer.GetToken(']');
  end;
end;

procedure TD16Assembler.PushAdressForLabel(AAdress: Word; ALabel: string);
begin
  FAdressStack.Add(IntToStr(AAdress) + '=' + ALabel);
  AddAdressToRelocTable(AAdress);
end;

procedure TD16Assembler.RegisterLabel(AName: string);
var
  LWord: Word;
begin
  LWord := FPC;
//  if SameText(ANAme, 'generate_board') then
//  begin
//  end;
//  if UseBigEdian then
//  begin
//    LWord := ChangeEndian16(LWord);
//  end;
//  if FLabels.IndexOfName(AName) < 0 then
//  begin
    FLabels.Add(AName + '=' + IntToStr(LWord));
//  end
//  else
//  begin
//    FLabels.Values[AName] := IntToStr(LWord);
//  end;
  ReplaceAllLabels(True);
end;

procedure TD16Assembler.ReplaceAllLabels;
var
  i: Integer;
  LAddress, LValue: Word;
begin
  for i := FAdressStack.Count - 1 downto 0  do
  begin
    if (not ASilent) or LabelExists(FAdressStack.Names[i]) then
    begin
      LAddress := StrToInt(FAdressStack.Names[i]);
      LValue := GetAdressForLabel(FAdressStack.ValueFromIndex[i]);
      FMemory[LAddress] := LValue;
      FAdressStack.Delete(i);
    end;
  end;
end;

procedure TD16Assembler.SaveAsModuleTo(AFile: string);
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create();
  WriteRelocationTableToStream(LStream);
  ToStream(LStream);
  LStream.SaveToFile(AFile);
  LStream.Free;
end;

procedure TD16Assembler.SaveTo(AFile: string);
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create();
  ToStream(LStream);
  LStream.SaveToFile(AFile);
  LStream.Free;
end;

procedure TD16Assembler.SwapEndianForAll;
var
  i: Integer;
begin
  for i := 0 to FPC - 1 do
  begin
    FMemory[i] := ChangeEndian16(FMemory[i]);
  end;
end;

procedure TD16Assembler.ToStream(AStream: TStream);
begin
  AStream.Write(FMemory, FPC*2);
end;

procedure TD16Assembler.WriteRelocationTableToStream(AStream: TStream);
var
  LWord: Word;
  i: Integer;
begin
  LWord := FRelocTable.Count;
  if UseBigEdian then
  begin
    LWord := ChangeEndian16(LWord);
  end;
  AStream.Write(LWord, 2);
  for i := 0 to FRelocTable.Count - 1 do
  begin
    LWord := StrToInt(FRelocTable.Strings[i]);
    if UseBigEdian then
    begin
      LWord := ChangeEndian16(LWord);
    end;
    AStream.Write(LWord, 2);
  end;
  LWord := FPC;
  if UseBigEdian then
  begin
    LWord := ChangeEndian16(LWord);
  end;
  AStream.Write(LWord, 2);
end;

procedure TD16Assembler.WriteWord(AWord: Word);
begin
  FMemory[FPC] := AWord;
  Inc(FPC);
end;

{ TParameter }

constructor TParameter.Create;
begin
  Reset();
end;

function TParameter.GetHasLabel: Boolean;
begin
  Result := Trim(FLabelName) <> '';
end;

procedure TParameter.Reset;
begin
  RegCode := 0;
  FLabelName := '';
  FValue := 0;
  FHasValue := False;
end;

end.
