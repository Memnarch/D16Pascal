unit Opcode;

interface

uses
  Classes, Types;

type
  TOpCode = class
  private
    FName: string;
    FValue: Byte;
    FArgCount: Byte;
    FIsBasic: Boolean;
  public
    constructor Create(AName: string; AValue, AArgCount: Byte; AIsBasic: Boolean = True);
    property Name: string read FName;
    property Value: Byte read FValue;
    property ArgCount: Byte read FArgCount;
    property IsBasic: Boolean read FIsBasic;
  end;

implementation

{ TOpCode }

constructor TOpCode.Create(AName: string; AValue, AArgCount: Byte; AIsBasic: Boolean = True);
begin
  FName := AName;
  FValue := AValue;
  FArgCount := AArgCount;
  FIsBasic := AIsBasic;
end;


end.
