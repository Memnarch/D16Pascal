unit Demo;

uses
	Types, Sys, ScreenIO;

type
	TTestArray = array[12] of array[32] of Word;
	PTestArray = ^TTestArray;
	
var
	msgHello: string = 'Hello World';
	msgMulti: string = 'Multiline\nText';
	msgKey: string = 'Key Test:';
	PTest: PTestArray = 0x8000;
	LVar: Word = 5;
	k, m: Word;
	
const
	IsFive: Word = 5;
	
begin
	CLS(0);
	PrintLn(msgKey);
	PrintHex(16);
	case LVar of
		1, 2:
		begin
			println('its 1 or 2');
		end;
		IsFive:
		begin
			println('its 5');
		end;
		
		else
		begin
			println('its something else');
		end;
	end;
	while not false do
	begin
		PrintChar(GetKey());
		if KeyOffset >= 511 then
		begin
			Cls(0);
		end;
	end;
	Halt();
end.