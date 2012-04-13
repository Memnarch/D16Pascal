unit Demo;

uses
	Types, Sys, ScreenIO;

type
	TTestArray = array[10] of Word;
	PTestArray = ^TTestArray;
	
var
	msgHello: string = 'Hello World';
	msgMulti: string = 'Multiline\nText';
	msgKey: string = 'Key Test:';
	PTest: TTestArray;
	LVar: Word = 5;
	
begin
	CLS(0);
	PrintLn(msgKey);
	PrintHex(0xF23c);
	while true do
	begin
		PrintChar(GetKey());
		if KeyOffset >= 511 then
		begin
			Cls(0);
		end;
	end;
	Halt();
end.