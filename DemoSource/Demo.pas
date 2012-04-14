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
	
begin
	CLS(0);
	PrintLn(msgKey);
	PrintHex(0xF23c);
	PrintChar(PTest[1][2] and 0xFF);
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