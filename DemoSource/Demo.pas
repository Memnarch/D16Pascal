unit Demo;

uses
	Sys;
	
var
	msgHello: Word = 'Hello World';
	msgMulti: Word = 'Multiline\nText';
	msgKey: Word = 'Key Test:';
	PFrom: Word = 0x8000;
	PTo: Word = 0x8040;
	
begin
	PrintLn(msgKey);
	PrintHex(0xF23c);
	while true do
	begin
		PrintChar(GetKey());
	end;
	CLS();
	Halt();
end.