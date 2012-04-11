unit Demo;

uses
	Sys;
	
var
	msgHello: Word = 'Hello World';
	msgMulti: Word = 'Multiline\nText';

begin
	PrintLn(msgHello);
	PrintLn('SomeMulti\ninline text');
	PrintLn(msgHello);
	PrintLn(msgMulti);
	PrintLn(msgHello);
	CLS();
	Halt();
end.