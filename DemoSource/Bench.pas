unit Bench;

var
	GRed: Word = 0x4000;

procedure Print(AText: string);
var
	LVid: Word;
begin
	LVid := 0x8000;
	while Word(AText)^ > 0 do
	begin
		LVid^ := GRed + Word(AText)^;
		LVid := LVid + 1;
		Word(AText) := Word(AText) + 1;
	end;
end;

begin
	Print('Hello world!');
	asm
		:halt 
		set pc, halt
	end;
end.