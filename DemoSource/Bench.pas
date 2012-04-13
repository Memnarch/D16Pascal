unit Bench;

var
	GRed: Word = 0x4000;

procedure Print(AText: Word);
var
	LVid: Word;
begin
	LVid := 0x8000;
	while AText^ > 0 do
	begin
		LVid^ := GRed + AText^;
		LVid := LVid + 1;
		AText := AText + 1;
	end;
end;

begin
	Print('Hello world!');
	asm
		:halt 
		set pc, halt
	end;
end.