unit Sys;
var
	GRed: Word = 0x4000;

procedure Print(AText: Word);
var
	LChar: Word;
	LVid: Word;
begin
	LVid := 0x8000;
	while AText^ > 0 do
	begin
		LChar := GRed + AText^;
		LVid^ := LChar;
		LVid := LVid + 1;
		AText := AText + 1;
	end;
end;

procedure Halt();
asm
:halt set pc, halt;
end;

end.