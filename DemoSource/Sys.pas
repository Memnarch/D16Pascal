unit Sys;
var
	TextColor: Word = 0xF000;
	TextCursor: Word = 0x0;
	LineWidth: Word = 32;
	
procedure InsertLineBreak();
begin
	TextCursor := TextCursor + (LineWidth - TextCursor mod LineWidth);
end;

procedure Print(AText: Word);
var
	LChar: Word;
	LVid: Word;
begin
	LVid := 0x8000 + TextCursor;
	while AText^ > 0 do
	begin
		if AText^ = 10 then
		begin
			InsertLineBreak();
			LVid := 0x8000 + TextCursor;
		end
		else
		begin
			LChar := TextColor + AText^;
			LVid^ := LChar;
			LVid := LVid + 1;
			TextCursor := TextCursor + 1;
		end;
		AText := AText + 1;
	end;
end;



procedure PrintLn(AText: Word);
begin
	Print(AText);
	InsertLineBreak();
end;

procedure CLS();
asm
	set i, 0
	:loop
	set [0x8000 + i], 0
	add i, 1
	ifg 0x0200, i
	set pc, loop
	set [TextCursor], 0
end;

procedure Halt();
asm
	set pc, halt;
end;

end.