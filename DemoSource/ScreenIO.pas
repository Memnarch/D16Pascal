unit ScreenIO;

var
	TextColor: Word = 0xF000;
	TextCursor: Word = 0x0;
	LineWidth: Word = 32;
	KeyOffset: Word = 0;
	
procedure PrintLineBreak();
begin
	TextCursor := TextCursor + (LineWidth - TextCursor mod LineWidth);
end;

procedure Print(AText: string);
var
	LChar: Word;
	LVid: Word;
begin
	LVid := 0x8000 + TextCursor;
	while Pointer(AText)^ > 0 do
	begin
		if Pointer(AText)^ = 10 then
		begin
			PrintLineBreak();
			LVid := 0x8000 + TextCursor;
		end
		else
		begin
			LChar := TextColor + Pointer(AText)^;
			LVid^ := LChar;
			LVid := LVid + 1;
			TextCursor := TextCursor + 1;
		end;
		Word(AText) := Word(AText) + 1;
	end;
end;

procedure PrintLn(AText: string);
begin
	Print(AText);
	PrintLineBreak();
end;

function GetKey(): Word;
asm
	set b, [KeyOffset]
	add b, 0x9000
	:wait
	ife [b], 0
	set pc, wait
	set a, [b]
	set [b], 0
	add [KeyOffset], 1
	and [KeyOffset], 0xf
end;

procedure PrintChar(AChar: Word);
var
	LVid: Word;
begin
	LVid := 0x8000 + TextCursor;
	LVid^ := AChar + TextColor;
	TextCursor := TextCursor + 1;
end;

procedure PrintNumber(ANumber: Word);
asm
	set x, 0
	:IntToStrLoop
	set b, a
	mod b, 10
	add b, 48
	set push, b
	div a, 10
	add x, 1
	ifn a, 0
	set pc, IntToStrLoop
	
	set y, 0
	:PrintNumberLoop
	set a, pop
	set push, x
	set push, y
	jsr PrintChar
	set y, pop
	set x, pop
	add y, 1
	ifg x, y
	set pc, PrintNumberLoop
end;

procedure PrintHex(ANumber: Word);
asm
	set x, 0
	:IntToStrLoop
	set b, a
	mod b, 16
	ifg b, 9
	set pc, HexChar
	add b, 48
	set pc, DoPush
	:HexChar
	add b, 55
	:DoPush
	set push, b
	div a, 16
	add x, 1
	ifn a, 0
	set pc, IntToStrLoop
	
	set y, 0
	:PrintNumberLoop
	set a, pop
	set push, x
	set push, y
	jsr PrintChar
	set y, pop
	set x, pop
	add y, 1
	ifg x, y
	set pc, PrintNumberLoop
end;

procedure CLS(AColor: Word);
asm
	set i, 0x8000
	:loop
	set [i], a
	add i, 1
	ifg 0x8180, i
	set pc, loop
	set [TextCursor], 0
end;



end.