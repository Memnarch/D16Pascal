unit Sys;
var
	TextColor: Word = 0xF000;
	TextCursor: Word = 0x0;
	LineWidth: Word = 32;
	KeyOffset: Word = 0;
	True: Word = 0xFFFF;
	False: Word = 0;
	
procedure InsertLineBreak();
begin
	TextCursor := TextCursor + (LineWidth - TextCursor mod LineWidth);
end;

procedure MemCopy(AFromPtr, AToPtr, ALength: Word);
asm
	set i, SP
	set SP, A
	set x, a
	add x, c
	:MemCopy_Loop
	 set [b], [SP++]
	 add b, 1
	ifg x, SP
	set pc, MemCopy_Loop
	set SP, i
end;

function StringLength(AStr: Word): Word;
asm
	set b, a
	set a, 0
	:StringLength_Loop
	ife [b], 0
	set pc, StringLength_End
	add b, 1
	add a, 1
	set pc, StringLength_Loop
	:StringLength_End
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
	LVid^ := AChar;
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