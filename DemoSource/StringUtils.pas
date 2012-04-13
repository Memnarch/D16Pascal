unit StringUtils;


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

end.