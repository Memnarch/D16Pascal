unit Memory;

procedure MemCopy(AFromPtr, AToPtr, ALength: Word);
asm
	set i, SP
	set SP, A
	set x, a
	add x, c
	:MemCopy_Loop
	 set [b], pop
	 add b, 1
	ifg x, SP
	set pc, MemCopy_Loop
	set SP, i
end;

end.