unit Sys;

procedure Halt();
asm
	set pc, halt;
end;

end.