unit Tasks;

var
	OSTaskExitPtr: Word = 0;
	OSTaskStackPtr: Word = 0;
	OSTaskExitCode: Word = 0;

function RunTask(ATaskPtr: Word): Word;
asm
	set push, b
	set push, c
	set push, x
	set push, y
	set push, z
	set push, i
	set push, j
	set push, [OSTaskExitPtr]
	set push, [OSTaskStackPtr]
	set [OSTaskExitPtr], TaskExit
	set [OSTaskStackPtr], SP
	set pc, a
	:TaskExit
	set SP, [OSTaskStackPtr]
	set [OSTaskStackPtr], pop
	set [OSTaskExitPtr], pop
	set pop, j
	set pop, i
	set pop, z
	set pop, y
	set pop, x
	set pop, c
	set pop, b
	set a, [OSTaskExitCode]
end;

procedure ExitTask(ACode: Word);
asm
	set [OSTaskExitCode], a
	set pc, [OSTaskExitPtr]
end;

end.