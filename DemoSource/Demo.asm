               set push, a
               set a, msgHello
               jsr PrintLn
               set a, str0000
               jsr PrintLn
               set a, msgHello
               jsr PrintLn
               set a, msgMulti
               jsr PrintLn
               set a, msgHello
               jsr PrintLn
               set a, pop
               jsr CLS
               jsr Halt
:msgHello dat "Hello World",0x0
:msgMulti dat "Multiline",10,"Text",0x0
:str0000 dat "SomeMulti",10,"inline text", 0x0
:InsertLineBreak
               set y, [TextCursor]
               mod y, [LineWidth]
               set x, [LineWidth]
               sub x, y
               add [TextCursor], x
               set pc, pop
:Print
               set push, j
               sub sp, 2
               set j, sp
               set [j], 0x8000
               add [j], [TextCursor]
:While0001
               set x, 0
               ifg [a], 0
               set x, 0xffff
               ife x, 0
               set pc, End0001
               set x, 0
               ife [a], 10
               set x, 0xffff
               ife x, 0
               set pc, Else0002
               jsr InsertLineBreak
               set [j], 0x8000
               add [j], [TextCursor]
               set pc, End0002:Else0002
               set [1 + j], [TextColor]
               add [1 + j], [a]
               set y, [j]
               set [y], [1 + j]
               add [j], 1
               add [TextCursor], 1
:End0002
               add a, 1
               set pc, While0001
:End0001
               set sp, j
               add sp, 2
               set j, pop
               set pc, pop
:PrintLn
               set push, a
               jsr Print
               set a, pop
               jsr InsertLineBreak
               set pc, pop
:CLS
               set i ,0
:loop 
               set [0x8000+i ],0
               add i ,1
               ifg 0x0200,i 
               set pc ,loop 
               set [TextCursor ],0
               set pc, pop
:Halt
               set pc ,halt ;
               set pc, pop
:TextColor dat 0xF000
:TextCursor dat 0x0
:LineWidth dat 32
