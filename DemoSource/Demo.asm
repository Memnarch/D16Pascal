               set push, a
               set a, str0000
               jsr Print
               set a, pop
               jsr Halt
:str0000 dat "Hello world!", 0x0
:Print
               set push, j
               sub sp, 2
               set j, sp
               set [j], 0x8000
:While0001
               set x, 0
               ifg [a], 0
               set x, 0xffff
               ife x, 0
               set pc, End0001
               set [1 + j], [GRed]
               add [1 + j], [a]
               set y, [j]
               set [y], [1 + j]
               add [j], 1
               add a, 1
               set pc, While0001
:End0001
               set sp, j
               add sp, 2
               set j, pop
               set pc, pop
:Halt
:halt set pc ,halt ;
               set pc, pop
:GRed dat 0x4000
