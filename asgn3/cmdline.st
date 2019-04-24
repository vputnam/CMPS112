#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: cmdline.st,v 1.8 2011-05-05 20:45:21-07 - - $"
""
"Parse command line arguments"
""

execname := '$RCSfile: cmdline.st,v $' =~ '.*: (.*),v.*' at: 1.
nl := Character nl.

stdout << 'execname: ' << execname << nl.
stdout << 'version: ' << Smalltalk version << nl.
stdout << 'hostSystem: ' << Smalltalk hostSystem << nl.
stdout << 'arguments: ' << Smalltalk arguments << nl.
"
(1 to: Smalltalk arguments size)
do: [:index |
    'argument[' display.
    index display.
    '] = "' display.
    (Smalltalk arguments at: index) display.
    '"' displayNl.
].
"

(1 to: Smalltalk getArgc)
do: [:argi |
    |infile fileName aStream|

   ((Smalltalk getArgv: argi) = '-d') ifTrue: [
         '-d True' displayNl.
         (Smalltalk getArgv: argi+1) displayNl.
   ].
   ((Smalltalk getArgv: argi) = '-t') ifTrue: [
         '-t True' displayNl.
    ].
   ((Smalltalk getArgv: argi) = '-c') ifTrue: [
         '-c True' displayNl.
         fileName := FileStream open: (Smalltalk getArgv: argi+2)
                              mode: FileStream write.
         fileName << echo << 'hello my name is'.
         fileName close.
    ].
   ((Smalltalk getArgv: argi) = '-u') ifTrue: [
        '-u True' displayNl.
        (Smalltalk getArgv: argi+1) displayNl.
        (Smalltalk getArgv: argi+2) displayNl.
    ].
].




"
Smalltalk arguments: '-a -b --foo --bar'
do: [:opt :arg |
   'arguments:do: ' display.
   opt display.
   ' ' display.
   arg displayNl.
]
ifError: [
   'Error: invalid option' displayNl.
   '--foo'
]
"
'$RCSfile: cmdline.st,v $' display.
'finished.' displayNl.

"TEST: cmdline.st foo bar baz"

