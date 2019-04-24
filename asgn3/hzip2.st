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

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [ ^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

Character extend [
   isGraph [
      ^ (Character space < self) & (self <= $~)
   ]
   visible [
      self isGraph ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 16]
   ]
]

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]


Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

]



Leaf subclass: Tree [
   |left right|

   "Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]"

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   "actually JK THIS TOTALLY WORKS BECAUSE OF HOW OUR TREE
IS SET UP YEEE
"
   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]



]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result

   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index[
      (self at: index) isNil
         ifTrue: [self at: index put: 0].
      self at: index put: (self at: index) + 1.
      "stdout << 'hereiam'."
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

FileStream extend [
   nl := Character nl.
   freqtable := ZeroArray new: 257.
   priority := SortedCollection new.
   entable := SortedCollection sortBlock: [:a :b | a char < b char].

   tOption [
      |count|
      count := 0.
      "freqtable := ZeroArray new: 256."
      [self atEnd not] whileTrue: [
         "FileStream fileIn 'hzcharfreq.st' self nextLine."
         "count := count + 1.
         count asString size to: 5 do: [:skip| stdout << ' ']."
         "stdout << count << ': ' << self nextLine << nl.
         [self nextLine next lf not] whileTrue: [
            stdout << count << nl.
         ]"
         self nextLine do: [:char |
            "stdout << 'current char: '<<  char << nl."
            |ordChar|
            ordChar := char asInteger.
            freqtable incr: ordChar.
         ].
         freqtable incr: nl asInteger.
         
      ].
      
      "im sure there's a better way but here ya go end of file
       one repercussion is it prints really gross so we can 
       either change that or fix this"
      freqtable incr: 256.

      "how tf do i display the fucking freak table"


      "freqtable keysAndValuesDo: [:key :value |
         (value notNil and: [value > 0]) ifTrue: [
           stdout << 'freq[' << key << ' (' << key asCharacter visible << ') ]=' << value
                   << nl.
         ] 
      ]."


      "want the smallest values to be most easily accessible so we can
       grab lowest frequencies first to put at bottom of tree"

      "make each nonempty spot in the freq table a node (leaf?)"
      
      freqtable keysAndValuesDo: [: key :value |
         (value notNil and: [value > 0]) ifTrue: [
            |node|
            node := Leaf new: key count: value.
            "put each node in the priority queue"
            priority add: node. "; inspect."
            "stdout << 'freq[' << key << ' (' << key asCharacter visible << ') ]=' << value << nl.
            "
         ]
      ].    


      "until priority queue is empty (only one tree left)"
      "start removing the two smallest elements (at the top for us)"
      "put in tree"
   
      [priority size >= 2] whileTrue: [
         |lnode rnode newTree| "smallest small parent"
         lnode := priority removeFirst.
         rnode := priority removeFirst.
         newTree := Tree new: (lnode char) 
                              count: (lnode count) + (rnode count) 
                              left: lnode
                              right: rnode.
         priority add: newTree." ; inspect."
      ].

      "now check all nonempty thingis in the freqtable and MAKE
       THOSE ENCODINGS!!!!!!!!"
      "these are now in order based on char value"
      priority first inorder: [:char :string |
         |node|
         node := Leaf new: char count: string.
         entable add: node ; inspect.
         "stdout << char asCharacter visible << '     ' << 'freq' << '     ' <<  string << nl.
         "
      ] prefix: ''.

      
      [entable size > 0] whileTrue: [
         |entry entfreq chfreq|
         entry := entable removeFirst.
         entfreq := freqtable at: (entry char).

 
         (entry char = nl asInteger) "newline case"
            ifTrue: [
               stdout << '   ' << 'x0A' << '     ' << entfreq << ' ' << entry count << nl.
            ] 
            ifFalse: [
               (entry char = 256) "EOF case"
                  ifTrue: [
                     stdout << '   ' << 'EOF' << '     ' << entfreq << ' ' << entry count << nl.
                  ]
                  ifFalse: [
                     (entry char = ' ' asInteger) "space case"
                     ifTrue: [
                        stdout << '   ' << 'x20' << '     ' << entfreq << ' ' << entry count << nl.
                     ]
                     ifFalse: [ "anything else"
                        stdout << '   ' << entry char asCharacter << '     ' << entfreq << ' ' << entry count << nl.

                     ]
                  ]
            ]
         
      ]
   ]

   cOption [
      |count|
      count := 0.
      [self atEnd not] whileTrue: [
         self nextLine do: [:char |
            |ordChar|
            ordChar := char asInteger.
            freqtable incr: ordChar.
         ].
         freqtable incr: nl asInteger.
         
      ].
      
      "im sure there's a better way but here ya go end of file
       one repercussion is it prints really gross so we can 
       either change that or fix this"
      freqtable incr: 256.

      "want the smallest values to be most easily accessible so we can
       grab lowest frequencies first to put at bottom of tree"

      "make each nonempty spot in the freq table a node (leaf?)"
      
      freqtable keysAndValuesDo: [: key :value |
         (value notNil and: [value > 0]) ifTrue: [
            |node|
            node := Leaf new: key count: value.
            "put each node in the priority queue"
            priority add: node. "; inspect."
            "stdout << 'freq[' << key << ' (' << key asCharacter visible << ') ]=' << value << nl.
            "
         ]
      ].    


      "until priority queue is empty (only one tree left)"
      "start removing the two smallest elements (at the top for us)"
      "put in tree"
   
      [priority size >= 2] whileTrue: [
         |lnode rnode newTree| "smallest small parent"
         lnode := priority removeFirst.
         rnode := priority removeFirst.
         newTree := Tree new: (lnode char) 
                              count: (lnode count) + (rnode count) 
                              left: lnode
                              right: rnode.
         priority add: newTree." ; inspect."
      ].

      "now check all nonempty thingis in the freqtable and MAKE
       THOSE ENCODINGS!!!!!!!!"
      "these are now in order based on char value"
      priority first inorder: [:char :string |
         |node|
         node := Leaf new: char count: string.
         entable add: node ; inspect.
         "stdout << char asCharacter visible << '     ' << 'freq' << '     ' <<  string << nl
         "
      ] prefix: ''.

      
 
   ]
]


Smalltalk arguments size = 1
   ifTrue: [
      stderr << 'BAD USAGE' << nl.
   ]



(1 to: Smalltalk getArgc)
do: [:argi |
    |infile fileName aStream|

   ((Smalltalk getArgv: argi) = '-d') ifTrue: [
         '-d True' displayNl.
         (Smalltalk getArgv: argi+1) displayNl.
   ].
   ((Smalltalk getArgv: argi) = '-t') ifTrue: [
         '-t True' displayNl.
         infile := FileStream open: (Smalltalk getArgv: argi+1)
                              mode: FileStream read.
         infile tOption.
         infile close.
    ].
   ((Smalltalk getArgv: argi) = '-c') ifTrue: [
         '-c True' displayNl.
         "fileName := FileStream open: (Smalltalk getArgv: argi+2)
                              mode: FileStream write.
         fileName << echo << 'hello my name is'.
         
         filename cOption.
         fileName close."
         infile := FileStream open: (Smalltalk getArgv: argi+1)
                              mode: FileStream read.
         infile cOption.
         infile close.


    ].
   ((Smalltalk getArgv: argi) = '-u') ifTrue: [
        '-u True' displayNl.
        (Smalltalk getArgv: argi+1) displayNl.
        (Smalltalk getArgv: argi+2) displayNl.
    ].
].

'$RCSfile: cmdline.st,v $' display.
'finished.' displayNl.

"TEST: cmdline.st foo bar baz"

