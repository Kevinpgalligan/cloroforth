### Memory model
See: https://www.forth.com/starting-forth/9-forth-execution/#Forth_Geography
Also useful: https://forth-standard.org/standard/core/WORD

For now, treating the dictionary as an array of bytes, dictionary entries will be stored as raw bytes.

The (parameter) stack is a list. If it becomes necessary to manipulate the stack memory using addresses, I can always convert it to an array.

The return stack can be implicit, I would hope? YES, since it's generally controlled by using 'call' and 'rts' instructions. Also, there are no high-level Forth words that give you the addresses in the return stack.

TIB can be its own array maybe?

In the worst case, I use the same array for everything.

Okay, I guess I'm using the same array for everything.

I don't really get "user variables" (BASE, SP0). If they're not in the dictionary, then how do they get evaluated? Okay, unlike ordinary variables, their values are not kept in the body of the dictionary entry. Instead, the body contains an offset into the "user table" section of memory. When u execute the name of a user variable, the offset is added to the beginning address of the user table. Motivation is supposed to be that each "task" (thread? or what?) has its own user table, so can use the same definition of the variable but keep its own value.

The map of Forth (from low to high memory):

   pre-compiled Forth
   system vars (created by Forth core, used by entire system; not
                generally accessed by user)
   load definitions
   user dictionary (HERE points to the bottom of this)
   EMPTY SPACE
   PAD (some fixed offset from HERE)
   EMPTY SPACE
   parameter stack (grows up)
   TIB (grows down)
   return stack (grows up... aka doesn't exist)
   user variables
   buffers

Useful definition of run-time code: "a routine, compiled in memory, which specifies what happens when a member of a given class of words is executed. The run-time code for a colon definition is the address interpreter; the run-time code for a variable pushes the address of the variable’s body on the stack."

And of a task: "in Forth, a partition in memory that contains at minimum a parameter and a return stack and a set of user variables." (I guess I don't have to worry about it).

================

Memory-related words to think about:
* CP -- a variable that stores the next free memory address in the dictionary.
* HERE -- a word that stores the value of CP on the stack.
* ALLOT -- advances CP by the number of bytes given.
* ' -- returns "execution token" (address?) of word in dictionary. If not found, it executes ABORT" and prints an error message. Note: it's next word in the INPUT STREAM, not e.g. in a definition. Okay, the "execution token" is the code pointer!
* ['] -- same, but for the next word in the DEFINITION. Used only in colon definition.
* EXECUTE -- executes a definition given its "execution token" on the stack.
* INTERPRET -- uses some word to find a dictionary entry and return a zero flag if the word is found; if word found, executes it; otherwise, converts to number.
* DUMP -- takes memory address and # bytes and prints out those memory values.
* CELL -- the number of bytes in 1 cell.
* CELLS -- ( n1 -- n2 ) compute the amount of bytes taken up by a given number of cells (I think).
  I think this is basically the number of bytes in a "word" on the machine. That makes sense: addresses seem to be "CELLS" bytes long, so on a 32-bit machine CELLS=4 would give 32-bit addressing. The fields of dictionary entries (such as name) are padded so that they all align with word boundaries. For my program, I think I can set CELLS=1 'cause a function reference or any other type of reference or any number can be the element of an array.
* >BODY -- given a code pointer, returns the beginning of the data field. Does not work for colon definitions (why?), and may be forbidden for other types of dictionary entries. I think I can see why for constants -- maybe it just compiles the constant value directly into place?
* ! ( val address -- ) saves (single-length) value to memory address
* , saves a single-length value to the next available dictionary cell and advances CP by 1 cell.
* PAD -- returns the current address of the beginning of the pad (it's offset by a fixed amount from CP), which is a region in memory where text I/O stuff is done. It should be separated from the parameter stack by 100s of kilobytes.
* (NOT STANDARD) SP@ -- returns the address of the top of the stack location. Thus 'SP@ @' gives the value at the top of the stack (identical to DUP). And 'SP@ 4 CELLS + @' would copy the 5th one down.
* (NOT STANDARD) S0 -- address of next cell below the "empty stack" cell.
  "Notice that with double-length numbers, the high-order cell is stored at the lower memory address whether on the stack or in the dictionary."
  "The operators 2@ and 2! keep the order of the cells consistent."
  Note to self: so, the stack grows towards lower memory address and numbers are stored in little-endian order (when considering the direction the stack grows).
* TIB (USER VARIABLE) -- starting address of the "input message buffer" or "terminal input buffer", grows towards high memory. Text input gets stored here, where the text interpreter will scan it.
* #TIB (USER VARIABLE) -- size of contents of the terminal input buffer.
* BASE (USER VARIABLE) -- contains current number-conversion radix.
* H (USER VARIABLE) --  points to next available byte in dictionary (how different from CP?).
* >IN (USER VARIABLE) -- pointer to the current position parsed in the input stream.
* @ -- gets value at address.
* USER -- defines a user variable.
* TYPE ( addr u -- ) -- transmits u characters, beginning at address, to the output device.

Other tricky words to think about:
* EXIT -- "When compiled within a colon definition, terminates execution at that point."
  --> need some way to be able to exit control flow from a function.
* QUIT -- "Clears the return stack and returns control to the terminal. No message is given."
* IF / BEGIN / UNTIL ...
  --> these must somehow manipulate the control flow.

### Anatomy of a dictionary entry
- first bit -- precedence bit also indicates whether word should be executed during compilation or to be compiled into the definition.
- next 7 bits -- indicate length of the word name.
- next 'length' bytes -- the name.
- optional padding.
- link -- address of previous definition in the dictionary list.
- code pointer -- address of function that determines how to execute this word.
for variables, it pushes the address of the variable.
for constants, it pushes the content of the constant on the stack.
for colon definitions, it executes all the words in that colon definition.
for "machine-level" words, it refers to the machine code to run.
NOTE: this is the "execution token" supplied by tick and expected by EXECUTE.
- data field -- vars and constants it's a single cell. colon definition, it depends on the length of the definition. an array, as long as you want.

Colon definitions:
- The data field is a list of addresses of the words in the body.
- When executed, the definitions pointed to by the data field are executed one at a time.
- The address interpreter reads the list of execution tokens and executes the definitions they point to.
- The word ';' at the end of a definition compiles the xt of a word called EXIT, which terminates the execution of the address interpreter (and presumably returns execution control to the next level of the interpreter).
- One interpretation: address interpreter calls subroutines of all execution tokens in the list, with the return stack used to keep return addresses; EXIT works as the machines RTS (return from subroutine) instruction.

### Flow of execution
  - Call stack may look like: QUIT -> INTERPRET -> EXECUTE -> WORD, where
    WORD is a colon definition type of word.
  - QUIT: clear return stack, accept input, interpret, loop
  - Calling QUIT at any time will land you back in the REPL.
  - INTERPRET loops through the entire input stream; I guess it calls EXECUTE on each word in the input stream?
  - At the end of WORD's execution, EXECUTE calls the EXIT command, which I guess pops back up to interpret? I'm not sure, though. Wouldn't that become...
       QUIT -> INTERPRET -> EXECUTE -> WORD -> EXIT,
    so that we would end up back at WORD?
    Actually, it's:
       QUIT -> INTERPRET -> EXECUTE -> ADDRESS_INTERPRETER -> EXIT
    ...right? But that would only pop out to ADDRESS_INTERPRETER, would it
    not then try to read the next instruction?
    Perhaps, in x86, there's an instruction to pop off the return stack.
    Unless... it's using JMP rather than CALL to go between them.

So there are two interpreters: one executes the data (list of addresses, list of calls, machine code) found in the dictionary entry of a colon definition. Also handles "nesting of execution levels for words within words". This is the codeword / address of the "execution token" (i.e. the address) for the run-time code of this entry. So it would point to the address interpreter for colon-type words.

### What are the gaps?
- How does EXIT work? How about: each sub-function call is wrapped in handler-case or something, and we trigger a condition.
- Why does ' return the codeword of a token? Should it not return the address of the dictionary entry for that word? I think that must be what it does?

Might need an extra return stack, for tracking memory addresses in our big blob of memory.

### Roadmap
== Step 1: colon definitions.
First, DOCOL.
Next, : compiler:
  Read word from input stream, create dictionary header w/ that word.
  Write DOCOL to codeword field.
  Loop, implement

== Step 2: QUIT 
From Starting Forth:

: QUIT BEGIN
    (clear return stack)
    (accept input)
    INTERPRET
    ." ok " CR
  AGAIN

Maybe implement in low-level first, without loops.

- CLEAR (codeword to drop the return stack).
- Something to read a word into memory somewhere, from input stream.
- INTERPRET: Dictionary lookup. If not in dictionary, number conversion.
- EXECUTE (execute c/w of word address that is currently on stack).
- ." and " -- these can be colon words, I guess? With IMMEDIATE.

== Step 3: conditionals.
First do the primitives, 0branch or whatever.

### TODO
Here are all the words that I will need:
- word: uses key to read into internal buffer
- number: reads numbers in base BASE from a given address.
- find: looks up word in dictionary (as parsed by WORD)
- built-in variables: state (mode), latest (latest word), here (next place to write), S0 (base of parameter stack), base (number base)
- built-in constants: version, r0, docol, f_immed, f_hidden, f_lenmask
- cmove (block copy)?
- param stack words: dsp@, dsp!
- key: read next byte from stdin and push on stack (fills a buffer, also checks if stdin has closed and if so it exits the program)
- >CFA: turns dictionary pointer into codeword pointer
  So "WORD DOUBLE FIND >CFA" would end up with a pointer to the codeword
  address for DOUBLE.
- >DFA: same but for data field, first address after codeword.
- On INTERPRET: "FORTH has an INTERPRET function (a true interpreter this time, not DOCOL) which runs in a loop, reading words (using WORD), looking them up (using FIND), turning them into codeword pointers (using >CFA) and deciding what to do with them."
  Now, obviously what it does depends on the mode.
  If compile mode, writes to dict. If IMMEDIATE mode, calls EXECUTE.
- IMMEDIATE: toggles immediate flag of current word.
- HIDDEN: same for hidden flag. Called after dictionary header created and by semicolon. Used like 'LATEST @ HIDDEN'. Another form is 'HIDE WORDNAME'.
- CREATE: creates header of word in dictionary (see: HERE).
- ,: writes item to dictionary at HERE.
- [: switch to immediate mode
- ]: switch to compile mode
- we also need a way to set a word to HIDDEN, so that FIND skips it.
- ': returns cw pointer of next word.
- branching: BRANCH and 0BRANCH (see Jones)
- ." implemented in terms of LITSTRING and TELL (see Jones)
- QUIT: clear can actually be implemented in terms of RZ and RSPSTORE (return stack manipulating words). Then it calls INTERPRET and branches back to the start.
- CHAR: puts ascii code of first char of next word on stock.
- return stack and input buffer need space in memory.

Then... it's SELF-HOSTING!? I can define new words in Forth, I think?

ROADMAP:
- Definition of colon (works in the context of INTERPRET):
   read next word, create dictionary header, toggle hidden, toggle compile mode
- Definition of semicolon:
    toggle hidden, toggle compile mode
- Requirements: toggles, word, create
- Requirements for interpret: looping, word, 0branch, write to dict (',' word), execute
- Other things: I'll need proper variable/constant support.
  Somehow need to mark them.

(defword "name" "definition")
Bootstrapping involves reading all the words, setting input stream to point to those words (somehow), and compiling them by running Forth routines.

### Lower-priority words
- +! (addstore), -! (...)
