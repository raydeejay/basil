# A BASIC interpreter

## Dialect specification:

The language is somewhat inspired in Sinclair BASIC.
**Some** of those internals are replicated/simulated.

## Available commands
#### REM
Does nothing. Used for comments.

#### INPUT
Reads a string from standard input.

#### INPUT#
Reads a number from standard input.

#### PRINT
Prints values. Use a comma to print a tab, and a semicolon to not
print a space.

#### LIST
Displays the program source.

#### NEW
Erases the program and variables.

#### CLEAR
Erases only the variables.

#### RUN [line]
Starts execution from the top, or at the specified line.

#### STOP
Causes the program to terminate.

#### GOTO <line>
Jump to the specified line.


#### EXIT
Exits the REPL.



## Code format
`<line number> <instr> [<arg> ...] [: <instr> [<arg> ...] ]`
Arguments can be constants, variables, or function calls.



### Interaction with the REPL
Entering a line of code will cause it to be executed immediately.
Preceeding a line of code with a number will add it to the program.
Entering a number by itself will delete the corresponding line if it exists.


### Data types
Strings
Numbers


### Remarks
British spelling is prefered.


### Planned features
Indices are 1-based.
Colour support
Graphics
Default charset with User Defined Graphics
Actual UDG
Sound
Printing
Connectivity
