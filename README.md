# A BASIC interpreter

## Dialect specification:

The language is somewhat inspired in Sinclair BASIC.
**Some** of those internals are replicated/simulated.

## Available commands
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

#### EXIT
Exits the REPL.


### Features
Lines are numbered.
Code RUN is executed sequentially starting at line 0 by default.

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
Lines can contain more than one statement, separated by colons (:).
Indices are 1-based.
Colour support
Graphics
Default charset with User Defined Graphics
Actual UDG
Sound
Printing
Connectivity
