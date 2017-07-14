# A BASIC interpreter

Dialect specification:

The language is somewhat modelled after Sinclair BASIC.
**Some** internals are simulated.


Lines are numbered.
Lines can contain more than one statement, separated by colons (:).
Indices are 1-based.
Code RUN is executed sequentially starting at line 0 by default.

Data types:
Strings
Numbers

10 PRINT 1
20 GOTO 10


British spelling is prefered.

Planned features:

Colour support
Graphics
Default charset with User Defined Graphics
Actual UDG
Sound
Printing
Connectivity
