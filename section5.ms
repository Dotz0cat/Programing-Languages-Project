.
.EQ
delim $$
.EN
.
.NH 1 5
Medium Program Implmentation
.
.NH 2
Introduction
.
.PP
The goal of this section is to describe the implementation of our medium program.
For our medium program we chose to implement a calculator.
By doing this we hope to share some analysis of our code.
.
.NH 2
Program structure
.
.PP
The structure of our program is simple.
It is setup as a driver program,
and multiple modules.
.
.NH 2
The modular structure
.
.PP
We contain the procedures that we use in submodules,
while we have the interfaces for these procedures in the base module.
This is considered decent practice.
This can be compared to \FCC\F[]\(cqs header files.
.
.LP
.ce
Example of module used like a header file
.
.SOURCE mod_medium.ms
.
.LP
.ce
The corasponding submodule
.
.SOURCE sub_mod_medium.ms
.
.NH 2
Interfaces
.
.PP
As touched upon previously,
we used interfaces in our modules and put the implementations in our submodules.
These are explicit interfaces.
We used them like this for 2 reasons.
First,
as that is what the standard calls for.
Second,
as it allows us to later expand our program to handle more than one type / kind
through overloading.
.
.LP
.ce
Example of interface used
.
.SOURCE interface_medium.ms
.
.NH 2
Subprograms used
.
.PP
Most of the important operations in the program are broken up into
subprograms.
Both functions and subroutines are used.
.
.LP
.ce
Example of procedure from program
.
.SOURCE procedure_medium.ms
.
.NH 2
Parsing
.
.PP
In our calculator program,
we allow users to enter an expression for calculation.
We support parenpahies,
operator preseidence,
and functions like $sin(x)$.
To be able to handle these cleanly a parser was needed.
Since this program was in Fortran and not \F[C]C\F[],
a compiler\(emcompiler like \F[C]yacc\F[]/\F[C]bison\F[]
could not be used.
To do this a \fBLALR(1)\fR parser was implemented.
The action and goto table was generated with \F[C]Jison\F[],
a \F[C]bison\F[]-like program in javascript.
It produced the tables that could be translated over to Fortran.
.
.SOURCE grammar.ms
.
.NH 2
Conculsion
.
.PP
Stuff
.

