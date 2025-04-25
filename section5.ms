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
a compiler\(encompiler like \F[C]yacc\F[]/\F[C]bison\F[]
could not be used.
To do this a \fBLALR(1)\fR parser was implemented.
The action and goto table was generated with \F[C]Jison\F[],
a \F[C]bison\F[]-like program in javascript.
It produced the tables that could be translated over to Fortran.
.
.SOURCE grammar.ms
.
.NH 2
Enumerations
.
.PP
Parsing is hard.
In order to make it easier,
states should be kept.
In order to have a easy to keep track of set of states,
enumerators are used.
Enumeration types are used to declare named constants in our program.
Due to processor issues,
(compiler: gfortran)
Fortran 2023 enumeration types are not yet implemented.
Due to this we used Fortran 2018 C interoptable enums.
.
.SOURCE enum_medium.ms
.
.NH 2
Conculsion
.
.PP
In this section,
we have implemented a medium-sized Fortran calculator that supports parentheses,
operator precedence,
and standard math functions via a custom \fBLALR(1)\fP parser.
We used modules and submodules to organize our code.
We used \F[C]Jison\F[] to generate our action/goto tables,
to overcome the lack of a Fortran specific compiler-compiler.
We also made large use of Fortran\(cqs enumerations,
to build our state machines and tables.
.
.PP
On the positive side,
Fortran\(cqs modules and submodules made organizing our code easy.
However,
Fortran\(cqs verbose syntax,
does not lend well to parser building.
Overall Fortran,
is not a bad language when it is used for what it is suited for doing.
.

