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
The goal of this section is to describe the implementation of a medium-sized program coded in the Fortran 2023 programming language.
As for what the program is supposed to do, it takes in a user numeric input (as well as other arithmetic operators) and returns a numerical output,
but anyone reading this might recognize this concept more as a simple I/O calculator.
The purpose of creating such a program is to gain experience using the Fortran programming language as well as showcase the unique strengths and limitations
of said programming language.
.
.NH 2
Program Structure
.
.PP
The structure of the medium-sized I/O calculator program is quite simple for such a powerful program.
It is set up as a driver program,
utilizing multiple modules to take in user input and perform calculations with said input before returning the correct calculated value.
.
.NH 2
Modular Structure
.
.PP
The procedures utilized in the medium-sized program are kept in submodules,
while the interfaces for these procedures are kept in the base module.
While not the most popular or well-disciplined layout for a Fortran program, a layout such as this is considered decent practice.
A recognizable structure in other high-level programming languages, specifically C-based languages, similar to submodules would be their header files.
.
.LP
.ce
Example of a Module Being Used Like a Header File
.
.SOURCE mod_medium.ms
.
.LP
.ce
Corresponding Submodules
.
.SOURCE sub_mod_medium.ms
.
.NH 2
Interfaces
.
.PP
As touched upon previously, 
interfaces are used in the program modules and utilized these implementations in the various submodules throughout the program,
and these explicitly stated interfaces are aptly referred to as explicit interfaces.
These explicit interfaces are utilized for two reasons:
this is what the Fortran programming language standard calls for, and
it allows programmers to later expand this simple calculator program to handle more than one data type/kind
through overloading.
.
.LP
.ce
Example of an Interface Being Used
.
.SOURCE interface_medium.ms
.
.NH 2
Subprograms Used
.
.PP
Most of the important operations in the program are broken up into
various, distinct subprograms.
Both types of subprogram, functions and subroutines, are used throughout.
.
.LP
.ce
Example of a Procedure from the Program
.
.SOURCE procedure_medium.ms
.
.NH 2
Parsing
.
.PP
In this I/O calculator program,
users are allowed to input an expression for calculation.
We support parentheses for selective ordering of operations,
operator precedence defined inherently in Fortran via PEMDAS,
and even certain trigonometric functions like $sin(x)$.
In order for Fortran to be able to handle these cleanly, a parser was required.
Since this program was coded in Fortran and not a modernized high-level programming language like \F[C]C\F[],
a compiler\(encompiler like \F[C]yacc\F[]/\F[C]bison\F[]
could not be used.
To combat such an inconvenience, a \fBLALR(1)\fR parser was implemented in its place.
The action and goto table was generated with \F[C]Jison\F[],
a \F[C]bison\F[]-like program in JavaScript.
It produces the tables which were promptly translated over to Fortran for future use.
.
.SOURCE grammar.ms
.
.NH 2
Enumerations
.
.PP
Parsing is difficult in a programming language like Fortran.
In order to make it easier,
certain program states should be kept.
In order to have an easily kept track of a set of states,
enumerators are used.
Enumeration types are used to declare named constants in the program.
Due to processor and compiler issues (compiler: gfortran),
enumeration types are not yet innately implemented in Fortran 2023,
so an unfortunate alternative used to combat this was Fortran 2018 C-interoperable enums.
.
.SOURCE enum_medium.ms
.
.NH 2
Conculsion
.
.PP
In this section,
a medium-sized I/O calculator supporting parentheses, operator precedence, and standard mathematical functions,
has been successfully programmed within the Fortran programming language with the help of a custom \fBLALR(1)\fP parser.
Many of the features found in Fortran were utilized in this practice program, such as modules and submodules being used to organize code,
and external help came in the form of \F[C]Jison\F[] being used to generate action/goto tables,
helping the historic programming language overcome the lack of a specific compiler-compiler.
The large amount of Fortran enumerations were also utilized in the code to build state machines and tables.
.
.PP
The modules and submodules of Fortran made organizing the program and its functions easy.
However,
The verbose syntax plaguing Fortran does not mix well with parser building.
Overall, Fortran should be used for its innate purpose of numerical computations and scientific programming,
as otherwise the language becomes too verbose and complex to justify using over other, more prominent high-level programming languages.
.

