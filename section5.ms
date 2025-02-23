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
By doing this we hope we share some analysis of our code.
.
.NH 2
Program structure
.
.PP
The structure of our program is simple.
It is setup as a driver program,
and 2 modules.
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
Error handling
.
.PP
In our calculator program,
we support the operation of division.
In division,
at least in common mathmatics,
you cannot divide by 0.
So,
in our \FCdivide_numbers\F[] procedure we had to provide error handling.
We did this by checking if the second argument was 0.
If it was we set \FCquotient\F[] to a number that it very likely would not obtain,
to signal this.
We had orginally had the thought of using \FCNaN\F[],
however would not have been a simple task.
To set it we used the \FChuge\F[] intrinsic.
Before we set it printed out an error message.
.
.SOURCE divide_numbers.ms
.
.PP
Outside of \FCdivide_numbers\F[],
we handled the error by silently exiting our program.
.
.PP
There was also the error of selecting an upsupported operation.
We handled this through the use of a default case in a select case statement.
.
.NH 2
Conculsion
.
.PP
Stuff
.

