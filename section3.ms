.
.EQ
delim $$
.EN
.
.NH 1 3
Tutorial
.
.NH 2
Introduction
.
.PP
In this section we want to show how to make a basic hello world program.
We also want to introduce you to the basic Fortran enviroment and complation process.
After that we want to demostrate some of the major features of the language.
Finally we will share some resources for learning the language.
.
.NH 2
How do you create a basic hello world program?
.
.PROGRAM_LISTING hello.ms hello.f90
.
.LP
.ce
Complation steps of \f(CWhello.f90\fR
.
.TS H
center doublebox;
L.
.TH
gfortran hello.f90 -o hello
.TE
.
.PP
Fortran as a language has 2 forms of syntax.
Fixed form and free form.
Fixed form was meant for punched cards.
While free form is less strict in its formatting.
Free form source code was added in Fortran 90.
Due to this,
most free form source code has the file extension \f(CWf90\fR.
.
.PP
Every Fortran program begins with \f(CWprogram [Name]\fR.
Every Fortran program also ends with \f(CWend program [Name]\fR.
\f(CWName\fR is optional on the end statement.
After that you have your \f(CWuse\fR statements.
Since this is \*QHello World!\*U, none is needed.
Next is your option to turn the imlicit features of the language off.
Most programs should use \f(CWimplicit none\fR.
There is almost no reason that a new modern Fortran program should use
the implicit features of the language.
After this next there is \f(CWprint *, \(aqHello World!\(aq\fR.
\f(CWprint\fR tells the processor that it needs to print.
The \f(CW*\fR tells \f(CWprint\fR that it needs to print to the default unit.
\f(CW\(aqHello World!\(aq\fR is the string to be printed.
.
.PP
This \*QHello World!\*U program is stored into \f(CWhello.f90\fR.
To compile this file into a runable program,
you call a compiler like gfortran to compile and link it.
Gfortran, part of the GNU Compiler Collection (GCC), is just
one example of a Fortran compiler.
There is also iFort (Intel\(cqs Fortran compiler),
Flang (The LLVM project\(cqs Fortran compiler),
nAGfor (From nAG).
After compiling the file \f(CWhello.f90\fR is transformed into a runable
program \f(CWhello\fR.
.
.LP
.ce
Output of the \f(CWhello\fR program
.
.TS H
center doublebox;
L.
.TH
\& Hello World!
.TE
.
.NH 2
Major features
.
.PP
Stuff
.
.NH 2
Resources
.
.PP
Stuff
.
.

