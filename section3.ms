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
Intrinsics
.
.PP
Fortran does not have a standard library like other programming languages.
Instead of having standard library functions Fortran has intrinsics.
.
.NH 2
Modules
.
.PP
In Fortran 90 modules were added.
Modules help organize code,
in a way that is potentially eaiser to reuse and maintain.
.
.LP
.ce
Old way
.
.PROGRAM_LISTING program_contains.ms program_contains.f90
.
.PP
Using modules makes this a lot easier.
.
.PROGRAM_LISTING basic_mod.ms basic_mod.f90
.
.PROGRAM_LISTING use_mod.ms use_mod.f90
.
.PP
Modules almost always should begin with \f(CWimplcit none\fR.
In modules you can set if something is exported or not.
This is done by setting something to either \f(CWpublic\fR or \f(CWprivate\fR.
When there is a procedure (function or subroutine) that you want to use outside of
a module,
it needs to have an explicit interface.
An interface is just a delecration of what a procedure takes.
Think of it like a \f(CWC\fR prototype.
.
.NH 2
Submodules
.
.PP
Modules can be extended to submodules.
These allow you to better break up your code.
If you change something in a submodule,
only that submodule will need to be recompiled,
potentially making complation faster.
.
.PP
Submodules can be made by using
\f(CWsubmodule (super) sub\fR.
For extending submodules you can use
\f(CWsubmodule (super:sub) sub2\fR.
So on and so forth
\f(CWsubmodule (super:sub) sub3\fR.
.
.NH 2
Procedures
.
.NH 3
Functions
.
.PP
Functions are useful for breaking up your code into functions that return something.
.
.PP
A functions can be declared by
\f(CWTYPE function NAME([arguments]...) result(RETURN)\fR.
Functions are ended by
\f(CWend function NAME\fR.
The \f(CWresult(RETURN)\fR is not required.
If it is not specified,
then the variable that you return from is the same name as the function.
.
.
.NH 3
Subroutines
.
.PP
Subroutines are used for breaking your code into procedures that do not return anything.
They can be compared to void functions in \f(CWC\fR.
.
.PP
A subroutine can be decalred by
\f(CWsubroutine NAME([arguments]...)\fR.
As same as functions subroutines are ended by
\f(CWend subroutine NAME\fR.
.
.NH 3
Dummy Arguments
.
.PP
When you declare a procedure,
you don\(cqt specifiy the types of arguments in the delcaration.
These are defined within the procedure body.
.
.NH 2
Interfaces
.
.PP
Interfaces provide multiple functionalities.
They provide a delcaration of arguments and returns for a procedure.
They also allow overloading of procedures.
.
.NH 2
Arrays
.
.PP
Arrays are declared by using
\f(CWTYPE,\~dimension(SIZE)\~::\~NAME\fR.
Also can be used is
\f(CWTYPE :: NAME(SIZE)\fR.
In procedure interfaces,
it is not uncommon to replace \fISIZE\fR with \fI:\fR.
This specifies that any size could be passed.
.
.NH 2
Allocate
.
.PP
The allocate intrinsic allows you to allocate memory.
This can allow you to defer the size of an array until runtime.
.
.PP
Allocate is very useful,
but you do need to be mindful of your memory usage.
If your program terminates,
all memory is reclaimed by your operating system.
If you allocate memory,
and later leave the scope it was allocated in,
you will cause a memory leak.
Memory leaks are not good.
To advoid memory leaks,
you should deallocate the memory that you are done with.
.
.PP
To deallocate memory you use
\f(CWdeallocate(NAME)\fR.
.
.NH 2
Kind Specification
.
.PP
Sometimes a variable maybe too small or you want more precsion.
Perhaps you know a variable will never be larger them a specific amount,
so you could use a smaller variable.
(ie.\~if its never bigger than 32,767 then it can be stored in a
\f(CWshort\fR over an \f(CWint\fR).
Fortran does not have diffrent intrinsic types for diffrent sized variables.
Fortran uses a kind system.
Fortran\(cqs kind system could take up an entire chapter or 2 in a book.
.
.\"simple example here
.
.NH 2
Derived Types
.
.PP
Sometimes you have some structured data.
For example you have multiple variables that are intertwined with each other.
In \f(CWC\fR you could define a \f(CWstruct\fR for this.
In Fortran,
you would define a derived type.
.
.NH 2
Type-bound Procedures
.
.
.NH 2
Modern Fortran Explained
.
.PP
An amazing series of books with many releases following language revisions.
The latest release is \fIModern Fortran Explained: Incorperating Fortran 2023\fR.
Wrote by Michael Metcalf and John Reid, later adding Malcom Cohen and Reinhold Bader.
.
.

