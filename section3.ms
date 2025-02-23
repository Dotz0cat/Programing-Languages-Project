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
Complation steps of \FChello.f90\F[]
.
.TS H
doublebox;
Lx.
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
most free form source code has the file extension \FCf90\F[].
.
.PP
Every Fortran program begins with \FCprogram [Name]\F[].
Every Fortran program also ends with \FCend program [Name]\F[].
\FCName\F[] is optional on the end statement.
After that you have your \FCuse\F[] statements.
Since this is \*QHello World!\*U, none is needed.
Next is your option to turn the imlicit features of the language off.
Most programs should use \FCimplicit none\F[].
There is almost no reason that a new modern Fortran program should use
the implicit features of the language.
After this next there is \FCprint *, \(aqHello World!\(aq\F[].
\FCprint\F[] tells the processor that it needs to print.
The \FC*\fR tells \FCprint\F[] that it needs to print to the default unit.
\FC\(aqHello World!\(aq\F[] is the string to be printed.
.
.PP
This \*QHello World!\*U program is stored into \FChello.f90\F[].
To compile this file into a runable program,
you call a compiler like gfortran to compile and link it.
Gfortran, part of the GNU Compiler Collection (GCC), is just
one example of a Fortran compiler.
There is also iFort (Intel\(cqs Fortran compiler),
Flang (The LLVM project\(cqs Fortran compiler),
nAGfor (From nAG).
After compiling the file \FChello.f90\F[] is transformed into a runable
program \FChello\F[].
.
.LP
.ce
Output of the \FChello\F[] program
.
.TS H
doublebox;
Lx.
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
Examples of intrinsics are \FCprint\F[],
\FCwrite\F[],
and \FCread\F[]
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
.bp
.
.LP
.ce
Using modules
.
.SOURCE basic_mod.ms
.
.SOURCE use_mod.ms
.
.PP
Modules almost always should begin with \FCimplcit none\F[].
In modules you can set if something is exported or not.
This is done by setting something to either \FCpublic\F[] or \FCprivate\F[].
When there is a procedure (function or subroutine) that you want to use outside of
a module,
it needs to have an explicit interface.
An interface is just a delecration of what a procedure takes.
Think of it like a \FCC\F[] prototype.
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
\FCsubmodule (super) sub\F[].
For extending submodules you can use
\FCsubmodule (super:sub) sub2\F[].
So on and so forth
\FCsubmodule (super:sub) sub3\F[].
.
.LP
.ce
Example of Submodule use
.
.SOURCE submodule.ms
.
.LP
.ce
Corasponding parent module
.
.SOURCE numbers_p.ms
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
\FCTYPE function NAME([arguments]...) result(RETURN)\F[].
Functions are ended by
\FCend function NAME\F[].
The \FCresult(RETURN)\F[] is not required.
If it is not specified,
then the variable that you return from is the same name as the function.
.
.LP
.ce
Example function
.
.SOURCE function.ms
.
.LP
.ce
Alternative function declaration
.
.SOURCE function_alt.ms
.
.NH 3
Subroutines
.
.PP
Subroutines are used for breaking your code into procedures that do not return anything.
They can be compared to void functions in \FCC\F[].
.
.PP
A subroutine can be decalred by
\FCsubroutine NAME([arguments]...)\F[].
As same as functions subroutines are ended by
\FCend subroutine NAME\F[].
.
.LP
.ce
Subroutine example
.
.SOURCE subroutine.ms
.
.NH 3
Dummy Arguments
.
.PP
When you declare a procedure,
you don\(cqt specifiy the types of arguments in the delcaration.
These are defined within the procedure body.
You must also declare the intent of your arguments.
.
.TS
Center doublebox tab(#);
C s
Cb Cb
Cf(CR) L.
Types of intent#
Type#Description
intent(in)#access in only
intetn(out)#can only access out
intent(inout)#can access both in and out
.TE
.
.NH 2
Interfaces
.
.PP
Interfaces provide multiple functionalities.
They provide a delcaration of arguments and returns for a procedure.
They also allow overloading of procedures.
.
.LP
.ce
Basic Interface Example
.
.SOURCE interface.ms
.
.LP
.ce
Interface Overloading Example
.
.SOURCE interface_overload.ms
.
.NH 2
Arrays
.
.PP
Arrays are declared by using
\FCTYPE,\~dimension(SIZE)\~::\~NAME\F[].
Also can be used is
\FCTYPE :: NAME(SIZE)\F[].
In procedure interfaces,
it is not uncommon to replace \f(CISIZE\f(TR with \f(CI:\f(TR.
This specifies that any size could be passed.
.
.LP
.ce
Example Array Delcaration
.
.SOURCE arrays.ms
.
.NH 2
Allocate
.
.PP
The allocate intrinsic allows you to allocate memory
on an object with the allocatable attribute.
This can allow you to defer the size of an array until runtime.
.
.LP
.ce
Example of array with allocatable attribute
.
.SOURCE allocatable_array.ms
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
\FCdeallocate(NAME)\F[].
.
.NH 2
Derived Types
.
.PP
Sometimes you have some structured data.
For example you have multiple variables that are intertwined with each other.
In \FCC\F[] you could define a \FCstruct\F[] for this.
In Fortran,
you would define a derived type.
.
.LP
.ce
Derived type example
.
.SOURCE derived_type.ms
.
.PP
Derived types can then be used to declare variables in your code.
.
.LP
Derived type variable declaration example
.
.SOURCE derived_type_use.ms
.
.PP
You can access the internals of the derived type by using \FC%\F[].
.
.LP
.ce
Example of derived type assignment
.
.SOURCE derived_type_assign.ms
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

