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
In this section,
the steps on how to make a basic Hello World
in the Fortran programming language program will be provided.
In addition,
the basic Fortran environment
and its compilation process will also be showcased.
Afterwards,
some major features of the language will
be demonstrated before ending with some shared sources for further inquiry regarding Fortran. 
.
.NH 2
How to Create a Hello World Program in Fortran?
.
.PROGRAM_LISTING hello.ms hello.f90
.
.LP
.ce
Compilation of Steps for \FChello.f90\F[]
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
Afterwards,
the \FCuse\F[] statements are included in the program,
but since this is a basic \*QHello World!\*U program, none are needed.
Next is the option to disable the implicit features of the language.
Most programs should use \FCimplicit none\F[];
there is almost no reason that a new modern Fortran program should use
the implicit features of the language.
After this exists the line \FCprint *, \(aqHello World!\(aq\F[].
\FCprint\F[],
which tells the processor it needs to print an argument.
The \FC*\fR tells \FCprint\F[] it needs to print to the default unit;
\FC\(aqHello World!\(aq\F[] is the string to be printed.
.
.PP
This \*QHello World!\*U program is stored into \FChello.f90\F[].
To compile this file into a runable program,
a compiler like gfortran must be called to compile and link it.
Gfortran, part of the GNU Compiler Collection (GCC), is just
one of the many examples of a Fortran compiler;
other compliers include iFort (Intel\(cqs Fortran compiler),
Flang (The LLVM project\(cqs Fortran compiler),
and nAGfor (From nAG).
After compiling the file,
\FChello.f90\F[] is transformed into a runnable
program \FChello\F[].
.
.LP
.ce
Output of the \FChello\F[] Program
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
Instead of having standard library functions, Fortran has intrinsics.
Examples of intrinsics include \FCprint\F[],
\FCwrite\F[],
and \FCread\F[]
.
.NH 2
Modules
.
.PP
In Fortran 90, modules were added.
Modules help organize code in a way where it can be potentially easier to reuse and maintain.
.
.LP
.ce
Old Way
.
.PROGRAM_LISTING program_contains.ms program_contains.f90
.
.bp
.
.LP
.ce
Using Modules
.
.SOURCE basic_mod.ms
.
.bp
.
.SOURCE use_mod.ms
.
.PP
Modules should almost always begin with \FCimplcit none\F[].
In modules, a programmer can set whether something is exported or not;
this is done by setting something to either \FCpublic\F[] or \FCprivate\F[].
When there is a procedure/subprogram
(function or subroutine) which needs to be used outside of
a module,
it needs to have an explicit interface.
An interface is just a delecration of what a procedure takes.
Think of it like a prototype in the \FCC\F[] programming langauge.
.
.bp
.
.NH 2
Submodules
.
.PP
Modules can be extended to submodules.
These allow programmers to better break up their code.
If something changes in a submodule,
only the altered submodule will need to be recompiled,
potentially making compilation faster.
.
.PP
Submodules can be made by using
\FCsubmodule (super) sub\F[].
To extend submodules, programmers use
\FCsubmodule (super:sub) sub2\F[],
\FCsubmodule (super:sub) sub3\F[],
so on and so forth.
.
.LP
.ce
Example of Submodule Use
.
.SOURCE submodule.ms
.
.LP
.ce
Corresponding Parent Module
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
Functions are useful for breaking up code into functions which return values.
.
.PP
A function can be declared by
\FCTYPE function NAME([arguments]...) result(RETURN)\F[].
Functions are ended by
\FCend function NAME\F[].
The \FCresult(RETURN)\F[] is not required.
If it is not specified,
then the variable being return from shares a name with the function in question.
.
.LP
.ce
Function Example
.
.SOURCE function.ms
.
.LP
.ce
Alternative Function Declaration
.
.SOURCE function_alt.ms
.
.bp
.
.NH 3
Subroutines
.
.PP
Subroutines are used for breaking up code into procedures which return no parameters.
They can be compared to void functions in \FCC\F[].
.
.PP
A subroutine can be declared by
\FCsubroutine NAME([arguments]...)\F[].
Like functions, subroutines are ended with
\FCend subroutine NAME\F[].
.
.LP
.ce
Subroutine Example
.
.SOURCE subroutine.ms
.
.NH 3
Dummy Arguments
.
.PP
When a programmer declares a procedure,
they don\(cqt specifiy the types of arguments in the declaration;
these are defined within the procedure body.
The intent of the procedure arguments must also be declared.
.
.TS
Center doublebox tab(#);
C s
Cb Cb
Cf(CR) L.
Types of Intent#
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
They provide a declaration of arguments and returns for a procedure.
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
\FCTYPE,\~dimension(SIZE)\~::\~NAME\F[];
an alternative declaration is
\FCTYPE :: NAME(SIZE)\F[].
In procedure interfaces,
it is not uncommon to replace \f(CISIZE\f(TR with \f(CI:\f(TR;
this allows any size to be passed.
.
.LP
.ce
Example Array Declaration
.
.SOURCE arrays.ms
.
.bp
.
.NH 2
Allocate
.
.PP
The allocate intrinsic allows programmers to allocate memory
to an object with the allocatable attribute.
This can allow programmers to defer the size of an array until runtime.
.
.LP
.ce
Example of Array w/Allocatable Attribute
.
.SOURCE allocatable_array.ms
.
.PP
Allocate is very useful,
but programmers need to be mindful of their memory usage.
If the program terminates,
all memory is reclaimed by the operating system.
If one allocates memory and later leaves the scope it was allocated in,
an undesired memory leak will occur.
To avoid memory leaks,
memory whose purpose in the program has been fulfilled should be deallocated.
.
.PP
To deallocate memory, use
\FCdeallocate(NAME)\F[].
.
.NH 2
Derived Types
.
.PP
Sometimes programmers have some structured data to deal with.
For example, the program contains multiple variables which are intertwined with each other.
In \FCC\F[],
defining a \FCstruct\F[] would be enough to handle this.
In Fortran,
a derived type would be defined instead.
.
.LP
.ce
Derived Type Example
.
.SOURCE derived_type.ms
.
.PP
Derived types can then be used to declare variables in the code.
.
.LP
Derived Type Variable Declaration Example
.
.SOURCE derived_type_use.ms
.
.PP
Programmers can access the internals of the derived type by using \FC%\F[].
.
.LP
.ce
Example of Derived Type Assignment
.
.SOURCE derived_type_assign.ms
.
.NH 2
Modern Fortran Explained
.
.PP
An amazing series of books with many releases following language revisions.
The latest release is \fIModern Fortran Explained: Incorperating Fortran 2023\fR.
Written by Michael Metcalf and John Reid,
with later additions by Malcolm Cohen and Reinhold Bader.
.
.NH 2
Fortran-lang.org
.
.PP
This is the website responsible for reviving Fortran.
It contains everything from basic tutorials to more advanced demonstrations.
There is also a discourse board,
which can be very helpful at times.
It is truly a wealth of knowledge where old-timers,
new users,
programming prodigies,
and clueless coders all gather to discuss their hatred and love of Fortran.
.
.NH 2
Gfortran Documentation
.
.PP
This source is not going to teach many new things.
However,
it shines later when trying to figure out processor-specific behaviour.
Its intrinsic procedures section is the best at showing what is available in gfortran.
.

