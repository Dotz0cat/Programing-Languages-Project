.
.EQ
delim $$
.EN
.
.NH 1 4
Basic Features
.
.NH 2
Introduction
.
.PP
What is anything in life without the small,
individually insignificant attributes which combine to make up its greater whole?
Likewise,
programming languages of all kinds fall victim to the same encapsulationist ideology,
with their unique and shared features
and attributes coming together
to create a technological marvel best described
as a translator from machine to programmer, from digital to reality,
from binary to emotion.
Formula Translator performs its namesake well given the tools to succeed,
but falters when prepped to handle a task outside of its intended scope.
The same can be applied to most other programming languages,
however Fortran possesses this problem on a magnitude
of an unthinkable scale when compared
to the popular high-level programming languages today.
To understand the basic features and quirks of Fortran
is to understand why these problems occur,
allowing for programmers to also realize the specific strengths
and unique perks of utilizing Fortran programs to their fullest extent,
which is only discovered by cultivating the program in its wild,
intended habitat of scientific and engineering programming,
numerical computations,
and high-performance computing.
.
.PP
Analyzing the basic features of a language is a tall task,
as it may be difficult to pinpoint a starting location on what to speak about first.
However,
this is but a non-issue if a certain general programming technique
applicable throughout any programming language is practiced,
the technique being divide-and-conquer.
The basic features of Fortran will be thoroughly analyzed
in different general sections consisting of its paradigm,
data types,
control flow,
expressions,
object-oriented programming functionality,
and exception handling and optimization.
These attributes will be analyzed again later
in this document with the goal of evaluating
the performance of the program in an isolated environment
as well as lightly compared to other programming languages of its caliber,
so expect the following sections to consist more of a thorough breakdown
of how Fortran operates as the first mainstream high-level programming language.
.
.NH 2
Paradigm
.
.PP
The history of paradigms supported by Fortran
is as unique as it is vast and eventful,
which makes sense considering the role
in programming history the programming language plays;
being around for such a long time naturally creates a more robust history.
For starters,
the technical term used to describe the paradigm type of Fortran
in its most modern iteration is multi-paradigm,
meaning multiple programming paradigms are supported within the language.
These paradigms include the imperative programming paradigm,
the procedural programming paradigm,
the structured programming paradigm,
the modular programming paradigm,
the object-oriented programming paradigm,
the generic programming paradigm,
the array programming paradigm,
and the parallel programming paradigm.
At first glance,
this may seem to give Fortran near limitless possibilities
in terms of what kind of programs
it can possibly make like in many other
modern high-level programming languages,
and this is a true fact,
but it falls apart in practice;
this will be explained in due time.
This document will briefly go over the
listed programming paradigms Fortran has
to offer and evaluate them each based
on the context of the capabilities of Fortran.
.
.NH 3
Imperative Programming
.
.PP
Fortran was initially designed around a particular programming paradigm back in 1957,
a common practice for many programming languages back in its time,
and this paradigm is imperative programming.
Imperative programming is a paradigm which utilizes the programmer
and their syntax above all else,
with the programmers specifying how each step of a program should
be executed using commands and state changes with the desire
to achieve a result in the end.
Being the paradigm Fortran was built for,
it utilizes this paradigm well to calculate
and determine mathematical results and is still considered to be one of,
if not the,
strongest paradigm usable for the historic programming language.
.
.NH 3
Procedural Programming
.
.PP
The first newly implemented programming paradigm implemented in Fortran
is procedural programming,
added in the first language revision update of FORTRAN II in 1958,
according to Obliquity.com.
Procedural programming is a paradigm structuring the program
with a sequence of procedures or functions which are called in a specific order,
built off the back of imperative programming as what explicitly needs
to happen is still required to be stated by the programmer,
all with the same goal of gaining a result by the end.
Being somewhat of a continuation of the innately designed imperative paradigm,
Fortran utilizes these procedural programming ideas well even in the modern day,
allowing for clearer and more readable code while still containing
the pinpoint programmer control of imperative programming.
.
.NH 3
Structured Programming
.
.PP
One of the first programming paradigms added
to Fortran in FORTRAN 77 was structured programming,
and though its functionality at release in comparison
to its functionality and capabilities today are laughable,
this was a major achievement for the programming language.
Structured programming is a paradigm using a strict approach
to organize code and access only certain parts at a time with control structures.
The control structures of Fortran will be covered later in the document,
but know Fortran set the standard for modern structured programming in 1977,
even if it lacks in comparison to other modern programming languages today.
.
.NH 3
Modular Programming
.
.PP
The next major update in Fortran is Fortran 90,
and with it came the new paradigm of modular programming,
the ability of encapsulation finally becoming a feature of Fortran.
Modular programming is a paradigm where software is broken down into smaller,
interchangeable modules which each play a role in making the program function.
Fortran includes these features as modules,
and its simple syntax and encapsulation abilities make for perfect library
and application creation,
according to fortran-lang.org.
.
.NH 3
Object-Oriented Programming
.
.PP
In the modern programming landscape,
object-oriented programming is all but required
for a programming language to be successful,
and this essential paradigm is included in Fortran 2003,
though its inclusion is flawed.
Object-oriented programming is a very popular paradigm
utilizing objects containing data and methods to operate
with said data in order to organize programs.
While OOP is the crux of the most used high-level programming languages,
it works well with them since they were built around the idea of OOP,
while Fortran simply slapped the complex paradigm into its realistically archaic syntax,
which results in a clunky interface avoided by most Fortran programmers
who work in Fortran-reliant fields.
.
.NH 3
Generic Programming
.
.PP
Another paradigm introduced in the major Fortran 2003 update is generic programming,
which gives the historic programming language the ability to use interfaces.
Generic programming is a paradigm using type parameters
to make algorithms and data structures able to accept different
data types without having to write different versions
of said algorithms and structures.
While generic programming is a useful technique
in many programming languages,
it can only go so far with Fortran as overloading
function procedures lack templates,
so its usefulness is extended only to simpler programs.
.
.NH 3
Array Programming
.
.PP
Ever since the Fortran 90 update,
the historic high-level programming language
has dominated other languages in array operations thanks
to the solid implementation of the array programming paradigm.
Array programming is a paradigm allowing for mathematical operations
to be performed on entire arrays at once instead of one element at a time.
The programming language meticulously designed
to excel with scientific programming and complex mathematical computations
happily takes an addition as large as the array-based paradigm,
and this singular programming technique allows Fortran to become one of,
if not the best,
high-level programming language choice for array arithmetic and conversions,
which is a major factor contributing to its modern usage.
.
.NH 3
Parallel Programming
.
.PP
The last paradigm covered is parallel programming,
which was included in Fortran 2008
as a way to clean up code with ISO language standards,
according to NVidia.
Parallel programming is a paradigm which organizes code
to utilize multiple processors to execute multiple instructions
at once to speed up execution type and improve overall program performance.
This paradigm gives way to coarrays for distributed memory parallelism,
and techniques to utilize multiple cores sounds useful
to a programming language which specializes in high-performance computing,
though its implementation is rocky and thus rarely used
in practice by top Fortran prorgrammers.
.
.NH 2
Data Types
.
.PP
Fortran has Types and Kinds.
.
.TS H
center doublebox tab(#);
Cb s, Lb Cb.
Fortran Data Types
Data type#Description
_
.TH
.T&
Lf(CR) C.
Integer#T{
Numeric Type
T}
Real#T{
Floating point numeric Type
T}
Complex#T{
Complex floating point numeric Type
T}
Logical#T{
A Boolean Type
T}
Character#T{
Character Types
T}
Derived Type#T{
User-Defined Type
T}
.TE
.
.TS H
center doublebox tab(#);
Cb s s, C s s, Cb Cb Cb.
Fortran Kind Types
Taken from \FCiso_fortran_env\F[]
Kind#C Equivalent#Description
_
.TH
.T&
Lf(CR) Lf(CR) C.
int8#signed char#T{
8-Bit signed Integer
T}
int16#signed short#T{
16-Bit signed Integer
T}
int32#signed int#T{
32-Bit signed Integer
T}
int64#signed long#T{
64-Bit signed Integer
T}
real32#float#T{
32-Bit IEEE 754 single precision floating point number
T}
real64#double#T{
64-Bit IEEE 754 double precision floating point number
T}
real128#long double#T{
128-Bit IEEE 754 quadruple precision floating point number
T}
.TE
.
.NH 2
Control Flow
.
.PP
Fortran provides multiple kinds of control flow.
.
.NH 3
If then
.
.PP
Fortran provides an if statement.
It is used to change the flow of a program upon a condition being true.
.
.SOURCE if_then.ms
.
.SOURCE if_else.ms
.
.SOURCE if_elif.ms
.
.NH 3
Select case
.
.PP
Fortran provides a \F[C]select case\F[] statement.
This behaves like a \F[C]switch\F[] in \F[C]C\F[].
.
.SOURCE select_case.ms
.
.SOURCE select_range.ms
.
.SOURCE select_case_ranges.ms
.
.NH 3
Do construct
.
.PP
Fortran provides a \F[C]do\F[] construct.
It is equvilent to a \F[C]for\F[] loop in \F[C]C\F[].
Notice that Fortran does not provide a while loop.
.
.SOURCE do.ms
.
.SOURCE do_step.ms
.
.SOURCE do_while_eqiv.ms
.
.SOURCE depracated_while.ms
.
.NH 2
Expressions
.
.PP
Fortran is considered by many experts to be
the first majorly popular high-level programming language,
and Backus and his team of IBM employees who created the language
assumedly believed as much before its release in 1957.
To break the mold in this world is to follow in the footsteps of predecessors
and improve on their own work,
all while implementing a new twist into a stale formula
to craft a truly unique product.
While low-level and medium-level programming languages possess the capabilities
to write expressions,
these expressions are a far cry from what many programmers
consider expressions to be today in the modern computing landscape.
In fact,
many of these expressions are able to be produced
in these non-high-level programming languages,
but the recognizable English syntax of Fortran allows
for easier use of these expressions.
This section covers the revolutionary expressions provided
by Fortran and their uses with light code examples to boot.
.
.NH 3
Arithmetic Expressions
.
.PP
In the modern day,
Fortran is utilized for its uses in scientific and engineering programming,
high-performance computing potential,
and unmatched innate capabilities of numerical computation,
so one may expect its arithmetic-based expressions to be top-notch,
and this seems to be the case.
Utilizing PEMDAS rules,
Fortran mathematics are considered by most experts of the language
to be one of its greatest strengths from its conciseness and ease of use.
Below is a table showcasing some of the simple arithmetic
operations and possible expressions in Fortran:
.
.TS
doublebox center tab(#);
Cb S S
Lb Lb Lb
Lf(C) Li Lf(C).
Operators##
Operator#Meaning#Example
_
+#Addition#a + b
-#Subtraction#x - y
*#Multiplcation#3 * 5
/#Division#10.0 / 2.0
**#Expontiation#2 ** 3
.TE
.
.NH 3
Relational Expressions
.
.PP
Fortran is most touted for its numerical computation abilities
and scientific programming prowess,
so naturally analyzing its user interface capabilities with menus
or user input baked into the program itself comes with a few drawbacks.
Relational expressions,
for example,
are highly used in these user-interface programs,
so their usefulness extends to real-world Fortran
only in checking logical conditions,
whatever they may be used for.
Below is a table showcasing some of the relational expressions usable in Fortran:
.
.TS
doublebox center tab(#);
Cb S S S
Lb Lb Lb Lb
Lf(C) Lf(C) Li Lf(C).
Operators###
Operator#Alterntave#Meaning#Example
_
==#.eq.#Equal to#x == y
/=#.neq.#Not equal to#a /= b
<#.lt.#Less than#i < 10
<=#.le.#Less or equal#j <= k
>#.gt.#Greater than#m > n
>=#.ge.#Greater or equal#p >= q
.TE
.
.NH 3
Logical Expression
.
.PP
Similar to relational expressions,
the usefulness of logical expressions within
real-world usages of Fortran is mainly only extended to checking logical conditions.
Logical expressions are also often avoided in Fortran programs
due to their verbosity when compared to other logical expressions
from high-level programming languages,
albeit not as wordy as it could possibly be. 
Below is a table showcasing some of the logical expressions found within Fortran:
.
.TS
doublebox center tab(#);
Cb S S
Lb Lb Lb
Lf(C) Li Lf(C).
Operators##
Operator#Meaning#Example
_
\&.and.#Logical AND#(a > 0) .and (b < 0)
\&.or.#Logical OR#(x == y) .or. (x == z)
\&.not.#Logical NOT#.not. (flag)
\&.eqv.#Logical equvilence#(a .eqv. b)
\&.neqv.#Logical non equvilence#(p .neqv. q)
.TE
.
.NH 3
Character Expressions
.
.PP
Strings/Fortran characters are rarely used in Fortran for good reason,
mainly because the programming language lacks the many functionalities
and methods usable for strings which exist in most other high-level programming languages.
There is but one character expression usable by Fortran:
\F[C]//\F[],
which is defined as the concatenation operator by Fortran;
the lack of character expressions is telling but understandable,
as Fortran lacks a necessity for character-based code since
its main uses ignore such features.
Below is a code example on how the concatenation operator of Fortran works:
.
.SOURCE concat.ms
.
.NH 3
Array Expressions
.
.PP
The arguably strongest tool in the arsenal of Fortran was released
with the major Fortran 90 update,
the tool being the introduction of array operations
and expressions which utilize all elements of an array
at once instead of a single element at a time.
As a programming language whose calling card is their ability
to handle and perform mathematical computations and high-performance computing,
implementing innate array expressions into the language was a no-brainer,
and now this functionality acts as a major reason to use Fortran.
Below is a snippet of code showcasing a few array operations along
with array sectioning to work on specified elements of an array:
.
.SOURCE array_ops.ms
.
.SOURCE array_subops.ms
.
.NH 3
Conditional Expressions
.
.PP
One weakness of Fortran is its lack of usability in user-oriented programs,
and this weakness is only made more apparent with the janky conditional operators.
Fortran today writes conditional expressions
using VERBOSE \F[C]IF-THEN-ELSE\F[] blocks of code,
but the \F[C]THEN-ELSE\F[] portion was originally absent,
leading to confusing syntax and discouraged programmers.
Another way Fortran writes conditional expressions is with the \F[C]merge(3)\F[] method,
which takes in 3 expressions and produces a result based
on a condition check usually performed with relational operators.
Examples of both types of conditional expressions are shown below:
.
.SOURCE ternary.ms
.
.SOURCE merge.ms
.
.NH 3
User-Defined Operators
.
.PP
The last expression type covered in this report
is the user-defined operator expressions one may overload if they so wish.
Seeing where this is useful is tough,
as normally user-defined operators extend the functionality
of the programming language so specific domains and data types
have certain operators to use whenever these domains are called upon,
but Fortran is a one-trick pony who does their tricks extremely well.
Nonetheless,
overloading operators with user-defined ones is simple,
and the code below showcases its usage in small program:
.
.SOURCE operator_overload.ms
.
.NH 2
Subprograms
.
.PP
Another area of innovation by Fortran must be its advancements in subprograms,
even if these advancements are in its English syntax developments.
Subprograms exist in non-high-level programming languages but are
often verbose and confusing to code with,
which is unfortunate considering the usefulness of subprograms
in real-world programs regarding readability and debugging management.
The advancement of usage for the English language
in the syntax of Fortran helps clear up confusing messes
of code which plagued the readability and,
consequently,
usefulness of subprograms.
The two main types of subprogram,
functions and subroutines,
will be reviewed below.
.
.NH 3
Functions
.
.PP
The first type of main subprogram utilized in Fortran is the function,
which fulfills the role of computing and returning a single value after
being called by the program.
These are perfect for returning the value of mathematical computations
which are computed by the code within the function,
and any way to organize and maintain the code for Fortran
and its unique and complex mathematical processing programs is a major help.
An example of some Fortran function code is shown below:
.
.SOURCE square.ms
.
.NH 3
Subroutines
.
.PP
The second type of main subprogram utilized in Fortran is the subroutine,
which performs operations whenever called by the program
and may modify arguments within its code.
While subroutines may seem so similar to functions
to the point where its existence is questionable,
a few key differences let the former stick out from the latter,
mainly its trait of not returning values.
Other notable differences from functions include the already-stated ability
to modify arguments within its code and perform I/O operations
along with other side effects if desired.
Code showcasing the usage of subroutines is shown below:
.
.SOURCE swap.ms
.
.NH 3
Key Features
.
.PP
While functions and subroutines alone are nice to have
for program organization and efficiency,
Fortran subprograms possess a few key features added in the major Fortran 90 update,
many of which increasing the usability and potential of subprograms.
One of the most important features included
is the \F[C]INTENT(IN/OUT/INOUT)\F[] method which determines the intent
of a certain argument as input-only,
output-only,
and both input and output respectively,
allowing for clean I/O functionality;
functions are usually \F[C]INTENT(IN)\F[],
while subroutines are conversely \F[C]INTENT(OUT)\F[].
Other important features include the \F[C]OPTIONAL\F[] keyword for flexible arguments,
the \F[C]RECURSIVE\F[] keyword for its namesake recursive functions or subroutines,
the \F[C]PURE\F[] and \F[C]ELEMENTAL\F[] keywords introduced in Fortran 95
for subprograms with the parallel computing-friendly lack of side effects
and array or scalar-supported work,
and internal subprograms defined within a \F[C]PROGRAM\F[],
\F[C]FUNCTION\F[],
or \F[C]SUBROUTINE\F[].
An example piece of code showcasing a recursive subprogram is shown below:
.
.SOURCE recursive.ms
.
.NH 2
Object-orientation
.
.PP
Ever since the major Fortran 2003 update,
the historic programming language has supported object-oriented programming,
and while its existence and shortcomings have been touched upon
in the paradigm section already,
this portion of the document will discuss
its possible Fortran features if one was to
ever use OOP in a Fortran program instead
of a more competent programming language with
a numerical computation library for some odd reason.
Just because OOP in Fortran is clunky and feels unplanned
is not an excuse to avoid understanding its OOP abilities,
after all, as there exists a few positives to knowing
its features such as improved code reusability and gaining
the ability to model real-world entities. 
.
.NH 3
Basics of OOP
.
.PP
Fortran uses derived types to define classes,
which are like the keywords \F[C]struct\F[] in C-based programming languages
and \F[C]class\F[] in other OOP-supported languages.
These derived type classes are then bound to subprograms acting as methods,
which execute the code containing the derived type.
An example of a derived type class declaration
and a method procedure being called is shown below:
.
.SOURCE class.ms
.
.SOURCE type_bound_procedure.ms
.
.SOURCE tbp_usage.ms
.
.NH 3
Polymorphism & Inheritance
.
.PP
As with most programming languages which support object-oriented programming,
polymorphism and single inheritance are supported
to maintain code and keep it flexible for future use.
Using the \F[C]EXTENDS\F[] keyword allows for a Fortran class
to inherit from another class,
maintaining the functions,
variables and other reusable code of the class being inherited from.
Defining an object with the keyword \F[C]CLASS\F[]
instead of \F[C]TYPE\F[] allows for polymorphism and,
consequently, flexible object code within the program.
Code examples of polymorphism and inheritance are shown below:
.
.SOURCE poly.ms
.
.SOURCE poly_usage.ms
.
.SOURCE inherit.ms
.
.SOURCE inherit_usage.ms
.
.NH 3
Encapsulation, Overloading, & Abstract Interfaces
.
.PP
OOP in Fortran also allows for the usual abilities of encapsulation,
overloading,
and interface abstraction with the right method calls.
Encapsulation is simple as it is with most other OOP-supporting programming languages,
relying on \F[C]PRIVATE\F[] and \F[C]PUBLIC\F[] keywords
to hide data from the user and control how data is accessed.
Abstract base classes resemble Java and C# interfaces and act as the
replacement for Fortran,
though its usability is fairly limited with
its tough-love nephew templates not being around to pick up
the pace where these pseudo-interfaces fall behind.
Operator overloading has been discussed in this features section already,
but maybe visualizing the code for encapsulation
and interface abstraction can clear up any misconceptions
regarding their implementation in OOP.
.
.SOURCE encap.ms
.
.SOURCE encap_usage.ms
.
.SOURCE abstract.ms
.
.SOURCE abstract_usage.ms
.
.NH 2
Exception Handling and Optimization
.
.PP
Stuff
.
.

