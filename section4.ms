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
What is anything in life without the small, individually insignificant attributes which combine to make up its greater whole? Likewise, programming languages of all kinds fall victim to the same encapsulationist ideology, with their unique and shared features and attributes coming together to create a technological marvel best described as a translator from machine to programmer, from digital to reality, from binary to emotion. Formula Translator performs its namesake well given the tools to succeed, but falters when prepped to handle a task outside of its intended scope. The same can be applied to most other programming languages, however Fortran possesses this problem on a magnitude of an unthinkable scale when compared to the popular high-level programming languages today. To understand the basic features and quirks of Fortran is to understand why these problems occur, allowing for programmers to also realize the specific strengths and unique perks of utilizing Fortran programs to their fullest extent, which is only discovered by cultivating the program in its wild, intended habitat of scientific and engineering programming, numerical computations, and high-performance computing.
.
.PP
Analyzing the basic features of a language is a tall task, as it may be difficult to pinpoint a starting location on what to speak about first. However, this is but a non-issue if a certain general programming technique applicable throughout any programming language is practiced, the technique being divide-and-conquer. The basic features of Fortran will be thoroughly analyzed in different general sections consisting of its paradigm, data types, control flow, expressions, object-oriented programming functionality, and exception handling and optimization. These attributes will be analyzed again later in this document with the goal of evaluating the performance of the program in an isolated environment as well as lightly compared to other programming languages of its caliber, so expect the following sections to consist more of a thorough breakdown of how Fortran operates as the first mainstream high-level programming language.
.
.NH 2
Paradigm
.
.PP
The history of paradigms supported by Fortran is as unique as it is vast and eventful, which makes sense considering the role in programming history the programming language plays; being around for such a long time naturally creates a more robust history. For starters, the technical term used to describe the paradigm type of Fortran in its most modern iteration is multi-paradigm, meaning multiple programming paradigms are supported within the language. These paradigms include the imperative programming paradigm, the procedural programming paradigm, the structured programming paradigm, the modular programming paradigm, the object-oriented programming paradigm, the generic programming paradigm, the array programming paradigm, and the parallel programming paradigm. At first glance, this may seem to give Fortran near limitless possibilities in terms of what kind of programs it can possibly make like in many other modern high-level programming languages, and this is a true fact, but it falls apart in practice; this will be explained in due time. This document will briefly go over the listed programming paradigms Fortran has to offer and evaluate them each based on the context of the capabilities of Fortran.
.
.NH 3
Imperative Programming
.
.PP
Fortran was initially designed around a particular programming paradigm back in 1957, a common practice for many programming languages back in its time, and this paradigm is imperative programming. Imperative programming is a paradigm which utilizes the programmer and their syntax above all else, with the programmers specifying how each step of a program should be executed using commands and state changes with the desire to achieve a result in the end. Being the paradigm Fortran was built for, it utilizes this paradigm well to calculate and determine mathematical results and is still considered to be one of, if not the, strongest paradigm usable for the historic programming language.
.
.NH 3
Procedural Programming
.
.PP
The first newly implemented programming paradigm implemented in Fortran is procedural programming, added in the first language revision update of FORTRAN II in 1958, according to Obliquity.com. Procedural programming is a paradigm structuring the program with a sequence of procedures or functions which are called in a specific order, built off the back of imperative programming as what explicitly needs to happen is still required to be stated by the programmer, all with the same goal of gaining a result by the end. Being somewhat of a continuation of the innately designed imperative paradigm, Fortran utilizes these procedural programming ideas well even in the modern day, allowing for clearer and more readable code while still containing the pinpoint programmer control of imperative programming.
.
.NH 3
Structured Programming
.
.PP
One of the first programming paradigms added to Fortran in FORTRAN 77 was structured programming, and though its functionality at release in comparison to its functionality and capabilities today are laughable, this was a major achievement for the programming language. Structured programming is a paradigm using a strict approach to organize code and access only certain parts at a time with control structures. The control structures of Fortran will be covered later in the document, but know Fortran set the standard for modern structured programming in 1977, even if it lacks in comparison to other modern programming languages today.
.
.NH 3
Modular Programming
.
.PP
The next major update in Fortran is Fortran 90, and with it came the new paradigm of modular programming, the ability of encapsulation finally becoming a feature of Fortran. Modular programming is a paradigm where software is broken down into smaller, interchangeable modules which each play a role in making the program function. Fortran includes these features as modules, and its simple syntax and encapsulation abilities make for perfect library and application creation, according to fortran-lang.org.
.
.NH 3
Object-Oriented Programming
.
.PP
In the modern programming landscape, object-oriented programming is all but required for a programming language to be successful, and this essential paradigm is included in Fortran 2003, though its inclusion is flawed. Object-oriented programming is a very popular paradigm utilizing objects containing data and methods to operate with said data in order to organize programs. While OOP is the crux of the most used high-level programming languages, it works well with them since they were built around the idea of OOP, while Fortran simply slapped the complex paradigm into its realistically archaic syntax, which results in a clunky interface avoided by most Fortran programmers who work in Fortran-reliant fields.
.
.NH 3
Generic Programming
.
.PP
Another paradigm introduced in the major Fortran 2003 update is generic programming, which gives the historic programming language the ability to use interfaces. Generic programming is a paradigm using type parameters to make algorithms and data structures able to accept different data types without having to write different versions of said algorithms and structures. While generic programming is a useful technique in many programming languages, it can only go so far with Fortran as overloading function procedures lack templates, so its usefulness is extended only to simpler programs.
.
.NH 3
Array Programming
.
.PP
Ever since the Fortran 90 update, the historic high-level programming language has dominated other languages in array operations thanks to the solid implementation of the array programming paradigm. Array programming is a paradigm allowing for mathematical operations to be performed on entire arrays at once instead of one element at a time. The programming language meticulously designed to excel with scientific programming and complex mathematical computations happily takes an addition as large as the array-based paradigm, and this singular programming technique allows Fortran to become one of, if not the best, high-level programming language choice for array arithmetic and conversions, which is a major factor contributing to its modern usage.
.
.NH 3
Parallel Programming
.
.PP
The last paradigm covered is parallel programming, which was included in Fortran 2008 as a way to clean up code with ISO language standards, according to NVidia. Parallel programming is a paradigm which organizes code to utilize multiple processors to execute multiple instructions at once to speed up execution type and improve overall program performance. This paradigm gives way to coarrays for distributed memory parallelism, and techniques to utilize multiple cores sounds useful to a programming language which specializes in high-performance computing, though its implementation is rocky and thus rarely used in practice by top Fortran prorgrammers.
.
.NH 2
Data Types
.
.PP
Fortran has Types and Kinds.

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
numeric type
T}
Real#T{
Floating point numeric type
T}
Complex#T{
Complex floating point numeric type
T}
Logical#T{
A boolean type
T}
Character#T{
Character types
T}
Derived Type#T{
User defined type
T}
.TE
.
.TS H
center doublebox tab(#);
Cb s s, C s s, Cb Cb Cb.
Fortran Kind Types
taken from \FCiso_fortran_env\F[]
Kind#C equivlent#Description
_
.TH
.T&
Lf(CR) Lf(CR) C.
int8#signed char#T{
8 bit signed integer
T}
int16#signed short#T{
16 bit signed integer
T}
int32#signed int#T{
32 bit signed integer
T}
int64#signed long#T{
64 bit signed integer
T}
real32#float#T{
32 bit ieee 754 single precision floating point number
T}
real64#double#T{
64 bit ieee 754 double precision floating point number
T}
real128#long double#T{
128 bit ieee 754 quaduraple precision floating point number
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
Stuff
.
.NH 2
Subprograms
.
.PP
Stuff
.
.NH 2
Object-orientation
.
.PP
Stuff
.
.NH 2
Exception Handling and Optimization
.
.PP
Stuff
.
.

