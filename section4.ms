.
.EQ
delim $$
.EN
.
.NH 1 4
Basic Features
.
.PP
Stuff
.
.NH 2
Introduction
.
.PP
Stuff
.
.NH 2
Paradigm
.
.PP
As with many programing languages,
Fortran doesn't support just one paradigm.
It supports multiple.
Fortran mainly follows the impretative paradigm.
It also supports structured programing,
procedual programing,
array programing,
generic programing,
and object-oriented programing.
This lets the language be used in many diffrent ways.
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

