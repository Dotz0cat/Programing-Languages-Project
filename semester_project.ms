.
.nr PS 12p
.
.EQ
delim $$
.EN
.
.eo
.de SOURCE_HEADING ..
.
.LP
.ce
Program Listing of \FC\$1\F[]
.
...
.ec
.
.eo
.de SOURCE ..
.
.sp
.
\l'6.5i'\h'-6.5i'\v'-0.2v'\l'6.5i'\v'0.2v'
.so \$1
\"
.gcolor black
\l'6.5i'\h'-6.5i'\v'-0.2v'\l'6.5i'\v'0.2v'
.
...
.ec
.
.eo
.de PROGRAM_LISTING ..
.ds listing_file \$1
.ie ( \n[.$] = 2 )  .ds optional_program_file \$2
.el                 .ds optional_program_file \*[listing_file]
.
.SOURCE_HEADING \*[optional_program_file]
.SOURCE \*[listing_file]
.
...
.ec
.
.\"\X'papersize=8.5i,11i'
.
\&
.sp 8
.
.LP
.ce
.
\s(18\fBA Report on the Fortran 23 Programming Language\fR\s0
.
.sp 29
.
.ID 4.0i
Seth Arnold
Carson Milano
2025/4/25
CSCI 3370 Programming Languages
Dr.\& Vamsi Paruchuri
Department of Computer Science
University of Central Arkansas
.DE
.
\&
.bp
.
.LP
\s(18\fBTable of Contents\fR\s0
.
.sp
.
.LP
\s(18\fBSection 1: Introduction\fR\s0
.
.sp
.
.RS
.
.LP
1.1
What was the goal of the project?
Why you choose this language?
What was the medium program that you decided to implement?
What is the goal of this report?
.
.LP
1.2
How does the report achieve this goal...
Give a brief overview of the sections that comprise the report.
.
.RE
.
.sp
.
.LP
\s(18\fBSection 2: History\fR\s0
.
.sp
.
.RS
.
.LP
2.1
Introduction
.
.LP
2.2
Creation
.
.LP
2.3
Lifetime
.
.LP
2.4
Current status in Industry
.
.RE
.
.sp
.
.LP
\s(18\fBSection 3: Tutoral\fR\s0
.
.sp
.
.RS
.
.LP
3.1
Introduction
.
.LP
3.2
How Do you create a basic hello world program?
.
.LP
3.3
Interinsics
.
.LP
3.4
Modules
.
.LP
3.5
Submodules
.
.LP
3.6
Procedures
.
.RS
.
.LP
3.6.1
Functions
.
.LP
3.6.2
Subroutines
.
.LP
3.6.3
Dummy Arguments
.
.RE
.
.LP
3.7
Interfaces
.
.LP
3.8
Arrays
.
.LP
3.9
Allocate
.
.LP
3.10
Derived types
.
.LP
3.11
Modern Fortran Explained
.
.LP
3.12
Fortran-lang.org
.
.LP
3.13
Gfortran Documentation
.
.RE
.
.sp
.
.LP
\s(18\fBSection 4: Basic Features\fR\s0
.
.sp
.
.RS
.
.LP
4.1
Introduction
.
.LP
4.2
Paradigm
.
.RS
.
.LP
4.2.1
Imperative Programming
.
.LP
4.2.2
Procedural Programming
.
.LP
4.2.3
Structured Programming
.
.LP
4.2.4
Modular Programming
.
.LP
4.2.5
Object-Oriented Programming
.
.LP
4.2.6
Generic Programming
.
.LP
4.2.7
Array Programming
.
.LP
4.2.8
Parallel Programming
.
.RE
.
.LP
4.3
Data Types
.
.LP
4.4
Control Flow
.
.RS
.
.LP
4.4.1
If then
.
.LP
4.4.2
Select case
.
.LP
4.4.3
Do construct
.
.RE
.
.LP
4.5
Expressions
.
.RS
.
.LP
4.5.1
Arithmetic Expressions
.
.LP
4.5.2
Relational Expressions
.
.LP
4.5.3
Logical Expression
.
.LP
4.5.4
Character Expressions
.
.LP
4.5.5
Array Expressions
.
.LP
4.5.6
Conditional Expressions
.
.LP
4.5.7
User-Defined Operators
.
.RE
.
.LP
4.6
Subprograms
.
.RS
.
.LP
4.6.1
Functions
.
.LP
4.6.2
Subroutines
.
.LP
4.6.3
Key Features
.
.RE
.
.LP
4.7
Object-orientation
.
.RS
.
.LP
4.7.1
Basics of OOP
.
.LP
4.7.2
Polymorphism & Inheritance
.
.LP
4.7.3.
Encapsulation,
Overloading,
& Abstract Interfaces
.
.RE
.
.LP
4.8
Exception Handling and Optimization 
.
.RS
.
.LP
4.8.1
Exception Handling
.
.LP
4.8.2
Optimization
.
.RE
.
.RE
.
.sp
.
.LP
\s(18\fBSection 5: Medium Program Implementation\fR\s0
.
.sp
.
.RS
.
.LP
5.1
Introduction
.
.LP
5.2
Program Structure
.
.LP
5.3
Modular Structure
.
.LP
5.4
Interfaces
.
.LP
5.5
Subprograms Used
.
.LP
5.6
Parsing
.
.LP
5.7
Enumerations
.
.LP
5.8
Conculsion
.
.RE
.
.sp
.
.LP
\s(18\fBSection 6: Evaluation\fR\s0
.
.sp
.
.RS
.
.LP
6.1
Introduction
.
.LP
6.2
Simplicity/orthogonality
.
.LP
6.3
Control Structures
.
.LP
6.4
Data types
.
.LP
6.5
Syntax Design
.
.LP
6.6
Support for abstraction
.
.LP
6.7
Expressivity
.
.LP
6.8
Type checking
.
.LP
6.9
Exception handling
.
.LP
6.10
Restricted aliasing
.
.LP
6.11
Conclusion
.
.RE
.
.sp
.
.LP
\s(18\fBSection 7: Reflection\fR\s0
.
.sp
.
.RS
.
.LP
7.1
Introduction
.
.RE
.
.sp
.
.LP
\s(18\fBReferences\fR\s0
.
.sp
.LP
\s(18\fBAppendix A: MEDIUM IMPLEMENTATION SOURCE CODE\fR\s0
.
.
.bp
.
.\"introduction"
.so section1.ms
.
.bp
.\"history"
.so section2.ms
.
.bp
.\"tutoral"
.so section3.ms
.
.bp
.\"basic features"
.so section4.ms
.
.bp
.\"medium program implementation"
.so section5.ms
.
.bp
.\"evaluation"
.so section6.ms
.
.bp
.\"reflection"
.so section7.ms
.
.bp
.\"references"
.so references.ms
.
.bp
.
.\"\X'papersize=11i,8.5i'
.
.\".nr LL 9i
.\".pl 8.5i
.
.\"appendix A"
.so appendixA.ms
.
.\".nr LL 6.5i
.\".pl 11i
.
.\"\X'papersize=8.5i,11i'
.

