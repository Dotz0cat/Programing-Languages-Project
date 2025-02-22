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
Program Listing of \f(CW\$1\fR
.
...
.ec
.
.eo
.de SOURCE ..
.
.TS H
doublebox;
Lx.
.TH
T{
.gcolor black
. so \$1

T}
.gcolor black
.TE
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
\&
.sp 8
.
.LP
.ce
.
\s(18\fBA Report on the Fortran Programming Language\fR\s0
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
Introduction \(em What is the goal of this section?
.
.LP
2.2
Creation
Who created the language?
Why was the language created?
What application domains is the language good for?
Why?
.
.LP
2.3
Lifetime
What has happened to the language since it was created?
Rate the popularity of the language, then, now, and in the future.
What is your evidence for this rating?
.
.LP
2.4
Current status in Industry
What are major projects/companies using it
How many jobs have proficiency in this as necessary or preferred 
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
Introduction \(em What is the goal of this section?
.
.LP
3.2
How Do you create a basic hello world program?
Include a description of how you use the Programming environment to edit,
build,
and run your program.
.
.LP
3.3 \(em 3.X
One section for each major feature
.
.LP
3.X+1
Resources \(em What books/manuals/web sites should I go to for further 
information about the language?
Make sure you cite the material here and include an entry in the bibliography.
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
Introduction \(em What is the goal of this section?
.
.LP
4.2
Paradigm
.
.LP
4.3
Data Types
.
.LP
4.4
Control Flow
.
.LP
4.5
Expressions
.
.LP
4.6
Subprograms
.
.LP
4.7
Object-orientation
.
.LP
4.8
Exception Handling and Optimization 
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
Introduction \(em What is the goal of this section?
What was the goal of the program you wrote?
.
.LP
5.2-5.x
Describe the basic design of the program.
How did you leverage the major language features to implement the design?
.
.LP
5.x+1
Conclusion \(em What was great about implementing the program in your language?
What was awful about implementing the program in your language?
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
Introduction \(em What is the goal of this section?
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
Conclusion \(em what is your overall feeling about the language?
Compared to your language would it have been easier or harder to implement your medium-sized program in C++ and WHY?
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
Introduction \(em What is the goal of this section?
.
.RE
.
.sp
.
.LP
\s(18\fBReferences\fR\s0
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
.\"apendix here if needed"
.
.

