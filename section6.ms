.
.EQ
delim $$
.EN
.
.NH 1 6
Evaluation
.
.NH 2
Introduction
.
.PP
The goal of this section is to evaluate the Fortran programming language and observe its strengths and weaknesses in an isolated environment regarding specific areas of its structure for the reader to understand the limitations and efficiencies of the potential tools they get to work with if they take an interest to learning the language. Another purpose of this section is to directly compare Fortran to other particular programming languages, some of which may be more popular or used than Fortran as well as other niche languages within the same field; this direct comparison allows the reader to visualize the strengths and weaknesses of Fortran beside programming languages they may know, which highlights use cases for Fortran and gives reason to use the language in the present day.
.
.NH 2
Simplicity/Orthogonality
.
.PP
The first area of evaluation for Fortran is in regard to its simplicity (easiness to learn and use in practice) and orthogonality (ability of different language features to be combined without restrictions), and the programming language has had a constantly flipping relationship with both aspects ever since its inception. Being the first high-level programming language, Fortran’s early years in the late 1950s meant its simplicity with syntax, readability, and learnability was unmatched, especially compared to its main competition at this time still using assembly programming techniques and even binary to write code, and this still remains the case even with the rest of the programming landscape catching up with the focused language in regards to being high-level. Of course, the process of reading and writing Fortran code only grew easier with the implementation of free-form code in Fortran 90, which allowed for more complex programs in regards to mathematical computations, but cracks in its solid foundation still exist amidst all the upside, such as the lack of dynamic memory allocation or recursion from inception and the still-relevant problem of complex and cumbersome string handling, which is made especially apparent when compared to other high-level programming languages used today like C++ and Java. Orthogonality was also a struggle for early Fortran with subroutines behaving differently than functions, though this was cleaned up in later updates with the introduction of modular programming and generic programming. Array and scalar operations in Fortran innately interact well with most mathematical operations, which makes sense for a mathematical-based programming language, but such innate cleanliness is left to be yearned by most other kinds of programs within the language since it was never designed for OOP or major orthogonality functionality. Overall, Fortran is a simplistic language in regards to mathematical programming but struggles with non-mathematical programming, and is more orthogonal today than in its earliest versions, but leaves much to be desired when compared to other high-level programming language competition.
.
.NH 2
Control Structures
.
.PP
The second area of evaluation for Fortran is in regard to its control structures (determines how decisions are made in programs according to certain conditions), and the first real high-level programming language has an odd history with control structures. Fortran was one of the first programming languages to have the conditional branch “IF” statement, though this would only be a clunky and obsolete arithmetic version using labels to jump to certain lines as the essential “THEN-ELSE” portion of the fundamental structure was only added in the FORTRAN 77 update, and it still remains this way to this day and reads almost identically to other languages with this branching statement. Fortran also possesses strong counter and condition-controlled loops with “DO” and “DO WHILE” respectively reading and functioning like “for” and “while” in other languages, but the “EXIT” and “CYCLE” break and continue statements are somewhat verbose as opposed to another language using a singular “break;” statement. “SELECT CASE” is the Fortran switch case which supports numerical ranges and remains readable like C switch statements while lacking fall-through unlike C switch statements. While Fortran allows for Coarrays and “DO CONCURRENT” for parallel control and named loops and exits for code clarity, the programming language struggles in the error handling department as it only has “ERROR STOP” which terminates program execution; the lack of a try-catch mechanism for errors found in its high-level language competitors is also a harsh look for Fortran. Overall, the control structures of Fortran play into its strengths as a numerical computation programming language, but struggle when trying to write different kinds of programs.
.
.NH 2
Data Types
.
.PP
The third area of evaluation for Fortran is in regard to its data types (determines the data a variable/object can hold), and its purpose of being a high-level programming language built for mathematical computations means strong data types are required for peak performance. Primitive data types include “INTEGER” and “REAL”/”DOUBLE PRECISION” (same as “REAL(KIND=8)”) for whole numbers and floating points respectively (use “KIND=#” to determine # of bytes allowed in numeric variable; 32-bit is default), “COMPLEX” for very rare innate complex number support, “LOGICAL” for standard Boolean values, and “CHARACTER” which takes in a non-dynamic length argument, more closely resembling a string data type from other programming languages rather than a typical char data type for other languages. Composite data types include memory-efficient and innate operation-supporting static arrays and derived types with the “TYPE” keyword, which acts like a struct in C-based programming languages where its main purpose is to organize data. Some modern features Fortran has regarding data types includes manual dynamic memory allocation with the “ALLOCATABLE” keyword for arrays, object-oriented programming introduced in Fortran 2003 which includes classes, inheritance, and polymorphism, all of which hold strange syntax when compared to other high-level programming languages; and parameterized types for generic programming also introduced in Fortran 2003, acting as a less flexible C-based programming language template. Fortran surprisingly lacks innate support for hash maps, tuples (derived types are meant to serve as a replacement), and algebraic data types such as ”ENUM” for Rust. Overall, Fortran is built to handle mathematical computations, and this idea is only solidified by the wide variety of numerical data types and possible functionality within them while all other data types are pains to work with, especially compared to popular high-level programming languages today like C++ and Java.
.
.NH 2
Syntax Design
.
.PP
Stuff
.
.NH 2
Support for Abstraction
.
.PP
Stuff
.
.NH 2
Expressivity
.
.PP
Stuff
.
.NH 2
Type Checking
.
.PP
Stuff
.
.NH 2
Execption Handling
.
.PP
Stuff
.
.NH 2
Restricted Aliasing
.
.PP
Stuff
.
.NH 2
Conculsion
.
.PP
Stuff
.
.

