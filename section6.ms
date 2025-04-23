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
The goal of this section is to evaluate the Fortran programming language and observe its strengths and weaknesses in an isolated environment
regarding specific areas of its structure for the reader to understand the limitations and efficiencies
of the potential tools they get to work with if they take an interest to learning the language.
Another purpose of this section is to directly compare Fortran to other particular programming languages,
some of which may be more popular or used than Fortran as well as other niche languages within the same field;
this direct comparison allows the reader to visualize the strengths and weaknesses of Fortran beside programming languages they may know,
which highlights use cases for Fortran and gives reason to use the language in the present day.
.
.NH 2
Simplicity/Orthogonality
.
.PP
The first area of evaluation for Fortran is in regard to its simplicity
(easiness to learn and use in practice)
and orthogonality
(ability of different language features to be combined without restrictions),
and the programming language has had a constantly flipping relationship with both aspects ever since its inception.
Being the first high-level programming language,
Fortran\(cqs early years in the late 1950s meant its simplicity with syntax,
readability,
and learnability was unmatched,
especially compared to its main competition at this time still using assembly programming techniques and even binary to write code,
and this still remains the case even with the rest of the programming landscape
catching up with the focused language in regards to being high-level.
Of course,
the process of reading and writing Fortran code only grew easier with the implementation of free-form code in Fortran 90,
which allowed for more complex programs in regards to mathematical computations,
but cracks in its solid foundation still exist amidst all the upside,
such as the lack of dynamic memory allocation or recursion from inception and the still-relevant problem
of complex and cumbersome string handling,
which is made especially apparent when compared to other high-level programming languages
used today like C++ and Java.
Orthogonality was also a struggle for early Fortran with subroutines behaving differently than functions,
though this was cleaned up in later updates with the introduction of modular programming and generic programming.
Array and scalar operations in Fortran innately interact well with most mathematical operations,
which makes sense for a mathematical-based programming language,
but such innate cleanliness is left to be yearned by most other kinds of programs within the language
since it was never designed for OOP or major orthogonality functionality.
Overall,
Fortran is a simplistic language in regards to mathematical programming but struggles with non-mathematical programming,
and is more orthogonal today than in its earliest versions,
but leaves much to be desired when compared to other high-level programming language competition.
.
.NH 2
Control Structures
.
.PP
The second area of evaluation for Fortran is in regard to its control structures
(determines how decisions are made in programs according to certain conditions),
and the first real high-level programming language has an odd history with control structures.
Fortran was one of the first programming languages to have the conditional branch \*QIF\*U statement,
though this would only be a clunky and obsolete arithmetic version using labels to jump to certain lines
as the essential \*QTHEN-ELSE\*U portion of the fundamental structure was only added in the FORTRAN 77 update,
and it still remains this way to this day and reads almost identically to other languages with this branching statement.
Fortran also possesses strong counter and condition-controlled loops with \*QDO\*U and \*QDO WHILE\*U respectively reading
and functioning like \*Qfor\*U and \*Qwhile\*U in other languages,
but the \*QEXIT\*U and \*QCYCLE\*U break and continue statements
are somewhat verbose as opposed to another language using a singular \*Qbreak;\*U statement.
\*QSELECT CASE\*U is the Fortran switch case which supports numerical ranges
and remains readable like C switch statements while lacking fall-through unlike C switch statements.
While Fortran allows for Coarrays and \*QDO CONCURRENT\*U for parallel control and named loops and exits for code clarity,
the programming language struggles in the error handling department as it only has \*QERROR STOP\*U which terminates program execution;
the lack of a try-catch mechanism for errors found in its high-level language competitors is also a harsh look for Fortran.
Overall,
the control structures of Fortran play into its strengths as a numerical computation programming language,
but struggle when trying to write different kinds of programs.
.
.NH 2
Data Types
.
.PP
The third area of evaluation for Fortran is in regard to its data types
(determines the data a variable/object can hold),
and its purpose of being a high-level programming language built for mathematical computations means strong data types
are required for peak performance.
Primitive data types include \*QINTEGER\*U and \*QREAL\*U / \*QDOUBLE PRECISION\*U
(same as \*QREAL(KIND=8)\*U)
for whole numbers and floating points respectively
(use \*QKIND=#\*U to determine # of bytes allowed in numeric variable;
32-bit is default),
\*QCOMPLEX\*U for very rare innate complex number support,
\*QLOGICAL\*U for standard Boolean values,
and \*QCHARACTER\*U which takes in a non-dynamic length argument,
more closely resembling a string data type from other programming languages rather than a typical char data type for other languages.
Composite data types include memory-efficient and innate operation-supporting static arrays
and derived types with the \*QTYPE\*U keyword,
which acts like a struct in C-based programming languages where its main purpose is to organize data.
Some modern features Fortran has regarding data types includes manual dynamic memory allocation with the \*QALLOCATABLE\*U keyword for arrays,
object-oriented programming introduced in Fortran 2003 which includes classes,
inheritance,
and polymorphism,
all of which hold strange syntax when compared to other high-level programming languages;
and parameterized types for generic programming also introduced in Fortran 2003,
acting as a less flexible C\(enbased programming language template.
Fortran surprisingly lacks innate support for hash maps,
tuples (derived types are meant to serve as a replacement),
and algebraic data types such as \*QENUM\*U for Rust.
Overall,
Fortran is built to handle mathematical computations,
and this idea is only solidified by the wide variety of numerical data types
and possible functionality within them while all other data types are pains to work with,
especially compared to popular high-level programming languages today like C++ and Java.
.
.NH 2
Syntax Design
.
.PP
The fourth area of evaluation for Fortran is in regard to its syntax design (how language elements are arranged to create valid code), and the design has only improved over the years. From being fixed-format and column-strict at inception to the more modern free-format and indentation-friendly since Fortran 90, most of its syntax quirks have been ironed out over the years as the ancestor of high-level programming languages attempts to adapt to the modern day of programming, however some legacy oddities remain, like with variables starting with the characters ‘I’-‘N’ defaulting to integer data types while every other labeled variable defaulting to real and its non-intuitive I/O syntax which pales in comparison to its competitors Python and C, for example. Some strengths of Fortran’s syntax include case insensitivity for all versions, minimalistic structuring as no braces are required for code and end blocks are explicitly stated, and clean code for mathematical computations which extends to array operations being usable without loops. Some weaknesses of Fortran’s syntax besides its legacy quirks include its verbosity regarding keywords (“DO WHILE” instead of “WHILE”, for example), limitations with expressiveness which extend to a lack of inline assignments in conditions and list comprehensions, and unreliable OOP syntax introduced in Fortran 2003 which seems to be added just for the sake of adding something, as its syntax feels muddled and sluggish to read and write. Overall, the syntax design of Fortran emphasizes its specialty use in mathematical programming since writing equations and difficult numerical processing tasks is simple in this programming language, but it struggles to efficiently handle the syntax other, more general programs require.
.
.NH 2
Support for Abstraction
.
.PP
The fifth area of evaluation for Fortran is in regard to its support for abstraction (simplifying complex ideas by focusing on essentials instead of details), and since abstraction is a concept more modern than the high-level programming language created in 1957, Fortran has its struggles implementing such a key principle in programming. Procedural abstraction is simple and effective as subroutines execute code without returning values while functions return values and can be used in expressions, though this is where the highlight reel ends for Fortran. Data abstraction exists in derived types, which have already been discussed, and modules (“MODULE”) can group procedures together into reusable chunks with private and public fields for encapsulation, but the lack of similar namespaces hurt its abstraction. Object-oriented abstraction has also been covered already, and even if one can effectively pull off OOP syntax and strategies within Fortran, the labor is for unfavorable fruits as these techniques are usually avoided in Fortran due to verbosity and comprehensive sluggishness. Generic programming exists within Fortran, though it may only be useful for more simple programs as generic procedures to overload functions lack templates and the closest replacement to C++ templates, parameterized derived types, lack the flexibility of the former templates, even if this technique is usable for making numeric libraries. Some features which make abstraction simple, let alone possible, are absent in Fortran, including lambda expressions, iterators, mixins, and even exceptions for error handling. Overall, Fortran and abstraction do not mix, which makes sense considering its high-performance computing capabilities, but almost every other kind of professional program requires some kind of abstraction for readability and conciseness, making it a hard programming language to reasonably use for projects outside of its comfort zone.
.
.NH 2
Expressivity
.
.PP
The sixth area of evaluation for Fortran is in regard to its expressivity (scope of ideas which can be expressed through code), and the results of such an evaluation test when compared to the previous sections of this pseudo-programming language exam are unsurprising to anyone paying attention. With such a critical opening statement, Fortran should be given the luxury to showcase its predictable strengths first, as mathematical operations with simple arithmetic, array operations, matrix multiplication, and complex numbers have readability and potential other high-level programming languages could only wish to obtain even with the correct libraries. Expressivity is also increased in due part to minimum boilerplate code for numerical operations removing the requirement for explicit loops in array operations and the required use of “INTENT(IN/OUT/INOUT)” to improve clarity. Some weaknesses of Fortran in the expressivity department include more verbose control structures when compared to other high-level programming language competition, fixed string lengths with a lack of string methods which limits usability of characters and strings in program, a lack of lambda expressions and list comprehensions which increases boilerplate code for simple duties, and the well-discussed sluggish and clunky OOP syntax which is rarely used in the language. Overall, Fortran offers endless possibilities with simple syntax for numerical programs, meaning they claim the title as the most expressive program variation Fortran can create, beating out most other popular high-level languages in this category, but those same languages crush Fortran in every other aspect expressivitywise. Best described as domain-specific, Fortran is built to handle mathematical programs, and this is a title no other language can take from them, but this is the only real kind of program someone should use Fortran for.
.
.NH 2
Type Checking
.
.PP
The seventh area of evaluation for Fortran is in regard to its type checking capabilities (verifying the variable types follow the language rules), and this programming language set the precedent for other high-level programming languages to use static typing which runs these type checks at compile time. Unfortunately, implicit typing still exists as a legacy quirk and acts as a major source of bugs for older Fortran programs, though this can be bypassed by disabling these implicit types with the “IMPLICIT NONE” keyword. Regarding type safety, Fortran lacks default bounds checking and type casting loopholes with “EQUIVALENCE” exist, but these weaknesses are balanced out by the strength of explicit “INTENT(IN/OUT/INOUT)” procedures which increase clarity and “KIND” parameters for numeric types to determine their size. Fortran also lacks type inference for variables and only has basic overlading with “INTERFACE” and rarely-used polymorphic types with “CLASS”. Overall, the type checking ability of Fortran is inferior when designing for most other programs besides those utilizing high-performance computing, and even then these type checks are only so well-made, especially when compared to other programming language operating in the same domain as Fortran like Julia, let alone more popular programming languages like Python or Rust.
.
.NH 2
Execption Handling
.
.PP
The eighth area of evaluation for Fortran is in regard to its ability to handle exceptions (manage errors which occur during program execution), and the first mainstream high-level programming language has a bad start in this category considering its most basic exception handling keyword “ERROR STOP” immediately terminates the running program with an error message; other basic exceptions include the legacy “IOSTAT” keyword for file I/O checks which prevent crashing and IEEE floating-point exceptions which may be called by certain Fortran compilers. What Fortran lacks in this department is more telling, though, as try-catch code blocks, custom exceptions, and stack traces are all absent, and resource cleanup is manual was opposed to automatic with “WITH” code blocks in other high-level programming languages in addition to containing the preciously stated absent exception handling techniques. Of course, some common workarounds for these lacked method exist in a language as important as Fortran, like manually writing code to set flags off for errors, which is verbose but functional, and custom error logging into a log file which documents where in the program errors are taking place, although it lacks a recovery mechanism to make it an equal to the exception handling methods of other high-level languages. Overall, Fortran has an exception handling system which is too simple and too stiff for its own good, making other high-level programming languages seem generations ahead in error-checking efficiency and capability; exception handling in Fortran is easily its worst asset in this list of concepts the language is being evaluated from, and it may have the worst exception handling interface of all popular programming languages used in the modern day.
.
.NH 2
Restricted Aliasing
.
.PP
The ninth and final area of evaluation for Fortran is in regard to its restricted aliasing abilities (making sure two references do not access the same object/data), and Fortran ends on a high note as restricted aliasing is built into the programming language by design. Some important rules Fortran follows in regards to aliasing include the prohibition of pointer aliasing by default, which allows for the compiler to aggressively optimize due to not having to check for aliasing; explicit aliasing requiring the “TARGET” and “POINTER” keywords, which slows the program down since it knows to check for aliasing but remains optional since these keywords must be declared first; and “INTENT(IN/OUT/INOUT)” keywords clarifying variable usage to enforce non-aliasing. Some strengths of this restricted aliasing ruleset include better optimization from the lack of having to check for aliasing, which generates faster code and is essential in high-performance computing programs for vectorization and loop unrolling and other such techniques; safer code since aliasing bugs are eliminated due to the assumptions of no aliasing, and the “INTENT(IN/OUT/INOUT)” keyword making interfaces in Fortran self-documenting. Some drawbacks of this ruleset exist as well, including the lack of precision which is offered in C’s “RESTRICT” keyword for per-pointer opt-in aliasing, overhead pointers with explicit aliasing slowing the program down since aliasing needs to be checked for by the compiler, and certain legacy code like the “EQUIVALENCE” keyword forcing aliasing, though this issue can be avoided if writing a Fortran problem in the modern day by not using legacy statements. Overall, restricted aliasing is the key ingredient in making Fortran the dominant force in the HPC field today, as its nature of assuming no aliasing allows for optimal runtime in very numerically complex programs, and even if a few downsides like a lack of granularity or inconvenient legacy codes weight it down, Fortran still has arguably the best system for restricted aliasing when compared to other popular high-level programming languages. 
.
.NH 2
Conculsion
.
.PP
A thorough evaluation of a programming language is performed to observe how it operates not only within its individual scope, but also when compared to other programming languages which pine for the attention of programmers all over the world; as a programming language, one needs to stand out and offer services and potential no other programming language can claim to possess, and despite being be grandfather of all high-level programming languages defining the world of technology and programming today, Fortran possesses these traits to stand out above the rest of the competition, albeit in a niche but vitally important field. Part of why Fortran has been so successful in the scientific programming and HPC fields is due to its powerful innate numerical data types, vast expressivity with mathematical programs and operations, and strong restricted aliasing rules built into the programming language for the purpose of maximizing high-performance computing potential. These factors combine together to make Fortran an unkillable beast in the computational programming domain, though investing so much in only one field makes the high-level programming language more of a specialist, if anything, as every other area evaluated for Fortran not related to engineering, numerical computations, or HPC, is at best performed better by other languages and at worst infamously unusable in practice when compared to its programming language peers. The lowest lows for Fortran include nonexistent useful innate exception handling, abstraction support going as far as Fortran saying to its programmers \*QI don’t\*U, and the added burdens of quirks related to all other areas not covered here which are written and performed easier by other languages, like using strings in a program or competent type checking performed by the compiler.
	A final verdict for Fortran is tough to give, an act comparable to grading a fish on how well it can walk on land. A fish lacks the capabilities and design to effectively live on land, let alone walk, since the fish is in the wrong environment; if one is to grade the fish on how well it swims in water, then the fish will receive marks unobtainable by anyone else within this metaphorical class. Fortran is the fish, and while the gill-havers get an entire ocean to claim rule over, scientific programming and HPC is more comparable to a very important lake in the vibrant world of computer programming. When Fortran is used in a high-performance computing environment, its final grade is an undisputable 9/10, only being weighed down by the few flaws its expertly crafted features have; when Fortran is used in a program more akin to one resembling a user interface or web application or any program besides ones the historic programming language was designed for, then its final grade is a very generous 3.5/10, as the hardest challenge one will have to face with Fortran while coding a general program like this is why one would even use this language to begin with as opposed to Python with the numPy library or Java or C++ with their accessibility and outside resources. In conclusion, use Fortran for what it was designed to create and work with: scientific programming, numerical computations, and high-performance computing.

.
.

