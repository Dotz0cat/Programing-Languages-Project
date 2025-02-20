
SECTIONS = section1.ms section2.ms section3.ms section4.ms section5.ms section6.ms section7.ms references.ms

PREPARED_SOURCES = hello.ms program_contains.ms basic_mod.ms use_mod.ms

semester_project.pdf: semester_project.ms $(SECTIONS) $(PREPARED_SOURCES)
	soelim semester_project.ms | tbl | eqn -T pdf | groff -U -ms -T pdf > semester_project.pdf

.PHONEY: clean

clean:
	@-rm semester_project.pdf

hello.ms: hello.f90
	source-highlight -f groff_mm_color -i hello.f90 -o hello.ms

program_contains.ms: program_contains.f90
	source-highlight -f groff_mm_color -i program_contains.f90 -o program_contains.ms

basic_mod.ms: basic_mod.f90
	source-highlight -f groff_mm_color -i basic_mod.f90 -o basic_mod.ms

use_mod.ms: use_mod.f90
	source-highlight -f groff_mm_color -i use_mod.f90 -o use_mod.ms
