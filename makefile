
SECTIONS = section1.ms section2.ms section3.ms section4.ms section5.ms section6.ms section7.ms references.ms

PREPARED_SOURCES = hello.ms program_contains.ms basic_mod.ms use_mod.ms subroutine.ms function.ms function_alt.ms interface.ms interface_overload.ms

SRCSDIR = code_examples

semester_project.pdf: semester_project.ms $(SECTIONS) $(PREPARED_SOURCES)
	soelim semester_project.ms | tbl | eqn -T pdf | groff -U -ms -T pdf > semester_project.pdf

.PHONEY: clean

clean:
	@-rm semester_project.pdf

%.ms: $(SRCSDIR)/%.f90
	source-highlight -f groff_mm_color -i $< -o $@

