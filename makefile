
SECTIONS = section1.ms section2.ms section3.ms \
		   section4.ms section5.ms section6.ms \
		   section7.ms references.ms appendixA.ms

PREPARED_SOURCES = hello.ms program_contains.ms basic_mod.ms \
				   use_mod.ms subroutine.ms function.ms function_alt.ms \
				   interface.ms interface_overload.ms submodule.ms \
				   numbers_p.ms arrays.ms allocatable_array.ms \
				   derived_type.ms derived_type_use.ms derived_type_assign.ms \
				   sub_mod_medium.ms mod_medium.ms interface_medium.ms \
				   procedure_medium.ms divide_numbers.ms

PREPARED_MEDIUM_SOURCES = main.ms input.ms input_implations.ms processing.ms \
						  processing_sub.ms

SRCSDIR = code_examples
MEDIUM_DIR = medium_program/src

semester_project.pdf: semester_project.ms $(SECTIONS) $(PREPARED_SOURCES) \
	$(PREPARED_MEDIUM_SOURCES)
	soelim semester_project.ms | tbl | eqn -T pdf | groff -U -ms -T pdf > semester_project.pdf

.PHONEY: clean

clean:
	@-rm semester_project.pdf

%.ms: $(SRCSDIR)/%.f90
	source-highlight -f groff_mm_color -i $< -o $@

%.ms: $(MEDIUM_DIR)/%.f90
	source-highlight -f groff_mm_color -i $< -o $@

