
SECTIONS = section1.ms section2.ms section3.ms \
		   section4.ms section5.ms section6.ms \
		   section7.ms references.ms appendixA.ms

PREPARED_SOURCES = $(addprefix $(PREPARED_DIR)/section3/, \
				   hello.ms program_contains.ms basic_mod.ms \
				   use_mod.ms subroutine.ms function.ms function_alt.ms \
				   interface.ms interface_overload.ms submodule.ms \
				   numbers_p.ms arrays.ms allocatable_array.ms \
				   derived_type.ms derived_type_use.ms derived_type_assign.ms ) \
				   $(addprefix $(PREPARED_DIR)/section4/, \
				   if_then.ms if_elif.ms if_else.ms select_case.ms \
				   select_range.ms select_case_ranges.ms \
				   do.ms do_step.ms do_while_eqiv.ms depracated_while.ms) \
				   $(addprefix $(PREPARED_DIR)/section5/, \
				   sub_mod_medium.ms mod_medium.ms interface_medium.ms \
				   procedure_medium.ms grammar.ms )

PREPARED_MEDIUM_SOURCES = $(addprefix $(PREPARED_DIR)/medium/, \
						  main.ms input.ms input_implations.ms action_mod.ms \
						  token.ms terminal_mod.ms state_mod.ms \
						  parse_mod.ms parse_sub.ms ast_mod.ms ast_sub.ms \
						  stack_mod.ms stack_sub.ms )

SRCSDIR = code_examples
MEDIUM_DIR = medium_program/src

PREPARED_DIR = prepared_code_examples

INCLUDEDIRS = $(PREPARED_DIR)/medium $(PREPARED_DIR)/section3 $(PREPARED_DIR)/section4 $(PREPARED_DIR)/section5
IFLAGS = $(addprefix -I, $(INCLUDEDIRS))

semester_project.pdf: semester_project.ms $(SECTIONS) $(PREPARED_SOURCES) \
	$(PREPARED_MEDIUM_SOURCES)
	soelim semester_project.ms | tbl | eqn -T pdf | groff -U $(IFLAGS) -ms -T pdf > semester_project.pdf

.PHONEY: clean

clean:
	@-rm -r $(PREPARED_DIR)
	@-rm semester_project.pdf

$(PREPARED_DIR)/section3/%.ms: $(SRCSDIR)/section3/%.f90 | $(PREPARED_DIR)/section3
	source-highlight -f groff_ms_color -i $< -o $@ --outlang-def=groff_ms_color.outlang

$(PREPARED_DIR)/section4/%.ms: $(SRCSDIR)/section4/%.f90 | $(PREPARED_DIR)/section4
	source-highlight -f groff_ms_color -i $< -o $@ --outlang-def=groff_ms_color.outlang

$(PREPARED_DIR)/section5/%.ms: $(SRCSDIR)/section5/%.f90 | $(PREPARED_DIR)/section5
	source-highlight -f groff_ms_color -i $< -o $@ --outlang-def=groff_ms_color.outlang

$(PREPARED_DIR)/section5/%.ms: $(SRCSDIR)/section5/%.y | $(PREPARED_DIR)/section5
	source-highlight -f groff_ms_color -i $< -o $@ --outlang-def=groff_ms_color.outlang

$(PREPARED_DIR)/medium/%.ms: $(MEDIUM_DIR)/%.f90 | $(PREPARED_DIR)/medium
	source-highlight -f groff_ms_color -i $< -o $@ --outlang-def=groff_ms_color.outlang

$(PREPARED_DIR):
	mkdir $(PREPARED_DIR)

$(PREPARED_DIR)/section3: | $(PREPARED_DIR)
	mkdir $@

$(PREPARED_DIR)/section4: | $(PREPARED_DIR)
	mkdir $@

$(PREPARED_DIR)/section5: | $(PREPARED_DIR)
	mkdir $@

$(PREPARED_DIR)/medium: | $(PREPARED_DIR)
	mkdir $@
