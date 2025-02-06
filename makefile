
SECTIONS = section1.ms section2.ms section3.ms section4.ms section5.ms section6.ms section7.ms references.ms hello.ms

semester_project.pdf: semester_project.ms $(SECTIONS)
	soelim semester_project.ms | tbl | eqn -T pdf | groff -U -ms -T pdf > semester_project.pdf

.PHONEY: clean

clean:
	@-rm semester_project.pdf

hello.ms: hello.f90
	source-highlight -f groff-mm-color -i hello.f90 -o hello.ms

