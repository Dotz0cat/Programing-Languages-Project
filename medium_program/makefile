FC = gfortran
CFLAGS = -O2 -g -Wall -Wextra
CPPFLAGS = -I $(MODDIR)
#LDLIBS

SRCDIR = src
MODDIR = mod
OBJDIR = obj

build: $(OBJDIR) $(MODDIR) program

program: $(OBJDIR)/input.o $(OBJDIR)/input_implations.o $(OBJDIR)/processing.o $(OBJDIR)/processing_sub.o $(OBJDIR)/main.o
	$(FC) $(CFLAGS) $(CPPFLAGS) $^ -o $@ $(LDLIBS)

$(OBJDIR):
	mkdir $(OBJDIR)

$(MODDIR):
	mkdir $(MODDIR)

$(OBJDIR)/%.o: $(SRCDIR)/%.f90
	$(FC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@ -J $(MODDIR)

.PHONEY: clean

clean:
	-rm -r $(OBJDIR)
	-rm -r $(MODDIR)
	-rm program

