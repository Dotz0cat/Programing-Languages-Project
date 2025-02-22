FC = gfortran
CFLAGS = -O2 -g -Wall -Wextra
CPPFLAGS = -I $(MODDIR)
#LDLIBS

SRCDIR = src
MODDIR = mod
OBJDIR = obj

build: $(OBJDIR) $(MODDIR) $(OBJDIR)/main.o
	$(FC) $(CFLAGS) $(CPPFLAGS) $(OBJDIR)/main.o -o program $(LDLIBS)

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

