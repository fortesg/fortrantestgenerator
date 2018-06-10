FTGROOT = $(HOME)/workspace/fortrantestgenerator
SBROOT = $(HOME)/lib/serialbox2
ICONBUILDDIR = $(HOME)/workspace/icon/build/x86_64-unknown-linux-gnu

FF = mpif90
FFLAGS = -g -O0
LIBS = -L$(SBROOT)/lib  -lSerialboxCore -lSerialboxC -lSerialboxFortran -lstdc++
INCLUDE = -I$(SBROOT)/include
ICONLIBS = -L$(ICONBUILDDIR)/src
MPINODES = 2 

######################################################

capture-standalone: clean ftg-standalone maingeneric 
	-mpirun -n $(MPINODES) ./maingeneric

capture-standalone_nompi: clean ftg-standalone_nompi maingeneric 
	./maingeneric

capture-icon_standalone: ICONINCLUDE = -I$(ICONBUILDDIR)/module 
capture-icon_standalone: ICONLIBS = 
capture-icon_standalone: ICONMODULES = $(ICONBUILDDIR)/src/mo_mpi.o $(ICONBUILDDIR)/src/mo_exception.o $(ICONBUILDDIR)/support/util_backtrace.o $(ICONBUILDDIR)/support/util_system.o $(ICONBUILDDIR)/externals/mtime/src/*.o
capture-icon_standalone: clean ftg-icon_standalone maingeneric 
	-mpirun -n $(MPINODES) ./maingeneric

maingeneric: maingeneric.f90 sub.o types.o globals.o
	$(FF) -o $@ $(FFLAGS) $(INCLUDE) $(ICONINCLUDE) $^ $(ICONMODULES) $(LIBS) $(ICONLIBS)
	
sub.o : types.o globals.o

ftg-standalone: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_standalone.py sub testsub

ftg-standalone_nompi: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_standalone_nompi.py sub testsub

ftg-icon_standalone: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_icon_standalone.py sub testsub

assembler: globals.s types.s sub.s maingeneric.s 

%.s: %.f90
	$(FF) -c -S $(FFLAGS) $<	
	
%.o: %.f90
	$(FF) -c $(FFLAGS) $(INCLUDE) $(ICONINCLUDE) $<
	
clean:
	$(FTGROOT)/FortranTestGenerator.py -b -cf ../config_fortrantestgenerator_test_standalone.py
	rm -f maingeneric *.o *.mod *.s
	
clean-o:
	rm -f *.o
	
.PHONY: clean seticonvars
	