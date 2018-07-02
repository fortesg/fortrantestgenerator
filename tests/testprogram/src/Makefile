FTGROOT = $(HOME)/workspace/fortrantestgenerator
SB2ROOT = $(HOME)/lib/serialbox2
ICONBUILDDIR = $(HOME)/workspace/icon/build/x86_64-unknown-linux-gnu

FF = mpif90
FFLAGS = -cpp -g -O0
LIBS = -L$(SB2ROOT)/lib  -lSerialboxCore -lSerialboxC -lSerialboxFortran -lstdc++
INCLUDE = -I$(SB2ROOT)/include
ICONINCLUDE = -I$(ICONBUILDDIR)/module 
ICONMODULES = $(ICONBUILDDIR)/src/mo_mpi.o $(ICONBUILDDIR)/src/mo_exception.o $(ICONBUILDDIR)/support/util_backtrace.o $(ICONBUILDDIR)/support/util_system.o $(ICONBUILDDIR)/externals/mtime/src/*.o
MPINODES = 2 

######################################################

capture-Standalone: clean ftg-Standalone maingeneric 
	-mpirun -n $(MPINODES) ./maingeneric

capture-StandaloneNoMPI: clean ftg-StandaloneNoMPI maingeneric 
	./maingeneric

capture-IconStandalone: clean ftg-IconStandalone mainicon 
	-mpirun -n $(MPINODES) ./mainicon

capture-IconTestbed: clean ftg-IconTestbed mainicon 
	-mpirun -n $(MPINODES) ./mainicon

maingeneric: maingeneric.f90 sub.o types.o globals.o
	$(FF) -o $@ $(FFLAGS) $(INCLUDE) $^ $(LIBS)
	
mainicon: mainicon.f90 sub.o types.o globals.o
	$(FF) -o $@ $(FFLAGS) $(INCLUDE) $(ICONINCLUDE) $^ $(ICONMODULES) $(LIBS)
	
sub.o : types.o globals.o

ftg-Standalone: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_Standalone.py sub testsub

ftg-StandaloneNoMPI: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_StandaloneNoMPI.py sub testsub

ftg-IconStandalone: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_IconStandalone.py sub testsub

ftg-IconTestbed: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_IconTestbed.py sub testsub

assembler: globals.s types.s sub.s maingeneric.s 

%.s: %.f90
	$(FF) -c -S $(FFLAGS) $<	
	
%.o: %.f90
	$(FF) -c $(FFLAGS) $(INCLUDE) $(ICONINCLUDE) $<
	
clean:
	$(FTGROOT)/FortranTestGenerator.py -b -cf ../config_fortrantestgenerator_test_Standalone.py
	rm -f maingeneric mainicon *.o *.mod *.s
	
clean-o:
	rm -f *.o
	
.PHONY: clean seticonvars
	