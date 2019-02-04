FTGROOT = $(HOME)/workspace/fortrantestgenerator
SB2ROOT = $(HOME)/lib/serialbox2
ICONBUILDDIR = $(HOME)/workspace/icon/build/x86_64-unknown-linux-gnu
FF = $(HOME)/lib/mpich/bin/mpif90

FFLAGS = -cpp -g -O0 -fopenmp
LIBS = -L$(SB2ROOT)/lib  -lSerialboxFortranStatic -lSerialboxCStatic  -lSerialboxStatic -lstdc++ -lstdc++fs
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

capture-IconCompare: clean ftg-IconCompare mainicon 
	-mpirun -n $(MPINODES) ./mainicon

capture-BaseCompare: clean ftg-BaseCompare mainicon 
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

ftg-IconCompare: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_IconCompare.py sub testsub

ftg-BaseCompare: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_BaseCompare.py sub testsub

assembler: globals.s types.s sub.s maingeneric.s 

%.s: %.f90
	$(FF) -c -S $(FFLAGS) $<	
	
%.o: %.f90
	$(FF) -c $(FFLAGS) $(INCLUDE) $(ICONINCLUDE) $<
	
clean: clean-ftg-b
	rm -f maingeneric mainicon *.o *.mod *.s
	
clean-capture-icon: clean-ftg-a 
	$(MAKE) -B mainicon
clean-capture-generic: clean-ftg-a 
	$(MAKE) -B maingeneric
	
clean-ftg-b:
	$(FTGROOT)/FortranTestGenerator.py -b -cf ../config_fortrantestgenerator_test_Standalone.py
	
clean-ftg-a:
	$(FTGROOT)/FortranTestGenerator.py -a -cf ../config_fortrantestgenerator_test_Standalone.py
	
clean-o:
	rm -f *.o
	
.PHONY: clean clean-capture-icon clean-capture-generic clean-ftg-b clean-ftg-a seticonvars
	