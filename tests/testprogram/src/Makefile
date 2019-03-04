FTGROOT = $(HOME)/workspace/fortrantestgenerator
SB2ROOT = $(HOME)/lib/serialbox2
ICONBUILDDIR = $(HOME)/workspace/icon/build/x86_64-unknown-linux-gnu
FF = $(HOME)/lib/mpich/bin/mpif90

FFLAGS = -cpp -g -O0 -fopenmp -D__FTG__
LIBS = -L$(SB2ROOT)/lib  -lSerialboxFortranStatic -lSerialboxCStatic  -lSerialboxStatic -lstdc++ -lstdc++fs
INCLUDE = -I$(SB2ROOT)/include
ICONINCLUDE = -I$(ICONBUILDDIR)/module 
ICONMODULES = $(ICONBUILDDIR)/src/mo_mpi.o $(ICONBUILDDIR)/src/mo_exception.o $(ICONBUILDDIR)/support/util_backtrace.o $(ICONBUILDDIR)/support/util_system.o $(ICONBUILDDIR)/externals/mtime/src/*.o
MPINODES = 2 

GENERIC_BIN = ../bin/generic
ICON_BIN = ../bin/icon

######################################################

capture-Standalone: clean ftg-Standalone $(GENERIC_BIN) 
	-mpirun -n $(MPINODES) $(GENERIC_BIN)

capture-StandaloneNoMPI: clean ftg-StandaloneNoMPI $(GENERIC_BIN) 
	$(GENERIC_BIN)

capture-IconStandalone: clean ftg-IconStandalone $(ICON_BIN) 
	-mpirun -n $(MPINODES) $(ICON_BIN)

capture-IconJsbachMock: clean ftg-IconJsbachMock $(ICON_BIN) 
	-mpirun -n $(MPINODES) $(ICON_BIN)
	
capture-BaseCompare: clean ftg-BaseCompare $(GENERIC_BIN) 
	-mpirun -n $(MPINODES) $(GENERIC_BIN)

$(GENERIC_BIN): maingeneric.f90 sub.o globals.o types.o
	$(FF) -o $@ $(FFLAGS) $(INCLUDE) $^ $(LIBS)
	
$(ICON_BIN): mainicon.f90 sub.o globals.o types.o
	$(FF) -o $@ $(FFLAGS) $(INCLUDE) $(ICONINCLUDE) $^ $(ICONMODULES) $(LIBS)
	
sub.o: types.o globals.o

globals.o: types.o

ftg-Standalone: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_Standalone.py sub testsub

ftg-StandaloneNoMPI: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_StandaloneNoMPI.py sub testsub

ftg-IconStandalone: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_IconStandalone.py sub testsub

ftg-IconJsbachMock: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_IconJsbachMock.py sub testsub

ftg-BaseCompare: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_BaseCompare.py sub testsub

assembler: types.s globals.s sub.s maingeneric.s 

%.s: %.f90
	$(FF) -c -S $(FFLAGS) $<	
	
%.o: %.f90
	$(FF) -c $(FFLAGS) $(INCLUDE) $(ICONINCLUDE) $<
	
clean: clean-ftg-b
	rm -f $(GENERIC_BIN) $(ICON_BIN) *.o *.mod *.s
	
clean-capture-icon: clean-ftg-a 
	$(MAKE) -B $(ICON_BIN)
clean-capture-generic: clean-ftg-a 
	$(MAKE) -B $(GENERIC_BIN)
	
clean-ftg-b:
	$(FTGROOT)/FortranTestGenerator.py -b -cf ../config_fortrantestgenerator_test_Standalone.py
	
clean-ftg-a:
	$(FTGROOT)/FortranTestGenerator.py -a -cf ../config_fortrantestgenerator_test_Standalone.py
	
clean-o:
	rm -f *.o
	
.PHONY: clean clean-capture-icon clean-capture-generic clean-ftg-b clean-ftg-a seticonvars
	