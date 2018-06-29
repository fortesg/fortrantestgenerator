FTGROOT = $(HOME)/workspace/fortrantestgenerator
SBROOT = $(HOME)/lib/serialbox
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

capture-standalone: clean ftg-standalone maingeneric 
	-mpirun -n $(MPINODES) ./maingeneric

capture-standalone_nompi: clean ftg-standalone_nompi maingeneric 
	./maingeneric

capture-IconStandalone: clean ftg-IconStandalone mainicon 
	-mpirun -n $(MPINODES) ./mainicon

capture-IconTestbed: clean ftg-IconTestbed mainicon 
	-mpirun -n $(MPINODES) ./mainicon

capture-icon_standalone_legacy: LIBS = -L$(SBROOT)/lib -lFortranSer -lSerialboxWrapper -lSerialbox -lUtils -ljson -lstdc++ -lsha256
capture-icon_standalone_legacy: INCLUDE = -I$(SBROOT)/include/fortran
capture-icon_standalone_legacy: clean ftg-icon_standalone_legacy mainicon 
	-mpirun -n $(MPINODES) ./mainicon

maingeneric: maingeneric.f90 sub.o types.o globals.o
	$(FF) -o $@ $(FFLAGS) $(INCLUDE) $^ $(LIBS)
	
mainicon: mainicon.f90 sub.o types.o globals.o
	$(FF) -o $@ $(FFLAGS) $(INCLUDE) $(ICONINCLUDE) $^ $(ICONMODULES) $(LIBS)
	
sub.o : types.o globals.o

ftg-standalone: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_standalone.py sub testsub

ftg-standalone_nompi: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_standalone_nompi.py sub testsub

ftg-IconStandalone: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_IconStandalone.py sub testsub

ftg-IconTestbed: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_IconTestbed.py sub testsub

ftg-icon_standalone_legacy: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_icon_standalone_legacy.py sub testsub

assembler: globals.s types.s sub.s maingeneric.s 

%.s: %.f90
	$(FF) -c -S $(FFLAGS) $<	
	
%.o: %.f90
	$(FF) -c $(FFLAGS) $(INCLUDE) $(ICONINCLUDE) $<
	
clean:
	$(FTGROOT)/FortranTestGenerator.py -b -cf ../config_fortrantestgenerator_test_standalone.py
	rm -f maingeneric mainicon *.o *.mod *.s
	
clean-o:
	rm -f *.o
	
.PHONY: clean seticonvars
	