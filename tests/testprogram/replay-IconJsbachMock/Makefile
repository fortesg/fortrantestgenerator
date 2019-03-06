FTG = $(HOME)/workspace/fortrantestgenerator/FortranTestGenerator.py
SBROOT = $(HOME)/lib/serialbox2
ICONBUILDDIR = $(HOME)/workspace/icon/build/x86_64-unknown-linux-gnu
FF = $(HOME)/lib/mpich/bin/mpif90

FFLAGS = -g -O0 -fopenmp
LIBS = -L$(SBROOT)/lib -lSerialboxFortranStatic -lSerialboxCStatic  -lSerialboxStatic -lstdc++ -lstdc++fs -L$(ICONBUILDDIR)/src
INCLUDE = -I$(SBROOT)/include -I../src
ICONLIBS = 
ICONINCLUDE = -I$(ICONBUILDDIR)/module
ICONMODULES = $(ICONBUILDDIR)/src/mo_mpi.o $(ICONBUILDDIR)/src/mo_exception.o $(ICONBUILDDIR)/support/util_backtrace.o $(ICONBUILDDIR)/support/util_system.o $(ICONBUILDDIR)/externals/mtime/src/*.o
MPINODES = 2 

######################################################

replay: ftg_testsub_test
	mpirun -n $(MPINODES) ./ftg_testsub_test

ftg_testsub_test: ftg_testsub_test.f90 ../src/sub.o ../src/types.o ../src/globals.o
	$(FF) -o $@ $(FFLAGS) $(INCLUDE) $(ICONINCLUDE) $^ $(ICONMODULES) $(LIBS) $(ICONLIBS)

ftg_testsub_test.f90: 
	$(FTG) -rm -cf ../config_fortrantestgenerator_test_IconJsbachMock.py sub testsub
	
clean:
	rm -f ftg_testsub_test *.f90
	
.PHONY: clean replay