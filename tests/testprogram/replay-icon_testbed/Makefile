FTGROOT = $(HOME)/workspace/fortrantestgenerator
SBROOT = $(HOME)/lib/serialbox2
ICONBUILDDIR = $(HOME)/workspace/icon/build/x86_64-unknown-linux-gnu

FF = mpif90
FFLAGS = -g -O0
LIBS = -L$(SBROOT)/lib -lSerialboxCore -lSerialboxC -lSerialboxFortran -lstdc++ -L$(ICONBUILDDIR)/src
INCLUDE = -I$(SBROOT)/include -I../src
ICONLIBS = 
ICONINCLUDE = -I$(ICONBUILDDIR)/module
ICONMODULES = $(ICONBUILDDIR)/src/mo_mpi.o $(ICONBUILDDIR)/src/mo_exception.o $(ICONBUILDDIR)/support/util_backtrace.o $(ICONBUILDDIR)/support/util_system.o $(ICONBUILDDIR)/externals/mtime/src/*.o
MPINODES = 2 

######################################################

replay: testbed
	-mpirun -n $(MPINODES) ./testbed

testbed: testbed.f90 mo_ftg_testsub_test.o ../src/sub.o ../src/types.o ../src/globals.o
	$(FF) -o $@ $(FFLAGS) $(INCLUDE) $(ICONINCLUDE) $^ $(ICONMODULES) $(LIBS) $(ICONLIBS)

mo_ftg_testsub_test.o: mo_ftg_testsub_test.f90 
	$(FF) -c $(FFLAGS) $(INCLUDE) $(ICONINCLUDE) $^

mo_ftg_testsub_test.f90: 
	$(FTGROOT)/FortranTestGenerator.py -r -cf ../config_fortrantestgenerator_test_icon_testbed.py sub testsub
	
clean:
	rm -f ftg_testsub_test *.f90
	
.PHONY: clean replay