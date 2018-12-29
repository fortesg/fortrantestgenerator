FTGROOT = $(HOME)/workspace/fortrantestgenerator
SBROOT = $(HOME)/lib/serialbox2
FF = $(HOME)/lib/mpich/bin/mpif90

FFLAGS = -g -O0 -fopenmp
LIBS = -L$(SBROOT)/lib  -lSerialboxFortranStatic -lSerialboxCStatic  -lSerialboxStatic -lstdc++ -lstdc++fs
INCLUDE = -I$(SBROOT)/include -I../src
MPINODES = 2 

######################################################

replay: ftg_testsub_test
	-mpirun -n $(MPINODES) ./ftg_testsub_test

ftg_testsub_test: ftg_testsub_test.f90 ../src/sub.o ../src/types.o ../src/globals.o
	$(FF) -o $@ $(FFLAGS) $(INCLUDE) $^ $(LIBS)

ftg_testsub_test.f90: 
	$(FTGROOT)/FortranTestGenerator.py -r -cf ../config_fortrantestgenerator_test_Standalone.py sub testsub
	
clean:
	rm -f ftg_testsub_test *.f90
	
.PHONY: clean replay