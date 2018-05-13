FTGROOT = /home/christian/workspace/fortrantestgenerator
SBROOT = /home/christian/lib/serialbox2

FF = mpif90
FFLAGS = -g -O0
LIBS = -L$(SBROOT)/lib  -lSerialboxCore -lSerialboxC -lSerialboxFortran -lstdc++
INCLUDE = -I$(SBROOT)/include -I../src
MPINODES = 4 

######################################################

replay: ftg_testsub_test
	-mpirun -n $(MPINODES) ./ftg_testsub_test

ftg_testsub_test: ftg_testsub_test.f90 ../src/sub.o ../src/types.o ../src/globals.o
	$(FF) -o $@ $(FFLAGS) $(INCLUDE) $^ $(LIBS)

ftg_testsub_test.f90: 
	$(FTGROOT)/FortranTestGenerator.py -r -cf ../config_fortrantestgenerator_test_standalone.py sub testsub
	
clean:
	rm -f ftg_testsub_test *.f90
	
.PHONY: clean replay