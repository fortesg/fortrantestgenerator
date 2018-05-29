FTGROOT = $(HOME)/workspace/fortrantestgenerator
SBROOT = $(HOME)/lib/serialbox2

FF = mpif90
FFLAGS = -g -O0
LIBS = -L$(SBROOT)/lib  -lSerialboxCore -lSerialboxC -lSerialboxFortran -lstdc++
INCLUDE = -I$(SBROOT)/include
MPINODES = 4 

######################################################

default: build run

capture: clean ftg maingeneric 
	-mpirun -n $(MPINODES) ./maingeneric

maingeneric: maingeneric.f90 sub.o types.o globals.o
	$(FF) -o $@ $(FFLAGS) $(INCLUDE) $^ $(LIBS)
	
sub.o : types.o globals.o

ftg: assembler
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_standalone.py sub testsub

assembler: globals.s types.s sub.s maingeneric.s 

%.s: %.f90
	$(FF) -c -S $(FFLAGS) $<	
	
%.o: %.f90
	$(FF) -c $(FFLAGS) $(INCLUDE) $<
	
clean:
	$(FTGROOT)/FortranTestGenerator.py -b -cf ../config_fortrantestgenerator_test_standalone.py
	rm -f maingeneric *.o *.mod *.s
	
clean-o:
	rm -f *.o
	
.PHONY: clean
	