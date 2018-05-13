FTGROOT = /home/christian/workspace/fortrantestgenerator
SBROOT = /home/christian/lib/serialbox2

FF = mpif90
FFLAGS = -g -O0
LIBS = -L$(SBROOT)/lib  -lSerialboxCore -lSerialboxC -lSerialboxFortran -lstdc++
INCLUDE = -I$(SBROOT)/include
MPINODES = 4 

######################################################

default: build run

capture: clean assembler ftg build run 

assembler: globals.s types.s sub.s maingeneric.s 

build: maingeneric.f90 sub.o types.o globals.o
	$(FF) -o maingeneric $(FFLAGS) $(INCLUDE) $^ $(LIBS)
	
run:	
	-mpirun -n $(MPINODES) ./maingeneric
	
sub.o : types.o globals.o

%.s: %.f90
	$(FF) -c -S $(FFLAGS) $<	
	
%.mod: %.o
	
%.o: %.f90
	$(FF) -c $(FFLAGS) $(INCLUDE) $<

ftg: 
	$(FTGROOT)/FortranTestGenerator.py -bc -cf ../config_fortrantestgenerator_test_standalone.py sub testsub
	
clean:
	$(FTGROOT)/FortranTestGenerator.py -b -cf ../config_fortrantestgenerator_test_standalone.py sub testsub
	rm -f maingeneric *.o *.mod *.s
	
clean-o:
	rm -f *.o
	
.PHONY: clean
	