FF = mpif90
FFLAGS = -g -O0
LIBS = 
INCLUDE =
NODES = 4 

######################################################

all : maingeneric assembler

maingeneric: maingeneric.f90 types.o globals.o
	$(FF) -o $@ $(FFLAGS) $(INCLUDE) $(LIBS) $^
	mpirun -n $(NODES) ./$@
	
assembler : maingeneric.s types.s globals.s

%.s: %.f90
	$(FF) -c -S $(FFLAGS) $(INCLUDE) $<	
	
%.o: %.f90
	$(FF) -c $(FFLAGS) $(INCLUDE) $<	
	
clean:
	rm -f maingeneric *.o *.mod *.s