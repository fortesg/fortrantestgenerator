FF = mpif90
FFLAGS = -g -O0
LIBS = 
INCLUDE =
NODES = 4 

######################################################

all : maingeneric maingeneric.s types.s 

maingeneric: maingeneric.f90 types.o
	$(FF) -o $@ $(FFLAGS) $(INCLUDE) $(LIBS) $<
	mpirun -n $(NODES) ./$@

%.s: %.f90
	$(FF) -c -S $(FFLAGS) $(INCLUDE) $<	
	
%.o: %.f90
	$(FF) -c $(FFLAGS) $(INCLUDE) $<	
	
clean:
	rm -f maingeneric *.o *.mod *.s