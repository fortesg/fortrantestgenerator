FF = mpif90
FFLAGS = 
LIBS = 
INCLUDE =
NODES = 4 

######################################################

all : maingeneric

maingeneric: maingeneric.f90 types.o
	$(FF) -o $@ $(FFLAGS) $(INCLUDE) $(LIBS) $<
	mpirun -n $(NODES) ./$@
	
%.o: %.f90
	$(FF) -c $(FFLAGS) $(INCLUDE) $(FPPFLAGS) $<	
	
clean:
	rm -f maingeneric *.o