FTG = /home/christian/workspace/fortrantestgenerator/FortranTestGenerator.py

######################################################

ftg: 
	$(FTG) -r -cf ../config_fortrantestgenerator_test_standalone.py sub testsub
	
clean:
	$(FTG) -b -cf ../config_fortrantestgenerator_test_standalone.py sub testsub
	rm -f *.f90 *.o *.mod *.s