test: clean capture replay compare

compare:
	echo "TODO: compare"
	
replay: 
	$(MAKE) -C replay replay	
	 
capture: 
	$(MAKE) -C src capture
	 
clean: 
	$(MAKE) -C src clean
	$(MAKE) -C replay clean
	rm -Rf data/*
	
.PHONY: test clean compare 