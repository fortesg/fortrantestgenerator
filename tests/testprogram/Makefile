COMPARE = $(HOME)/lib/serialbox2/python/compare/compare.py

test-standalone: clean capture replay-standalone compare

compare:
	$(COMPARE) -w data/ftg_testsub_test/output/MetaData-ftg_testsub_output_0.json data/ftg_testsub_test/output_test/MetaData-ftg_testsub_output_0.json
	$(COMPARE) -w data/ftg_testsub_test/output/MetaData-ftg_testsub_output_1.json data/ftg_testsub_test/output_test/MetaData-ftg_testsub_output_1.json
	
replay-standalone: 
	$(MAKE) -C replay-standalone replay
	 
capture: 
	$(MAKE) -C src capture
	 
clean: 
	$(MAKE) -C src clean
	$(MAKE) -C replay-standalone clean
	rm -Rf data/*
	
.PHONY: test clean capture replay-standalone compare 