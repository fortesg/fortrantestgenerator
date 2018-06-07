COMPARE = $(HOME)/lib/serialbox2/python/compare/compare.py

test-standalone: clean capture-standalone replay-standalone compare-standalone

test-standalone_nompi: clean capture-standalone_nompi replay-standalone_nompi compare-standalone_nompi

compare-standalone:
	$(COMPARE) -w data/ftg_testsub_test/output/MetaData-ftg_testsub_output_0.json data/ftg_testsub_test/output_test/MetaData-ftg_testsub_output_0.json
	$(COMPARE) -w data/ftg_testsub_test/output/MetaData-ftg_testsub_output_1.json data/ftg_testsub_test/output_test/MetaData-ftg_testsub_output_1.json

compare-standalone_nompi:
	$(COMPARE) -w data/ftg_testsub_test/output/MetaData-ftg_testsub_output.json data/ftg_testsub_test/output_test/MetaData-ftg_testsub_output.json
	
replay-standalone: 
	$(MAKE) -C replay-standalone replay
	 
replay-standalone_nompi: 
	$(MAKE) -C replay-standalone_nompi replay
	 
capture-standalone: 
	$(MAKE) -C src capture-standalone
	 
capture-standalone_nompi: 
	$(MAKE) -C src capture-standalone_nompi
	 
clean: 
	$(MAKE) -C src clean
	$(MAKE) -C replay-standalone clean
	$(MAKE) -C replay-standalone_nompi clean
	rm -Rf data/*
	
.PHONY: test clean capture-standalone replay-standalone replay-standalone_nompi compare 