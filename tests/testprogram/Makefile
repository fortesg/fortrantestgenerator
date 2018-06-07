COMPARE = $(HOME)/lib/serialbox2/python/compare/compare.py

test-standalone: clean capture-standalone replay-standalone compare-mpi

test-standalone_nompi: clean capture-standalone_nompi replay-standalone_nompi compare-nompi

test-icon_standalone: clean capture-icon_standalone replay-icon_standalone compare-mpi

compare-mpi:
	$(COMPARE) -w data/ftg_testsub_test/output/MetaData-ftg_testsub_output_0.json data/ftg_testsub_test/output_test/MetaData-ftg_testsub_output_0.json
	$(COMPARE) -w data/ftg_testsub_test/output/MetaData-ftg_testsub_output_1.json data/ftg_testsub_test/output_test/MetaData-ftg_testsub_output_1.json

compare-nompi:
	$(COMPARE) -w data/ftg_testsub_test/output/MetaData-ftg_testsub_output.json data/ftg_testsub_test/output_test/MetaData-ftg_testsub_output.json
	
replay-standalone: 
	$(MAKE) -C replay-standalone replay
	 
replay-standalone_nompi: 
	$(MAKE) -C replay-standalone_nompi replay
	
replay-icon_standalone: 
	$(MAKE) -C replay-icon_standalone replay
	 
capture-standalone: 
	$(MAKE) -C src capture-standalone
	 
capture-standalone_nompi: 
	$(MAKE) -C src capture-standalone_nompi
	 
capture-icon_standalone: 
	$(MAKE) -C src capture-icon_standalone
	 
clean: 
	$(MAKE) -C src clean
	$(MAKE) -C replay-standalone clean
	$(MAKE) -C replay-standalone_nompi clean
	$(MAKE) -C replay-icon_standalone clean
	rm -Rf data/*
	
.PHONY: test clean capture-standalone replay-standalone replay-standalone_nompi replay-icon_standalone compare 