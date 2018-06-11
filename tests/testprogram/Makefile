COMPARE = $(HOME)/lib/serialbox2/python/compare/compare.py
COMPARELEGACY = $(HOME)/workspace/serialbox-compare/sbcompare

test-standalone: clean capture-standalone replay-standalone compare-mpi

test-standalone_nompi: clean capture-standalone_nompi replay-standalone_nompi compare-nompi

test-icon_standalone: clean capture-icon_standalone replay-icon_standalone compare-mpi

test-icon_testbed: clean capture-icon_testbed replay-icon_testbed compare-mpi

test-icon_standalone_legacy: clean capture-icon_standalone_legacy replay-icon_standalone_legacy compare-legacy

replay-standalone: 
	$(MAKE) -C replay-standalone replay
	 
replay-standalone_nompi: 
	$(MAKE) -C replay-standalone_nompi replay
	
replay-icon_standalone: 
	$(MAKE) -C replay-icon_standalone replay
	 
replay-icon_testbed: 
	$(MAKE) -C replay-icon_testbed replay
	 
replay-icon_standalone_legacy: 
	$(MAKE) -C replay-icon_standalone_legacy replay
	 
capture-standalone: 
	$(MAKE) -C src capture-standalone
	 
capture-standalone_nompi: 
	$(MAKE) -C src capture-standalone_nompi
	 
capture-icon_standalone: 
	$(MAKE) -C src capture-icon_standalone
	 
capture-icon_testbed: 
	$(MAKE) -C src capture-icon_testbed
	 
capture-icon_standalone_legacy: 
	$(MAKE) -C src capture-icon_standalone_legacy
	 
compare-mpi:
	$(COMPARE) -w data/ftg_testsub_test/output/MetaData-ftg_testsub_output_0.json data/ftg_testsub_test/output_test/MetaData-ftg_testsub_output_0.json
	$(COMPARE) -w data/ftg_testsub_test/output/MetaData-ftg_testsub_output_1.json data/ftg_testsub_test/output_test/MetaData-ftg_testsub_output_1.json
	printf '\e[38;5;0m\e[48;5;10m*** OK ***\e[48;5;0m\n'

compare-nompi:
	$(COMPARE) -w data/ftg_testsub_test/output/MetaData-ftg_testsub_output.json data/ftg_testsub_test/output_test/MetaData-ftg_testsub_output.json
	printf '\e[38;5;0m\e[48;5;10m*** OK ***\e[48;5;0m\n'

compare-legacy:
	$(COMPARELEGACY) data/ftg_testsub_test/output/ftg_testsub_output_0.json data/ftg_testsub_test/output_test/ftg_testsub_output_0.json 
	$(COMPARELEGACY) data/ftg_testsub_test/output/ftg_testsub_output_1.json data/ftg_testsub_test/output_test/ftg_testsub_output_1.json
	printf '\e[38;5;0m\e[48;5;10m*** OK ***\e[48;5;0m\n'
	
clean: 
	$(MAKE) -C src clean
	$(MAKE) -C replay-standalone clean
	$(MAKE) -C replay-standalone_nompi clean
	$(MAKE) -C replay-icon_standalone clean
	$(MAKE) -C replay-icon_standalone_legacy clean
	rm -Rf data/*
	
.PHONY: test clean replay-standalone replay-standalone_nompi replay-icon_standalone replay-icon_testbed replay-icon_standalone_legacy compare 
