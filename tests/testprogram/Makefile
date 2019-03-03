COMPARE = $(HOME)/lib/serialbox2/python/compare/compare.py

test-Standalone: clean capture-Standalone replay-Standalone ok

test-StandaloneNoMPI: clean capture-StandaloneNoMPI replay-StandaloneNoMPI ok

test-IconStandalone: clean capture-IconStandalone replay-IconStandalone ok

test-IconTestbed: clean capture-IconTestbed replay-IconTestbed compare-mpi ok

test-IconCompare: clean capture-IconCompare replay-IconCompare ok

test-BaseCompare: clean capture-BaseCompare clean-capture-icon replay-BaseCompare ok

replay-Standalone: 
	$(MAKE) -C replay-Standalone replay
	 
replay-StandaloneNoMPI: 
	$(MAKE) -C replay-StandaloneNoMPI replay
	
replay-IconStandalone: 
	$(MAKE) -C replay-IconStandalone replay
	 
replay-IconTestbed: 
	$(MAKE) -C replay-IconTestbed replay
	
replay-IconCompare: 
	$(MAKE) -C replay-IconCompare replay
	
replay-BaseCompare: 
	$(MAKE) -C replay-BaseCompare replay
	 
capture-Standalone: 
	$(MAKE) -C src capture-Standalone
	 
capture-StandaloneNoMPI: 
	$(MAKE) -C src capture-StandaloneNoMPI
	 
capture-IconStandalone: 
	$(MAKE) -C src capture-IconStandalone
	 
capture-IconTestbed: 
	$(MAKE) -C src capture-IconTestbed
	 
capture-IconCompare: 
	$(MAKE) -C src capture-IconCompare
	 
capture-BaseCompare: 
	$(MAKE) -C src capture-BaseCompare
	 
compare-mpi:
	$(COMPARE) -w data/ftg_testsub_test/output/MetaData-ftg_testsub_output_00.json data/ftg_testsub_test/output_test/MetaData-ftg_testsub_output_00.json
	$(COMPARE) -w data/ftg_testsub_test/output/MetaData-ftg_testsub_output_01.json data/ftg_testsub_test/output_test/MetaData-ftg_testsub_output_01.json

compare-nompi:
	$(COMPARE) -w data/ftg_testsub_test/output/MetaData-ftg_testsub_output.json data/ftg_testsub_test/output_test/MetaData-ftg_testsub_output.json

ok:
	printf '\e[38;5;0m\e[48;5;10m*** OK ***\e[48;5;0m\n\e[38;5;15m'
	
clean-capture-icon: 
	$(MAKE) -C src clean-capture-icon
	
clean-capture-generic: 
	$(MAKE) -C src clean-capture-generic

clean: 
	$(MAKE) -C src clean
	$(MAKE) -C replay-Standalone clean
	$(MAKE) -C replay-StandaloneNoMPI clean
	$(MAKE) -C replay-IconStandalone clean
	$(MAKE) -C replay-IconTestbed clean
	$(MAKE) -C replay-IconCompare clean
	$(MAKE) -C replay-BaseCompare clean
	rm -Rf data/*
	
.PHONY: test clean clean-capture-icon clean-capture-generic replay-Standalone replay-StandaloneNoMPI replay-IconStandalone replay-IconTestbed replay-IconCompare replay-BaseCompare
