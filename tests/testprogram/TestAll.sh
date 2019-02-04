#! /bin/bash

make test-Standalone &&
make test-StandaloneNoMPI &&
make test-IconStandalone &&
make test-IconTestbed &&
make test-IconCompare &&
make test-BaseCompare &&
make clean
