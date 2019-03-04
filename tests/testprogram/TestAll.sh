#! /bin/bash

make test-Standalone &&
make test-StandaloneNoMPI &&
make test-IconStandalone &&
make test-IconJsbachMock &&
make test-BaseCompare &&
make clean
