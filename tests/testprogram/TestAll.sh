#! /bin/bash

make test-Standalone &&
make test-StandaloneNoMPI &&
make test-IconStandalone &&
make test-IconTestbed &&
make clean
