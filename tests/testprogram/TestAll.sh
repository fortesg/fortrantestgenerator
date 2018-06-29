#! /bin/bash

make test-standalone &&
make test-StandaloneNoMPI &&
make test-IconStandalone &&
make test-IconTestbed &&
make clean
