#! /bin/bash

make test-standalone &&
make test-standalone_nompi &&
make test-IconStandalone &&
make test-IconTestbed &&
make clean
