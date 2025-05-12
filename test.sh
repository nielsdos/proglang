#!/bin/bash
set -e
#cargo run -- example
gcc -Wall test.c output.s -o test -L./target/debug -lruntime
./test
