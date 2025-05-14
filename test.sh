#!/bin/bash
set -e
#cargo run -- example
gcc -Wall -Wextra test.c output.s -o test -L./target/debug -lruntime
./test
