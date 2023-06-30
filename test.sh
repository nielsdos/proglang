#!/bin/bash
set -e
cargo run -- example
gcc -Wall test.c output.s -lm -o test
./test
