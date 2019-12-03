#!/bin/sh
clang -Wall -Wextra -pedantic -std=c99 -Og -g -o scheme scheme.c -DOSX
