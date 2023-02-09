#/bin/bash

clang -shared testdll.c -o testdll.so -fPIC
clang -O2 -c testdll.c -o testdll.o -fno-stack-protector
ar rcs testdll.a testdll.c
