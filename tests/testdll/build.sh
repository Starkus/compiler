#/bin/bash

clang -O2 -shared testdll.c -o testdll.so -fPIC -fdeclspec -Wno-ignored-attributes
clang -O2 -c testdll.c -o testdll.o -fno-stack-protector -fdeclspec -Wno-ignored-attributes
ar rcs testdll.a testdll.c
