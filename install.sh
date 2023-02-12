#/bin/bash

mkdir -p /usr/include/compiler/core/
mkdir -p /usr/lib/compiler

cp core/* /usr/include/compiler/core/
cp bin/Compiler /usr/bin/compiler
cp bin/core.o /usr/lib/compiler/
cp bin/core.so /usr/lib/compiler/
cp bin/LinuxStart.o /usr/lib/compiler/
