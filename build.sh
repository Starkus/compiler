#/bin/bash

COMMON_OPTIONS="-I src/ -I external/ -std=c++20 -Wno-switch -Wno-assume -mavx2 -g -fno-omit-frame-pointer
	-fno-exceptions -fno-rtti"
# -fuse-ld=mold

echo Compiling...

nasm src/DynamicCallLinux.asm -f elf64 -o bin/basic.o
clang -shared -o bin/basic.so bin/basic.o

if [ "$1" = "-r" ]
then
	clang++ src/Compiler.cpp bin/basic.o -o bin/Compiler -DDEBUG_BUILD=0 -O3 $COMMON_OPTIONS
else
	clang++ src/Compiler.cpp bin/basic.o -o bin/Compiler -DDEBUG_BUILD=1 -O0 $COMMON_OPTIONS
fi

if [ "$?" = 0 ]
then
	echo Success
else
	echo Failed
fi

#ld bin/Compiler.o bin/basic.o -o bin/Compiler
echo Generating tags...
ctags -R .
