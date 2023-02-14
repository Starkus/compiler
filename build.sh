#/bin/bash

COMMON_OPTIONS="-I src/ -I external/ -std=c++20 -Wno-switch -Wno-assume -mavx2 -g
	-fno-exceptions -fno-rtti -DIS_LINUX=1 -DIS_CLANG=1"
# -fuse-ld=mold

echo Compiling...

nasm src/DynamicCallLinux.asm -f elf64 -o bin/DynamicCallLinux.o
nasm src/AsmMemoryNasm.asm -f elf64 -o bin/core.o
nasm src/LinuxStart.asm -f elf64 -o bin/LinuxStart.o
clang -shared -o bin/core.so bin/core.o

if [ "$1" = "-r" ]
then
	clang++ src/Compiler.cpp bin/DynamicCallLinux.o external/xed/xed.a -o bin/Compiler -DDEBUG_BUILD=0 -O3 $COMMON_OPTIONS
elif [ "$1" = "-p" ]
then
	clang++ src/Compiler.cpp bin/DynamicCallLinux.o external/xed/xed.a -o bin/Compiler -DDEBUG_BUILD=0 -O3 $COMMON_OPTIONS -fno-omit-frame-pointer
else
	clang++ src/Compiler.cpp bin/DynamicCallLinux.o external/xed/xed.a -o bin/Compiler -DDEBUG_BUILD=1 -O0 $COMMON_OPTIONS -fno-omit-frame-pointer
fi

if [ "$?" = 0 ]
then
	echo Success
else
	echo Failed
fi

echo Generating tags...
ctags -R .
