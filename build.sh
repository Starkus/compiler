#/bin/bash

COMMON_OPTIONS="-I src/ -I external/ -Wno-switch -Wno-assume -mavx2 -g -fno-omit-frame-pointer
	-fno-exceptions -fno-rtti -fuse-ld=mold"

echo Compiling...
if [ "$1" = "-r" ]
then
	clang++ src/Compiler.cpp -o bin/Compiler -DDEBUG_BUILD=0 -O3 $COMMON_OPTIONS
else
	clang++ src/Compiler.cpp -o bin/Compiler -DDEBUG_BUILD=1 -O0 $COMMON_OPTIONS
fi
echo Generating tags...
ctags -R .
