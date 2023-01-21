# Compiler
Toy compiler for learning purposes.

![Invaders screenshot](https://i.imgur.com/ySTRQyz.png)

_Invaders program found in examples/invaders.emi_

## Features
* Out of order compilation: top-level statements flow independently through the system, and can depend on any other part of the code.
* Linking with C code, supporting Microsoft's x64 calling convention and Linux's.
* Full compile-time execution: any code can be run while compiling in interpreter mode, with access to memory, modify the resulting executable's data segment, and can call dynamically linked code. The only limitations right now are, you can't have procedure pointers (since procedures aren't code in memory) and you can't have unions that can hold pointers on the data segment, since the compiler can't know if it should remap the pointers or not.
* A sketchy AMD64 codegen that generates way more code than it should but hey, no llvm!

## Syntax?
The syntax is mostly copied from Jon Blow's JAI. Check out the sample programs in the tests and examples folders.
```
Main :: (args: [] String) -> s64
{
	Print("Hello world!\n");
	return 0;
}
```

## Usage
`compiler.exe <source file> [-noEscapeSequences] [-silent]`

## Build
Run build.bat from within a Visual Studio shell (or any shell where you ran vsvarsall.bat).
