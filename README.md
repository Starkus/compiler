# Compiler
Toy compiler for learning purposes.

## Syntax?
The syntax is mostly copied from Jon Blow's JAI. Check out the sample programs in the tests and examples folders.
```
Main :: () -> s64
{
	Print("Hello world!\n");
	return 0;
}
```

## Usage
`compiler.exe <source file> [-noPromote] [-logAST] [-logAllocationInfo]`

Switches are pretty much just for debugging the compiler.

## Build
Run build.bat from within a Visual Studio shell (or any shell where you ran vsvarsall.bat).
