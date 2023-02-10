clang -c testdll.c -o testdll.obj
link /DLL /NOENTRY testdll.obj kernel32.lib
lib testdll.obj /out:testdll.lib
