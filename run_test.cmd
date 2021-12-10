@echo off

for %%f in (tests/*) do (
	echo | set /p=Running %%f...
	bin\Compiler.exe -silent tests/%%f
	bin\out.exe
)
