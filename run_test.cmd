@echo off

for %%f in (tests/*) do (
	echo | set /p=Running %%f...
	bin\Compiler.exe tests/%%f
	output\out.exe
)
