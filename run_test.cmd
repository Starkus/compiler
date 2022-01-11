@echo off
setlocal EnableDelayedExpansion

for %%f in (tests/*) do (
	echo | set /p=Running %%f...
	bin\Compiler.exe tests/%%f >NUL 2>NUL
	IF !ERRORLEVEL! NEQ 0 echo [36GCompilation [31mfailed![0m
	IF !ERRORLEVEL! EQU 0 (
		echo | set /p=[36GCompilation [32msuccess[0m...
		output\out.exe >NUL
		IF !ERRORLEVEL! NEQ 0 echo [60GTest [31mfailed![0m
		IF !ERRORLEVEL! EQU 0 echo [60GTest [32msuccess[0m
	)
)
