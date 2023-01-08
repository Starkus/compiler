@echo off
setlocal EnableDelayedExpansion

set start=%time%

for %%f in (tests/*) do (
	echo | set /p=Running %%f...
	bin\Compiler.exe tests/%%f -silent
	IF !ERRORLEVEL! NEQ 0 echo [36GCompilation [31mfailed![0m
	IF !ERRORLEVEL! EQU 0 (
		echo | set /p=[36GCompilation [32msuccess[0m...
		output\out.exe >NUL
		IF !ERRORLEVEL! NEQ 0 echo [60GTest [31mfailed![0m
		IF !ERRORLEVEL! EQU 0 echo [60GTest [32msuccess[0m
	)
)

echo | set /p=Running error test wrongFilename.emi...
bin\Compiler.exe errortests/wrongFilename.emi >NUL 2>NUL
IF !ERRORLEVEL! EQU 0 echo [55GCompilation [31msucceded![0m
IF !ERRORLEVEL! NEQ 0 echo [55GCompilation [32mfailed[0m

for %%f in (errortests/*) do (
	echo | set /p=Running error test %%f...
	bin\Compiler.exe errortests/%%f >NUL 2>NUL
	IF !ERRORLEVEL! EQU 0 echo [55GCompilation [31msucceded![0m
	IF !ERRORLEVEL! NEQ 0 echo [55GCompilation [32mfailed[0m
)

set end=%time%
cmd /c timediff.bat Tests %start% %end%
