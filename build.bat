@echo off

cls

set SourceFiles=..\src\Compiler.cpp
set CompilerFlags=-nologo -Gm- -GR- -Oi -EHa- -W4 -wd4201 -wd4100 -wd4996 -wd4063 -FC -Z7 -I ..\external\ -std:c++latest
set LinkerFlags=-opt:ref -incremental:no -debug:full DynamicCallWindows.obj
set Libraries=user32.lib winmm.lib shell32.lib advapi32.lib ..\external\xed\xed.lib

IF "%1"=="-r" (
	set CompilerFlags=%CompilerFlags% -MT -O2 -GL
) ELSE IF "%1"=="-p" (
	set CompilerFlags=%CompilerFlags% -MT -O2 -GL -DUSE_PROFILER_API=1
) ELSE (
	set CompilerFlags=%CompilerFlags% -MTd -Od -DDEBUG_BUILD=1
)

IF NOT EXIST .\bin mkdir .\bin

pushd .\bin

set start=%time%
ml64 /nologo /c /Fo DynamicCallWindows.obj ..\src\DynamicCallWindows.asm
cl.exe %CompilerFlags% %SourceFiles% %Libraries% -link %LinkerFlags%
set end=%time%
IF %ERRORLEVEL% NEQ 0 echo [31mFailed![0m
IF %ERRORLEVEL% EQU 0 echo [32mSuccess[0m

popd

cmd /c timediff.bat Compilation %start% %end%
echo.

echo Generating tags...
ctags -R --use-slash-as-filename-separator=yes .
IF %ERRORLEVEL% NEQ 0 echo Tag generation [31mfailed![0m
IF %ERRORLEVEL% EQU 0 echo Done generating tags
