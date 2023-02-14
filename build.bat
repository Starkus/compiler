@echo off

cls

set ArgRelease=0
set ArgProfile=0
set ArgClang=0
set ArgBuildLibs=0
:args
IF "%~1"=="" GOTO endargs
IF "%~1"=="-r" set ArgRelease=1
IF "%~1"=="-p" set ArgProfile=1
IF "%~1"=="-clang" set ArgClang=1
IF "%~1"=="-libs" set ArgBuildLibs=1
SHIFT
GOTO args
:endargs

set SourceFiles=..\src\Compiler.cpp
set CompilerFlags=-nologo -GR- -GT -Oi -EHa- -W4 -FC -Z7 -I ..\external\ -std:c++latest -DIS_WINDOWS=1
set LinkerFlags=-opt:ref -incremental:no -debug:full DynamicCallWindows.obj
set Libraries=user32.lib winmm.lib shell32.lib advapi32.lib ..\external\xed\xed.lib

IF "%ArgRelease%"=="1" (
	set CompilerFlags=%CompilerFlags% -MT -O2 -GL
) ELSE IF "%ArgProfile%"=="1" (
	set CompilerFlags=%CompilerFlags% -MT -O2 -GL -DUSE_PROFILER_API=1
) ELSE (
	set CompilerFlags=%CompilerFlags% -MTd -Od -DDEBUG_BUILD=1
)

if "%ArgClang%"=="1" (
	set Compiler=clang-cl.exe
	set CompilerFlags=%CompilerFlags% -mavx2 -DIS_CLANG=1
	set IgnoreWarnings=-Wno-assume -Wno-switch -Wno-missing-field-initializers -Wno-sign-compare ^
			-Wno-missing-braces -Wno-deprecated-declarations
) ELSE (
	set Compiler=cl.exe
	set CompilerFlags=%CompilerFlags% -DIS_MSVC=1
	set IgnoreWarnings=-wd4201 -wd4996 -wd4063
)

IF NOT EXIST .\bin mkdir .\bin

pushd .\bin

set start=%time%
IF "%ArgBuildLibs%"=="1" (
	ml64 /nologo /c /Fo core.obj ..\src\AsmMemoryMasm.asm
	lib /OUT:core.lib core.obj
	link /OUT:core.dll /DLL /NOENTRY core.obj
)

ml64 /nologo /c /Fo DynamicCallWindows.obj ..\src\DynamicCallWindows.asm
%Compiler% %CompilerFlags% %IgnoreWarnings% %SourceFiles% %Libraries% -link %LinkerFlags%
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
