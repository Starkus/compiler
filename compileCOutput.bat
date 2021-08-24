@echo off

cls

set SourceFiles=..\out.c
set CommonCompilerFlags=-MTd -nologo -Gm- -GR- -Od -Oi -EHa- -W4 -wd4201 -wd4100 -wd4996 -wd4063 -wd4305 -FC -Z7 -I ..\external\
set CommonLinkerFlags=-opt:ref -incremental:no
set Libraries=user32.lib Gdi32.lib winmm.lib

IF NOT EXIST .\bin mkdir .\bin

pushd .\bin

set start=%time%
cl %CommonCompilerFlags% -DDEBUG_BUILD=1 %SourceFiles% %Libraries% -link %CommonLinkerFlags%
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
