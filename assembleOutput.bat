@echo off

set SourceFiles=..\out.c
set CommonCompilerFlags=-MTd -nologo -Gm- -GR- -Od -Oi -EHa- -W0 -wd4098 -wd4201 -wd4100 -wd4996 -wd4063 -wd4305 -FC -Z7 -I ..\external\
set CommonLinkerFlags=-opt:ref -incremental:no
set Libraries=user32.lib Gdi32.lib winmm.lib

IF NOT EXIST .\bin mkdir .\bin

pushd .\bin

ml64 ..\out.asm /link kernel32.lib user32.lib Gdi32.lib winmm.lib /entry:Main /subsystem:CONSOLE
IF %ERRORLEVEL% NEQ 0 echo [31mFailed![0m
IF %ERRORLEVEL% EQU 0 (echo [32mSuccess[0m) & out.exe

popd
