@echo off

IF NOT EXIST .\bin mkdir .\bin

pushd .\bin

ml64 ..\out.asm /Zd /Zi /link kernel32.lib user32.lib Gdi32.lib winmm.lib /entry:Main /subsystem:CONSOLE /debug
IF %ERRORLEVEL% NEQ 0 echo [31mFailed![0m
IF %ERRORLEVEL% EQU 0 (echo [32mSuccess[0m) & out.exe

popd
