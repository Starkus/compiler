@echo off

IF NOT EXIST .\bin mkdir .\bin

pushd .\output

ml64 out.asm /Zd /Zi /link kernel32.lib user32.lib Gdi32.lib winmm.lib opengl32.lib /entry:Main /opt:ref /incremental:no /dynamicbase:no /subsystem:CONSOLE /debug
IF %ERRORLEVEL% NEQ 0 echo [31mFailed![0m
IF %ERRORLEVEL% EQU 0 (echo [32mSuccess[0m)

popd
IF %ERRORLEVEL% EQU 0 output\out.exe
