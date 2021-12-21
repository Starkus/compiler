@echo off

IF NOT EXIST .\bin mkdir .\bin

pushd .\output

ml64 out.asm /Zd /Zi /link kernel32.lib user32.lib Gdi32.lib winmm.lib /entry:Main /opt:ref /incremental:no /dynamicbase:no /subsystem:WINDOWS /debug
IF %ERRORLEVEL% NEQ 0 echo [31mFailed![0m
IF %ERRORLEVEL% EQU 0 (echo [32mSuccess[0m) & out.exe

popd
