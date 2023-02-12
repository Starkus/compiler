del /Q %USERPROFILE%\fabric\core
mkdir %USERPROFILE%\fabric\bin
copy bin\Compiler.exe %USERPROFILE%\fabric\bin\fabric.exe
copy bin\Compiler.pdb %USERPROFILE%\fabric\bin\fabric.pdb
copy bin\core.lib     %USERPROFILE%\fabric\bin\core.lib
copy bin\core.dll     %USERPROFILE%\fabric\bin\core.dll
xcopy /S core\ %USERPROFILE%\fabric\core\
