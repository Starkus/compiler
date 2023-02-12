del /Q %USERPROFILE%\compiler\core
mkdir %USERPROFILE%\compiler\bin
copy bin\Compiler.exe %USERPROFILE%\compiler\bin\Compiler.exe
copy bin\Compiler.pdb %USERPROFILE%\compiler\bin\Compiler.pdb
copy bin\core.lib     %USERPROFILE%\compiler\bin\core.lib
copy bin\core.dll     %USERPROFILE%\compiler\bin\core.dll
xcopy /S core\ %USERPROFILE%\compiler\core\
