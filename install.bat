del /Q %USERPROFILE%\compiler\core
mkdir %USERPROFILE%\compiler\bin
copy bin\Compiler.exe %USERPROFILE%\compiler\bin\Compiler.exe
copy bin\Compiler.pdb %USERPROFILE%\compiler\bin\Compiler.pdb
copy bin\basic.lib    %USERPROFILE%\compiler\bin\basic.lib
copy bin\basic.dll    %USERPROFILE%\compiler\bin\basic.dll
xcopy /S core\ %USERPROFILE%\compiler\core\
