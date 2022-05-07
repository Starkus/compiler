del /Q %USERPROFILE%\compiler\core
mkdir %USERPROFILE%\compiler\bin
copy bin\Compiler.exe %USERPROFILE%\compiler\bin\Compiler.exe
copy bin\Compiler.pdb %USERPROFILE%\compiler\bin\Compiler.pdb
xcopy /S core\ %USERPROFILE%\compiler\core\
