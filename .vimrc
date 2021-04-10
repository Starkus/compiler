let g:build_command = "build.bat"
let g:build_release_command = "build.bat -r"
let g:run_command =  "bin\\Compiler.exe"
let g:debug_command =  "remedybg.exe compiler.rdbg"

" Ignore build folder in wildcards
set wildignore+=*/build/*
set wildignore+=*/bin/*

call StartTerminal()
call HideTerminal()
