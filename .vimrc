let g:build_command = "build.bat"
let g:build_release_command = "build.bat -r"
let g:run_command =  "bin\\Compiler.exe"
let g:debug_command =  "remedybg.exe compiler.rdbg"

command! CompileC :call term_sendkeys("terminal", "compileCOutput.bat\<CR>") | call ShowTerminal()
command! Assemble :call term_sendkeys("terminal", "assembleOutput.bat\<CR>") | call ShowTerminal()
command! Test :call term_sendkeys("terminal", "output\\out.exe\<CR>") | call ShowTerminal()

" Ignore build folder in wildcards
set wildignore+=*/build/*
set wildignore+=*/bin/*

call StartTerminal()
call HideTerminal()
