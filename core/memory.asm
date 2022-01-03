_TEXT SEGMENT
CopyMemory PROC
xor rax, rax
$loop:
cmp rax, r8
jge $done
mov r9b, BYTE PTR [rdx]
mov BYTE PTR [rcx], r9b
inc rax
inc rcx
inc rdx
jmp $loop
$done:
ret
CopyMemory ENDP
_TEXT ENDS
