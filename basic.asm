_TEXT SEGMENT
CopyMemory PROC
push rbp
mov rbp, rsp
mov rax, 0
$loop:
cmp rax, r8
jge $done
mov r9b, BYTE PTR [rcx]
mov BYTE PTR [rdx], r9b
add rax, 1
add rcx, 1
add rdx, 1
jmp $loop
$done:
leave
ret
CopyMemory ENDP
_TEXT ENDS
