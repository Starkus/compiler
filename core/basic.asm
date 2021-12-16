_TEXT SEGMENT
CopyMemory PROC
xor rax, rax
$loop:
cmp rax, r8
jge $done
mov r9b, BYTE PTR [rdx]
mov BYTE PTR [rcx], r9b
add rax, 1
add rcx, 1
add rdx, 1
jmp $loop
$done:
ret
CopyMemory ENDP
_TEXT ENDS
