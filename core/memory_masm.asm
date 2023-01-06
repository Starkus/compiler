_TEXT SEGMENT
CopyMemory PROC PUBLIC
xor rax, rax
$loop:
cmp rax, rdx
jge $done
mov r9b, [rsi]
mov [rdi], r9b
inc rax
inc rdi
inc rsi
jmp $loop
$done:
ret
CopyMemory ENDP
ZeroMemory PROC PUBLIC
xor rax, rax
xor rdx, rdx
$loop:
cmp rax, rsi
jge $done
mov [rdi], dl
inc rax
inc rdi
jmp $loop
$done:
ret
ZeroMemory ENDP
_TEXT ENDS
END
