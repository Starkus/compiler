section .text
CopyMemory:
xor rax, rax
$loop:
cmp rax, r8
jge $done
mov r9b, [rdx]
mov [rcx], r9b
inc rax
inc rcx
inc rdx
jmp $loop
$done:
ret
