_TEXT SEGMENT
SYSCallProcedureDynamically PROC
push rbp
mov rbp, rsp
push r12
push rbx

mov r10, rcx ; r10 = procStart
mov r11, rdx ; r11 = argCount
mov r12, r8  ; r12 = argValues

cmp r11, 1
jb Lcall
mov rcx, [r12]
mov rcx, [rcx]

cmp r11, 2
jb Lcall
mov rdx, [r12+8]
mov rdx, [rdx]

cmp r11, 3
jb Lcall
mov r8,  [r12+16]
mov r8,  [r8]

cmp r11, 4
jb Lcall
mov r9,  [r12+24]
mov r9,  [r9]

; Stack parameters
lea rbx, [r11-1]

; Align to 16 bytes if odd
mov rax, r11
and rax, 1
jz Lloop
sub rsp, 8

Lloop:
mov rax, [r12+rbx*8]
push [rax]
sub rbx, 1
cmp rbx, 4
ja Lloop

Lcall:
; Call
call r10

mov rbx, [rbp-8]
mov r12, [rbp-16]
leave
ret
SYSCallProcedureDynamically ENDP
_TEXT ENDS
END
