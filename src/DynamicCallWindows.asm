_TEXT SEGMENT
savedR12$ = 128
savedRBX$ = 136
SYSCallProcedureDynamically PROC
; Stack calculated by MSVC, with space for up to 16 arguments
sub rsp, 152
mov savedR12$[rsp], r12
mov savedRBX$[rsp], rbx

mov r10, rcx ; r10 = procStart
mov r11, rdx ; r11 = argCount
mov r12, r8  ; r12 = argValues

; rcx / xmm0
cmp r11, 1
jb Lcall
mov rcx, [r12]
mov rcx, [rcx]
movq xmm0, rcx

; rdx / xmm1
cmp r11, 2
jb Lcall
mov rdx, [r12+8]
mov rdx, [rdx]
movq xmm1, rdx

; r8  / xmm2
cmp r11, 3
jb Lcall
mov r8,  [r12+16]
mov r8,  [r8]
movq xmm2, r8

; r9  / xmm3
cmp r11, 4
jb Lcall
mov r9,  [r12+24]
mov r9,  [r9]
movq xmm3, r9

; Stack parameters
mov rbx, 4
Lloop:
mov rax, [r12+rbx*8]
mov rax, [rax]
mov [rsp+rbx*8], rax
inc rbx
cmp rbx, r11
jb Lloop

Lcall:
; Call
call r10

mov r12, savedR12$[rsp]
mov rbx, savedRBX$[rsp]
add rsp, 152
ret 0
SYSCallProcedureDynamically ENDP
_TEXT ENDS
END
