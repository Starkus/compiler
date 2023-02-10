_TEXT SEGMENT
savedR12$ = 128
savedRBX$ = 136
SYSCallProcedureDynamically_windowscc PROC
; Stack calculated by MSVC, with space for up to 16 arguments
sub rsp, 152
mov savedR12$[rsp], r12
mov savedRBX$[rsp], rbx

mov r10, rcx ; r10 = procStart
mov r11, rdx ; r11 = argCount
mov r12, r8  ; r12 = argValues

; Stack parameters
mov ebx, 4
cmp ebx, r11d
jae LregArgs
Lloop:
mov rax, [r12+rbx*8]
mov [rsp+rbx*8], rax
inc ebx
cmp ebx, r11d
jae Lr9
jmp Lloop

; We only land here when argCount <= 4, so we can use the jump table directly.
LregArgs:
; Jump table
lea rbx, [JumpTable]
movsxd rax, dword ptr [rbx+r11*4]
add rax, rbx
jmp rax

Lr9:
mov r9,  [r12+24]
movq xmm3, r9
Lr8:
mov r8,  [r12+16]
movq xmm2, r8
Lrdx:
mov rdx, [r12+8]
movq xmm1, rdx
Lrcx:
mov rcx, [r12]
movq xmm0, rcx
Lnogp:

Lcall:
; Call
call r10

mov r12, savedR12$[rsp]
mov rbx, savedRBX$[rsp]
add rsp, 152
ret 0

JumpTable:
	dword Lnogp - JumpTable
	dword Lr9   - JumpTable
	dword Lr8   - JumpTable
	dword Lrdx  - JumpTable
	dword Lrcx  - JumpTable
SYSCallProcedureDynamically_windowscc ENDP

SYSCallProcedureDynamically_linuxcc PROC
; rcx: procedure address
; rdx: gp arg count   [0, 6]
; r8:  gp arg data
; r9:  xmm arg count
; [rbp+40] xmm arg data
; [rbp+48] stack arg count
; [rbp+56] stack arg data

push rbp
mov rbp, rsp

; Save proc address because we'll need rcx
push rcx ; [rbp-8]

; Stack parameters
mov rax, qword ptr [rbp+56]
mov r10d, [rbp+48]
Lloop:
dec r10d
js Lxmm
push [rax + r10 * 8]
jmp Lloop

Lxmm:
mov r11, [rbp+40]
; Jump table
lea r10, [LXMMJumpTable]
movsxd rax, dword ptr [r10 + 4 * r9]
add rax, r10
jmp rax

Lxmm16:
movsd xmm15, qword ptr [r11 + 15 * 8]
Lxmm15:
movsd xmm14, qword ptr [r11 + 14 * 8]
Lxmm14:
movsd xmm13, qword ptr [r11 + 13 * 8]
Lxmm13:
movsd xmm12, qword ptr [r11 + 12 * 8]
Lxmm12:
movsd xmm11, qword ptr [r11 + 11 * 8]
Lxmm11:
movsd xmm10, qword ptr [r11 + 10 * 8]
Lxmm10:
movsd xmm9,  qword ptr [r11 + 9  * 8]
Lxmm9:
movsd xmm8,  qword ptr [r11 + 8  * 8]
Lxmm8:
movsd xmm7,  qword ptr [r11 + 7  * 8]
Lxmm7:
movsd xmm6,  qword ptr [r11 + 6  * 8]
Lxmm6:
movsd xmm5,  qword ptr [r11 + 5  * 8]
Lxmm5:
movsd xmm4,  qword ptr [r11 + 4  * 8]
Lxmm4:
movsd xmm3,  qword ptr [r11 + 3  * 8]
Lxmm3:
movsd xmm2,  qword ptr [r11 + 2  * 8]
Lxmm2:
movsd xmm1,  qword ptr [r11 + 1  * 8]
Lxmm1:
movsd xmm0,  qword ptr [r11 + 0  * 8]
Lxmm0:

; Jump table
lea r10, [LGPJumpTable]
movsxd rax, dword ptr [r10 + 4 * rdx]
add rax, r10
jmp rax

Lr9:
mov r9,  [r8 + 5 * 8]
Lr8:
mov r8,  [r8 + 4 * 8]
Lrcx:
mov rcx, [r8 + 3 * 8]
Lrdx:
mov rdx, [r8 + 2 * 8]
Lrsi:
mov rsi, [r8 + 1 * 8]
Lrdi:
mov rdi, [r8 + 0 * 8]
Lnogp:

; Red zone (like pushing rcx, rdx, r8 and r9
sub rsp, 32

; Call
call qword ptr [rbp-8]

leave
ret 0

LGPJumpTable:
	dword Lnogp - LGPJumpTable
	dword Lrdi  - LGPJumpTable
	dword Lrsi  - LGPJumpTable
	dword Lrdx  - LGPJumpTable
	dword Lrcx  - LGPJumpTable
	dword Lr8   - LGPJumpTable
	dword Lr9   - LGPJumpTable

LXMMJumpTable:
	dword Lxmm0  - LXMMJumpTable
	dword Lxmm1  - LXMMJumpTable
	dword Lxmm2  - LXMMJumpTable
	dword Lxmm3  - LXMMJumpTable
	dword Lxmm4  - LXMMJumpTable
	dword Lxmm5  - LXMMJumpTable
	dword Lxmm6  - LXMMJumpTable
	dword Lxmm7  - LXMMJumpTable
	dword Lxmm8  - LXMMJumpTable
	dword Lxmm9  - LXMMJumpTable
	dword Lxmm10 - LXMMJumpTable
	dword Lxmm11 - LXMMJumpTable
	dword Lxmm12 - LXMMJumpTable
	dword Lxmm13 - LXMMJumpTable
	dword Lxmm14 - LXMMJumpTable
	dword Lxmm15 - LXMMJumpTable
	dword Lxmm16 - LXMMJumpTable
SYSCallProcedureDynamically_linuxcc ENDP
_TEXT ENDS
END
