_TEXT SEGMENT
SYSCallProcedureDynamically_windowscc PROC
; rcx: procedure address
; rdx: arg count
; r8:  arg data

push rbp
mov rbp, rsp

; Save proc address because we'll need rcx
push rcx ; [rbp-8]

; Stack parameters
lea rax, [r8 + 4*8] ; Pointer starting on the 4th arg value (first stack arg)
lea r10d, [rdx-4] ; Stack arg count
Lloop:
dec r10d
js LregArgs
push [rax + r10*8]
jmp Lloop

LregArgs:
mov r11, r8
cmp edx, 4
ja Lr9
; Jump table
lea r10, [JumpTable]
movsxd rax, DWORD PTR [r10+rdx*4]
add rax, r10
jmp rax

Lr9:
mov r9,  [r11+24]
movq xmm3, r9
Lr8:
mov r8,  [r11+16]
movq xmm2, r8
Lrdx:
mov rdx, [r11+8]
movq xmm1, rdx
Lrcx:
mov rcx, [r11]
movq xmm0, rcx
Lnogp:

; Red zone (like pushing rcx, rdx, r8 and r9)
sub rsp, 32

; Call
call QWORD PTR [rbp-8]

leave
ret 0

ALIGN(4)
JumpTable:
	DWORD Lnogp - JumpTable
	DWORD Lrcx  - JumpTable
	DWORD Lrdx  - JumpTable
	DWORD Lr8   - JumpTable
	DWORD Lr9   - JumpTable
SYSCallProcedureDynamically_windowscc ENDP

SYSCallProcedureDynamically_linuxcc PROC
; rcx: procedure address
; rdx: gp arg count   [0, 6]
; r8:  gp arg data
; r9:  xmm arg count
; [rbp+48] xmm arg data
; [rbp+56] stack arg count
; [rbp+64] stack arg data

push rbp
mov rbp, rsp

; Save proc address because we'll need rcx
push rcx ; [rbp-8]

; Stack parameters
mov rax, QWORD PTR [rbp+64]
mov r10d, [rbp+56]
Lloop:
dec r10d
js Lxmm
push [rax + r10 * 8]
jmp Lloop

Lxmm:
mov r11, [rbp+48]
; Jump table
lea r10, [LXMMJumpTable]
movsxd rax, DWORD PTR [r10 + 4 * r9]
add rax, r10
jmp rax

Lxmm16:
movsd xmm15, QWORD PTR [r11 + 15 * 8]
Lxmm15:
movsd xmm14, QWORD PTR [r11 + 14 * 8]
Lxmm14:
movsd xmm13, QWORD PTR [r11 + 13 * 8]
Lxmm13:
movsd xmm12, QWORD PTR [r11 + 12 * 8]
Lxmm12:
movsd xmm11, QWORD PTR [r11 + 11 * 8]
Lxmm11:
movsd xmm10, QWORD PTR [r11 + 10 * 8]
Lxmm10:
movsd xmm9,  QWORD PTR [r11 + 9  * 8]
Lxmm9:
movsd xmm8,  QWORD PTR [r11 + 8  * 8]
Lxmm8:
movsd xmm7,  QWORD PTR [r11 + 7  * 8]
Lxmm7:
movsd xmm6,  QWORD PTR [r11 + 6  * 8]
Lxmm6:
movsd xmm5,  QWORD PTR [r11 + 5  * 8]
Lxmm5:
movsd xmm4,  QWORD PTR [r11 + 4  * 8]
Lxmm4:
movsd xmm3,  QWORD PTR [r11 + 3  * 8]
Lxmm3:
movsd xmm2,  QWORD PTR [r11 + 2  * 8]
Lxmm2:
movsd xmm1,  QWORD PTR [r11 + 1  * 8]
Lxmm1:
movsd xmm0,  QWORD PTR [r11 + 0  * 8]
Lxmm0:

mov r11, r8
; Jump table
lea r10, [LGPJumpTable]
movsxd rax, DWORD PTR [r10 + 4 * rdx]
add rax, r10
jmp rax

Lr9:
mov r9,  [r11 + 5 * 8]
Lr8:
mov r8,  [r11 + 4 * 8]
Lrcx:
mov rcx, [r11 + 3 * 8]
Lrdx:
mov rdx, [r11 + 2 * 8]
Lrsi:
mov rsi, [r11 + 1 * 8]
Lrdi:
mov rdi, [r11 + 0 * 8]
Lnogp:

; Call
call QWORD PTR [rbp-8]

leave
ret 0

ALIGN(4)
LGPJumpTable:
	DWORD Lnogp - LGPJumpTable
	DWORD Lrdi  - LGPJumpTable
	DWORD Lrsi  - LGPJumpTable
	DWORD Lrdx  - LGPJumpTable
	DWORD Lrcx  - LGPJumpTable
	DWORD Lr8   - LGPJumpTable
	DWORD Lr9   - LGPJumpTable

ALIGN(4)
LXMMJumpTable:
	DWORD Lxmm0  - LXMMJumpTable
	DWORD Lxmm1  - LXMMJumpTable
	DWORD Lxmm2  - LXMMJumpTable
	DWORD Lxmm3  - LXMMJumpTable
	DWORD Lxmm4  - LXMMJumpTable
	DWORD Lxmm5  - LXMMJumpTable
	DWORD Lxmm6  - LXMMJumpTable
	DWORD Lxmm7  - LXMMJumpTable
	DWORD Lxmm8  - LXMMJumpTable
	DWORD Lxmm9  - LXMMJumpTable
	DWORD Lxmm10 - LXMMJumpTable
	DWORD Lxmm11 - LXMMJumpTable
	DWORD Lxmm12 - LXMMJumpTable
	DWORD Lxmm13 - LXMMJumpTable
	DWORD Lxmm14 - LXMMJumpTable
	DWORD Lxmm15 - LXMMJumpTable
	DWORD Lxmm16 - LXMMJumpTable
SYSCallProcedureDynamically_linuxcc ENDP
_TEXT ENDS
END
