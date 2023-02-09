global SYSCallProcedureDynamically_linuxcc
global SYSCallProcedureDynamically_windowscc

section .text

%define stackFrameSize 192

%define procAddress   r13
%define gpArgCount    r14
%define gpArgData     r15
%define xmmArgCount   rcx
%define xmmArgData    r8
%define stackArgCount r9
%define stackArgData  r10
SYSCallProcedureDynamically_linuxcc:
; rdi: procedure address
; rsi: gp arg count   [0, 6)
; rdx: gp arg data
; rcx: xmm arg count  [0, 16)
; r8: xmm arg data
; r9: stack arg count [0, 16)
; (r10): stack arg data

push rbp
push r15
push r14
push r13
push r12

sub rsp, stackFrameSize

mov procAddress, rdi
mov gpArgCount,  rsi
mov gpArgData,   rdx
mov stackArgData, [rsp + 240]

; Stack parameters
xor r12, r12
.loop:
mov rax, [stackArgData + r12 * 8]
mov [rsp + r12 * 8], rax
inc r12
cmp r12, stackArgCount
jb .loop

; Jump table
lea r12, [rel .XMMJumpTable]
movsxd rax, [r12 + 4 * xmmArgCount]
add rax, r12
jmp rax

.xmm16:
movsd xmm15, [xmmArgData + 15 * 8]
.xmm15:
movsd xmm14, [xmmArgData + 14 * 8]
.xmm14:
movsd xmm13, [xmmArgData + 13 * 8]
.xmm13:
movsd xmm12, [xmmArgData + 12 * 8]
.xmm12:
movsd xmm11, [xmmArgData + 11 * 8]
.xmm11:
movsd xmm10, [xmmArgData + 10 * 8]
.xmm10:
movsd xmm9,  [xmmArgData + 9  * 8]
.xmm9:
movsd xmm8,  [xmmArgData + 8  * 8]
.xmm8:
movsd xmm7,  [xmmArgData + 7  * 8]
.xmm7:
movsd xmm6,  [xmmArgData + 6  * 8]
.xmm6:
movsd xmm5,  [xmmArgData + 5  * 8]
.xmm5:
movsd xmm4,  [xmmArgData + 4  * 8]
.xmm4:
movsd xmm3,  [xmmArgData + 3  * 8]
.xmm3:
movsd xmm2,  [xmmArgData + 2  * 8]
.xmm2:
movsd xmm1,  [xmmArgData + 1  * 8]
.xmm1:
movsd xmm0,  [xmmArgData + 0  * 8]
.xmm0:

; Jump table
lea r12, [rel .GPJumpTable]
movsxd rax, [r12 + 4 * gpArgCount]
add rax, r12
jmp rax

.r9:
mov r9,  [gpArgData + 5 * 8]
.r8:
mov r8,  [gpArgData + 4 * 8]
.rcx:
mov rcx, [gpArgData + 3 * 8]
.rdx:
mov rdx, [gpArgData + 2 * 8]
.rsi:
mov rsi, [gpArgData + 1 * 8]
.rdi:
mov rdi, [gpArgData + 0 * 8]
.nogp:

; Call
call procAddress

add rsp, stackFrameSize

pop r12
pop r13
pop r14
pop r15
pop rbp

ret 0

.GPJumpTable:
	.jt0: equ .nogp - .GPJumpTable
	.jt1: equ .rdi  - .GPJumpTable
	.jt2: equ .rsi  - .GPJumpTable
	.jt3: equ .rdx  - .GPJumpTable
	.jt4: equ .rcx  - .GPJumpTable
	.jt5: equ .r8   - .GPJumpTable
	.jt6: equ .r9   - .GPJumpTable
	dd .jt0, .jt1, .jt2, .jt3, .jt4, .jt5, .jt6

.XMMJumpTable:
	.jt7  equ .xmm0  - .XMMJumpTable
	.jt8  equ .xmm1  - .XMMJumpTable
	.jt9  equ .xmm2  - .XMMJumpTable
	.jt10 equ .xmm3  - .XMMJumpTable
	.jt11 equ .xmm4  - .XMMJumpTable
	.jt12 equ .xmm5  - .XMMJumpTable
	.jt13 equ .xmm6  - .XMMJumpTable
	.jt14 equ .xmm7  - .XMMJumpTable
	.jt15 equ .xmm8  - .XMMJumpTable
	.jt16 equ .xmm9  - .XMMJumpTable
	.jt17 equ .xmm10 - .XMMJumpTable
	.jt18 equ .xmm11 - .XMMJumpTable
	.jt19 equ .xmm12 - .XMMJumpTable
	.jt20 equ .xmm13 - .XMMJumpTable
	.jt21 equ .xmm14 - .XMMJumpTable
	.jt22 equ .xmm15 - .XMMJumpTable
	.jt23 equ .xmm16 - .XMMJumpTable
	dd .jt7, .jt8, .jt9, .jt10, .jt11, .jt12, .jt13, .jt14, .jt15, .jt16, .jt17, .jt18, .jt19, .jt20, .jt21, .jt22, .jt23


%define argCount   esi
%define argCount64 rsi
%define argData    r11
SYSCallProcedureDynamically_windowscc:
; rdi: procedure address
; rsi: arg count   [0, 6)
; rdx: arg data

push rbp

sub rsp, 128

mov r10, rdi
mov r11, rdx

; Stack parameters
mov ecx, 4
cmp ecx, argCount
jae .regArgs
.loop:
mov rax, [argData + rcx * 8]
mov [rsp + rcx * 8], rax
inc ecx
cmp ecx, argCount
; We pushed at least one stack argument, so we know we need all 4 register arguments.
jae .r9
jmp .loop

; We only land here when argCount <= 4, so we can use the jump table directly.
.regArgs:
; Jump table
lea r12, [rel .GPJumpTable]
movsxd rax, [r12 + 4 * argCount64]
add rax, r12
jmp rax

.r9:
mov r9,  [argData + 3 * 8]
movq xmm3, r9
.r8:
mov r8,  [argData + 2 * 8]
movq xmm2, r8
.rdx:
mov rdx, [argData + 1 * 8]
movq xmm1, rdx
.rcx:
mov rcx, [argData + 0 * 8]
movq xmm0, rcx
.nogp:

; Call
call r10

add rsp, 128

pop rbp

ret 0

.GPJumpTable:
	.jt0: equ .nogp - .GPJumpTable
	.jt1: equ .rcx  - .GPJumpTable
	.jt2: equ .rdx  - .GPJumpTable
	.jt3: equ .r8   - .GPJumpTable
	.jt4: equ .r9   - .GPJumpTable
	dd .jt0, .jt1, .jt2, .jt3, .jt4
