global __LinuxStart
extern __LinuxMain

__LinuxStart:

; Command-line arguments array, passed as rdi, rsi registers
pop rdi
mov rsi, rsp
jmp __LinuxMain
