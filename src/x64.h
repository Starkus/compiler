#if USE_OWN_ASSEMBLER
extern "C" {
#include "xed/xed-interface.h"
}
#endif

enum X64OperandType
{
	OPERANDTYPE_NONE      = 0,
	OPERANDTYPE_REGISTER  = 1,
	OPERANDTYPE_MEMORY    = 2,
	OPERANDTYPE_REGMEM    = 3,
	OPERANDTYPE_IMMEDIATE = 4,
	OPERANDTYPE_ALL       = 7,
};

enum X64OperandAccess
{
	OPERANDACCESS_NONE      = 0,
	OPERANDACCESS_READ      = 1,
	OPERANDACCESS_WRITE     = 2,
	OPERANDACCESS_READWRITE = 3,
};

struct X64InstructionInfo
{
	String mnemonic;
	u8 operandTypesLeft;
	u8 operandAccessLeft;
	u8 operandTypesRight;
	u8 operandAccessRight;
	u8 operandTypesDest;
	u8 operandAccessDest;
#if USE_OWN_ASSEMBLER
	xed_iclass_enum_t xedIClass;
#endif
};

enum X64InstructionType
{
	X64_INT,
	X64_INT1,
	X64_INT3,
	X64_INTO,

	X64_MOV,
	X64_MOVZX,
	X64_MOVSX,
	X64_MOVSXD,
	X64_CQO,
	X64_PUSH,
	X64_POP,

	X64_Jump_Begin,
	X64_JMP,
	X64_JE,
	X64_JNE,
	X64_JG,
	X64_JL,
	X64_JGE,
	X64_JLE,
	X64_JA,
	X64_JB,
	X64_JAE,
	X64_JBE,
	X64_Jump_End,

	X64_CALL,
	X64_CALL_Indirect,
	X64_SYSCALL,
	X64_LEAVE,
	X64_RET,
	X64_LEA,
	X64_CMP,
	X64_TEST,
	X64_SETG,
	X64_SETL,
	X64_SETGE,
	X64_SETLE,
	X64_SETA,
	X64_SETB,
	X64_SETAE,
	X64_SETBE,
	X64_SETE,
	X64_SETNE,
	X64_ADD,
	X64_SUB,
	X64_MUL,
	X64_IMUL,
	X64_DIV,
	X64_IDIV,
	X64_SAR,
	X64_SAL,
	X64_SHR,
	X64_SHL,
	X64_AND,
	X64_OR,
	X64_XOR,
	X64_NOT,
	X64_NEG,
	X64_MOVSS,
	X64_MOVSD,
	X64_ADDSS,
	X64_ADDSD,
	X64_SUBSS,
	X64_SUBSD,
	X64_MULSS,
	X64_MULSD,
	X64_DIVSS,
	X64_DIVSD,
	X64_XORPS,
	X64_XORPD,
	X64_SQRTSS,
	X64_SQRTSD,
	X64_VXORPS,
	X64_VXORPD,
	X64_COMISS,
	X64_COMISD,
	X64_CVTSI2SS,
	X64_CVTSI2SD,
	X64_CVTTSS2SI,
	X64_CVTTSD2SI,
	X64_CVTSS2SD,
	X64_CVTSD2SS,
	X64_MOVUPS,
	X64_MOVAPS,
	X64_Count,
	X64_Ignore = X64_Count,
	X64_Comment,
	X64_Label,
	X64_Push_Scope,
	X64_Pop_Scope,
	X64_Push_Value,
	X64_Patch,
	X64_Patch_Many,
	X64_FullCount
};

u32 g_copyMemoryProcIdx = U32_MAX;
u32 g_zeroMemoryProcIdx = U32_MAX;

struct BEInstruction
{
	SourceLocation loc;
	X64InstructionType type;
	union
	{
		struct
		{
			IRValue dst;
			IRValue src;
			IRValue src2;
		};
		IRLabel *label;
		struct
		{
			// Proc call info
			union
			{
				u32 procedureIdx;
				IRValue procedureIRValue;
			};
			Array<u32, ThreadAllocator> liveValues;

			// These tell the liveness analisis what registers to flag as live.
			Array<u32, ThreadAllocator> parameterValues;
		};
		u32 valueIdx;
		struct
		{
			BEInstruction *patch1;
			BEInstruction *patch2;
		};
		Array<BEInstruction, LinearAllocator> patchInstructions;
		String comment;
	};
};
typedef BEInstruction X64Instruction;

struct BEFinalProcedure
{
	u32 procedureIdx;
	BucketArray<X64Instruction, LinearAllocator, 1024> instructions;
	u64 stackSize;
	BucketArray<Value, LinearAllocator, 256> localValues;
};
typedef BEFinalProcedure X64FinalProcedure;

enum X64FloatingType
{
	X64FLOATINGTYPE_NONE,
	X64FLOATINGTYPE_F32,
	X64FLOATINGTYPE_F64
};

struct X64InstructionStream
{
	struct Frame
	{
		X64Instruction *instruction;
		s64 idx;
	};

	BucketArray<X64Instruction, LinearAllocator, 1024> *instructionArray;
	s64 idx;
	s64 instructionArrayCount;
	FixedArray<Frame, 16> stack;
};

X64InstructionInfo x64InstructionInfos[X64_FullCount];

enum X64Register
{
	RAX_idx,
	RCX_idx,
	RDX_idx,
	RBX_idx,
	RSI_idx,
	RDI_idx,
	RSP_idx,
	RBP_idx,
	R8_idx ,
	R9_idx ,
	R10_idx,
	R11_idx,
	R12_idx,
	R13_idx,
	R14_idx,
	R15_idx,
	XMM0_idx,
	XMM1_idx,
	XMM2_idx,
	XMM3_idx,
	XMM4_idx,
	XMM5_idx,
	XMM6_idx,
	XMM7_idx,
	XMM8_idx,
	XMM9_idx,
	XMM10_idx,
	XMM11_idx,
	XMM12_idx,
	XMM13_idx,
	XMM14_idx,
	XMM15_idx,
	X64REGISTER_Count
};

const String x64RegisterNames64[] = {
	"rax"_s,
	"rcx"_s,
	"rdx"_s,
	"rbx"_s,
	"rsi"_s,
	"rdi"_s,
	"rsp"_s,
	"rbp"_s,
	"r8"_s,
	"r9"_s,
	"r10"_s,
	"r11"_s,
	"r12"_s,
	"r13"_s,
	"r14"_s,
	"r15"_s,
	"xmm0"_s,
	"xmm1"_s,
	"xmm2"_s,
	"xmm3"_s,
	"xmm4"_s,
	"xmm5"_s,
	"xmm6"_s,
	"xmm7"_s,
	"xmm8"_s,
	"xmm9"_s,
	"xmm10"_s,
	"xmm11"_s,
	"xmm12"_s,
	"xmm13"_s,
	"xmm14"_s,
	"xmm15"_s,
};

const String x64RegisterNames32[] = {
	"eax"_s,
	"ecx"_s,
	"edx"_s,
	"ebx"_s,
	"esi"_s,
	"edi"_s,
	"esp"_s,
	"ebp"_s,
	"r8d"_s,
	"r9d"_s,
	"r10d"_s,
	"r11d"_s,
	"r12d"_s,
	"r13d"_s,
	"r14d"_s,
	"r15d"_s,
};

const String x64RegisterNames16[] = {
	"ax"_s,
	"cx"_s,
	"dx"_s,
	"bx"_s,
	"si"_s,
	"di"_s,
	"sp"_s,
	"bp"_s,
	"r8w"_s,
	"r9w"_s,
	"r10w"_s,
	"r11w"_s,
	"r12w"_s,
	"r13w"_s,
	"r14w"_s,
	"r15w"_s,
};

const String x64RegisterNames8[] = {
	"al"_s,
	"cl"_s,
	"dl"_s,
	"bl"_s,
	"sil"_s,
	"dil"_s,
	"spl"_s,
	"bpl"_s,
	"r8b"_s,
	"r9b"_s,
	"r10b"_s,
	"r11b"_s,
	"r12b"_s,
	"r13b"_s,
	"r14b"_s,
	"r15b"_s,
};

const u64 RAX_bit	= 1llu << RAX_idx;
const u64 RCX_bit	= 1llu << RCX_idx;
const u64 RDX_bit	= 1llu << RDX_idx;
const u64 RBX_bit	= 1llu << RBX_idx;
const u64 RSI_bit	= 1llu << RSI_idx;
const u64 RDI_bit	= 1llu << RDI_idx;
const u64 RSP_bit	= 1llu << RSP_idx;
const u64 RBP_bit	= 1llu << RBP_idx;
const u64 R8_bit	= 1llu << R8_idx;
const u64 R9_bit	= 1llu << R9_idx;
const u64 R10_bit	= 1llu << R10_idx;
const u64 R11_bit	= 1llu << R11_idx;
const u64 R12_bit	= 1llu << R12_idx;
const u64 R13_bit	= 1llu << R13_idx;
const u64 R14_bit	= 1llu << R14_idx;
const u64 R15_bit	= 1llu << R15_idx;
const u64 XMM0_bit	= 1llu << XMM0_idx;
const u64 XMM1_bit	= 1llu << XMM1_idx;
const u64 XMM2_bit	= 1llu << XMM2_idx;
const u64 XMM3_bit	= 1llu << XMM3_idx;
const u64 XMM4_bit	= 1llu << XMM4_idx;
const u64 XMM5_bit	= 1llu << XMM5_idx;
const u64 XMM6_bit	= 1llu << XMM6_idx;
const u64 XMM7_bit	= 1llu << XMM7_idx;
const u64 XMM8_bit	= 1llu << XMM8_idx;
const u64 XMM9_bit	= 1llu << XMM9_idx;
const u64 XMM10_bit	= 1llu << XMM10_idx;
const u64 XMM11_bit	= 1llu << XMM11_idx;
const u64 XMM12_bit	= 1llu << XMM12_idx;
const u64 XMM13_bit	= 1llu << XMM13_idx;
const u64 XMM14_bit	= 1llu << XMM14_idx;
const u64 XMM15_bit	= 1llu << XMM15_idx;

const u64 callerSaveRegisters = RCX_bit  | RDX_bit  | R8_bit   | R9_bit   |
								R10_bit  | R11_bit  | XMM1_bit | XMM2_bit |
								XMM3_bit | XMM4_bit | XMM5_bit;

const u64 calleeSaveRegisters = RBX_bit   | RSI_bit   | RDI_bit   | RSP_bit   |
								RBP_bit   | R12_bit   | R13_bit   | R14_bit   |
								R15_bit   | XMM6_bit  | XMM7_bit  | XMM8_bit  |
								XMM8_bit  | XMM9_bit  | XMM10_bit | XMM11_bit |
								XMM12_bit | XMM13_bit | XMM14_bit | XMM15_bit;

X64Register x64ScratchRegisters[] = {
	RAX_idx, RBX_idx, RCX_idx, RDX_idx,
	RSI_idx, RDI_idx, R8_idx,  R9_idx,
	R10_idx, R11_idx, R12_idx, R13_idx,
	R14_idx, R15_idx };

IRValue RAX;
IRValue RCX;
IRValue RDX;
IRValue RBX;
IRValue RSI;
IRValue RDI;
IRValue RSP;
IRValue RBP;
IRValue R8;
IRValue R9;
IRValue R10;
IRValue R11;
IRValue R12;
IRValue R13;
IRValue R14;
IRValue R15;

IRValue EAX;
IRValue ECX;
IRValue EDX;
IRValue EBX;
IRValue ESI;
IRValue EDI;
IRValue ESP;
IRValue EBP;
IRValue R8D;
IRValue R9D;
IRValue R10D;
IRValue R11D;
IRValue R12D;
IRValue R13D;
IRValue R14D;
IRValue R15D;

IRValue AX;
IRValue CX;
IRValue DX;
IRValue BX;
IRValue SI;
IRValue DI;
IRValue SP;
IRValue BP;
IRValue R8W;
IRValue R9W;
IRValue R10W;
IRValue R11W;
IRValue R12W;
IRValue R13W;
IRValue R14W;
IRValue R15W;

IRValue AL;
IRValue CL;
IRValue DL;
IRValue BL;
IRValue SIL;
IRValue DIL;
IRValue SPL;
IRValue BPL;
IRValue R8B;
IRValue R9B;
IRValue R10B;
IRValue R11B;
IRValue R12B;
IRValue R13B;
IRValue R14B;
IRValue R15B;

IRValue XMM0;
IRValue XMM1;
IRValue XMM2;
IRValue XMM3;
IRValue XMM4;
IRValue XMM5;
IRValue XMM6;
IRValue XMM7;
IRValue XMM8;
IRValue XMM9;
IRValue XMM10;
IRValue XMM11;
IRValue XMM12;
IRValue XMM13;
IRValue XMM14;
IRValue XMM15;

IRValue x64Registers[X64REGISTER_Count] = {
	RAX,	RCX,	RDX,	RBX,
	RSI,	RDI,	RSP,	RBP,
	R8,		R9,		R10,	R11,
	R12,	R13,	R14,	R15,
	XMM0,	XMM1,	XMM2,	XMM3,
	XMM4,	XMM5,	XMM6,	XMM7,
	XMM8,	XMM9,	XMM10,	XMM11,
	XMM12,	XMM13,	XMM14,	XMM15
};

#if USE_OWN_ASSEMBLER
xed_reg_enum_t x64RegisterToXED[X64REGISTER_Count] = {
	XED_REG_RAX,
	XED_REG_RCX,
	XED_REG_RDX,
	XED_REG_RBX,
	XED_REG_RSI,
	XED_REG_RDI,
	XED_REG_RSP,
	XED_REG_RBP,
	XED_REG_R8,
	XED_REG_R9,
	XED_REG_R10,
	XED_REG_R11,
	XED_REG_R12,
	XED_REG_R13,
	XED_REG_R14,
	XED_REG_R15,
	XED_REG_XMM0,
	XED_REG_XMM1,
	XED_REG_XMM2,
	XED_REG_XMM3,
	XED_REG_XMM4,
	XED_REG_XMM5,
	XED_REG_XMM6,
	XED_REG_XMM7,
	XED_REG_XMM8,
	XED_REG_XMM9,
	XED_REG_XMM10,
	XED_REG_XMM11,
	XED_REG_XMM12,
	XED_REG_XMM13,
	XED_REG_XMM14,
	XED_REG_XMM15
};

xed_reg_enum_t x64RegisterToXED32[] = {
	XED_REG_EAX,
	XED_REG_ECX,
	XED_REG_EDX,
	XED_REG_EBX,
	XED_REG_ESI,
	XED_REG_EDI,
	XED_REG_ESP,
	XED_REG_EBP,
	XED_REG_R8D,
	XED_REG_R9D,
	XED_REG_R10D,
	XED_REG_R11D,
	XED_REG_R12D,
	XED_REG_R13D,
	XED_REG_R14D,
	XED_REG_R15D
};

xed_reg_enum_t x64RegisterToXED16[] = {
	XED_REG_AX,
	XED_REG_CX,
	XED_REG_DX,
	XED_REG_BX,
	XED_REG_SI,
	XED_REG_DI,
	XED_REG_SP,
	XED_REG_BP,
	XED_REG_R8W,
	XED_REG_R9W,
	XED_REG_R10W,
	XED_REG_R11W,
	XED_REG_R12W,
	XED_REG_R13W,
	XED_REG_R14W,
	XED_REG_R15W
};

xed_reg_enum_t x64RegisterToXED8[] = {
	XED_REG_AL,
	XED_REG_CL,
	XED_REG_DL,
	XED_REG_BL,
	XED_REG_SIL,
	XED_REG_DIL,
	XED_REG_SPL,
	XED_REG_BPL,
	XED_REG_R8B,
	XED_REG_R9B,
	XED_REG_R10B,
	XED_REG_R11B,
	XED_REG_R12B,
	XED_REG_R13B,
	XED_REG_R14B,
	XED_REG_R15B
};
#endif

#if IS_LINUX
String x64LinuxSyscallNames[] =
{
	"read"_s,
	"write"_s,
	"open"_s,
	"close"_s,
	"stat"_s,
	"fstat"_s,
	"lstat"_s,
	"poll"_s,
	"lseek"_s,
	"mmap"_s,
	"mprotect"_s,
	"munmap"_s,
	"brk"_s,
	"rt_sigaction"_s,
	"rt_sigprocmask"_s,
	"rt_sigreturn"_s,
	"ioctl"_s,
	"pread64"_s,
	"pwrite64"_s,
	"readv"_s,
	"writev"_s,
	"access"_s,
	"pipe"_s,
	"select"_s,
	"sched_yield"_s,
	"mremap"_s,
	"msync"_s,
	"mincore"_s,
	"madvise"_s,
	"shmget"_s,
	"shmat"_s,
	"shmctl"_s,
	"dup"_s,
	"dup2"_s,
	"pause"_s,
	"nanosleep"_s,
	"getitimer"_s,
	"alarm"_s,
	"setitimer"_s,
	"getpid"_s,
	"sendfile"_s,
	"socket"_s,
	"connect"_s,
	"accept"_s,
	"sendto"_s,
	"recvfrom"_s,
	"sendmsg"_s,
	"recvmsg"_s,
	"shutdown"_s,
	"bind"_s,
	"listen"_s,
	"getsockname"_s,
	"getpeername"_s,
	"socketpair"_s,
	"setsockopt"_s,
	"getsockopt"_s,
	"clone"_s,
	"fork"_s,
	"vfork"_s,
	"execve"_s,
	"exit"_s,
	"wait4"_s,
	"kill"_s,
	"uname"_s,
	"semget"_s,
	"semop"_s,
	"semctl"_s,
	"shmdt"_s,
	"msgget"_s,
	"msgsnd"_s,
	"msgrcv"_s,
	"msgctl"_s,
	"fcntl"_s,
	"flock"_s,
	"fsync"_s,
	"fdatasync"_s,
	"truncate"_s,
	"ftruncate"_s,
	"getdents"_s,
	"getcwd"_s,
	"chdir"_s,
	"fchdir"_s,
	"rename"_s,
	"mkdir"_s,
	"rmdir"_s,
	"creat"_s,
	"link"_s,
	"unlink"_s,
	"symlink"_s,
	"readlink"_s,
	"chmod"_s,
	"fchmod"_s,
	"chown"_s,
	"fchown"_s,
	"lchown"_s,
	"umask"_s,
	"gettimeofday"_s,
	"getrlimit"_s,
	"getrusage"_s,
	"sysinfo"_s,
	"times"_s,
	"ptrace"_s,
	"getuid"_s,
	"syslog"_s,
	"getgid"_s,
	"setuid"_s,
	"setgid"_s,
	"geteuid"_s,
	"getegid"_s,
	"setpgid"_s,
	"getppid"_s,
	"getpgrp"_s,
	"setsid"_s,
	"setreuid"_s,
	"setregid"_s,
	"getgroups"_s,
	"setgroups"_s,
	"setresuid"_s,
	"getresuid"_s,
	"setresgid"_s,
	"getresgid"_s,
	"getpgid"_s,
	"setfsuid"_s,
	"setfsgid"_s,
	"getsid"_s,
	"capget"_s,
	"capset"_s,
	"rt_sigpending"_s,
	"rt_sigtimedwait"_s,
	"rt_sigqueueinfo"_s,
	"rt_sigsuspend"_s,
	"sigaltstack"_s,
	"utime"_s,
	"mknod"_s,
	"uselib"_s,
	"personality"_s,
	"ustat"_s,
	"statfs"_s,
	"fstatfs"_s,
	"sysfs"_s,
	"getpriority"_s,
	"setpriority"_s,
	"sched_setparam"_s,
	"sched_getparam"_s,
	"sched_setscheduler"_s,
	"sched_getscheduler"_s,
	"sched_get_priority_max"_s,
	"sched_get_priority_min"_s,
	"sched_rr_get_interval"_s,
	"mlock"_s,
	"munlock"_s,
	"mlockall"_s,
	"munlockall"_s,
	"vhangup"_s,
	"modify_ldt"_s,
	"pivot_root"_s,
	"_sysctl"_s,
	"prctl"_s,
	"arch_prctl"_s,
	"adjtimex"_s,
	"setrlimit"_s,
	"chroot"_s,
	"sync"_s,
	"acct"_s,
	"settimeofday"_s,
	"mount"_s,
	"umount2"_s,
	"swapon"_s,
	"swapoff"_s,
	"reboot"_s,
	"sethostname"_s,
	"setdomainname"_s,
	"iopl"_s,
	"ioperm"_s,
	"create_module"_s,
	"init_module"_s,
	"delete_module"_s,
	"get_kernel_syms"_s,
	"query_module"_s,
	"quotactl"_s,
	"nfsservctl"_s,
	"getpmsg"_s,
	"putpmsg"_s,
	"afs_syscall"_s,
	"tuxcall"_s,
	"security"_s,
	"gettid"_s,
	"readahead"_s,
	"setxattr"_s,
	"lsetxattr"_s,
	"fsetxattr"_s,
	"getxattr"_s,
	"lgetxattr"_s,
	"fgetxattr"_s,
	"listxattr"_s,
	"llistxattr"_s,
	"flistxattr"_s,
	"removexattr"_s,
	"lremovexattr"_s,
	"fremovexattr"_s,
	"tkill"_s,
	"time"_s,
	"futex"_s,
	"sched_setaffinity"_s,
	"sched_getaffinity"_s,
	"set_thread_area"_s,
	"io_setup"_s,
	"io_destroy"_s,
	"io_getevents"_s,
	"io_submit"_s,
	"io_cancel"_s,
	"get_thread_area"_s,
	"lookup_dcookie"_s,
	"epoll_create"_s,
	"epoll_ctl_old"_s,
	"epoll_wait_old"_s,
	"remap_file_pages"_s,
	"getdents64"_s,
	"set_tid_address"_s,
	"restart_syscall"_s,
	"semtimedop"_s,
	"fadvise64"_s,
	"timer_create"_s,
	"timer_settime"_s,
	"timer_gettime"_s,
	"timer_getoverrun"_s,
	"timer_delete"_s,
	"clock_settime"_s,
	"clock_gettime"_s,
	"clock_getres"_s,
	"clock_nanosleep"_s,
	"exit_group"_s,
	"epoll_wait"_s,
	"epoll_ctl"_s,
	"tgkill"_s,
	"utimes"_s,
	"vserver"_s,
	"mbind"_s,
	"set_mempolicy"_s,
	"get_mempolicy"_s,
	"mq_open"_s,
	"mq_unlink"_s,
	"mq_timedsend"_s,
	"mq_timedreceive"_s,
	"mq_notify"_s,
	"mq_getsetattr"_s,
	"kexec_load"_s,
	"waitid"_s,
	"add_key"_s,
	"request_key"_s,
	"keyctl"_s,
	"ioprio_set"_s,
	"ioprio_get"_s,
	"inotify_init"_s,
	"inotify_add_watch"_s,
	"inotify_rm_watch"_s,
	"migrate_pages"_s,
	"openat"_s,
	"mkdirat"_s,
	"mknodat"_s,
	"fchownat"_s,
	"futimesat"_s,
	"newfstatat"_s,
	"unlinkat"_s,
	"renameat"_s,
	"linkat"_s,
	"symlinkat"_s,
	"readlinkat"_s,
	"fchmodat"_s,
	"faccessat"_s,
	"pselect6"_s,
	"ppoll"_s,
	"unshare"_s,
	"set_robust_list"_s,
	"get_robust_list"_s,
	"splice"_s,
	"tee"_s,
	"sync_file_range"_s,
	"vmsplice"_s,
	"move_pages"_s,
	"utimensat"_s,
	"epoll_pwait"_s,
	"signalfd"_s,
	"timerfd_create"_s,
	"eventfd"_s,
	"fallocate"_s,
	"timerfd_settime"_s,
	"timerfd_gettime"_s,
	"accept4"_s,
	"signalfd4"_s,
	"eventfd2"_s,
	"epoll_create1"_s,
	"dup3"_s,
	"pipe2"_s,
	"inotify_init1"_s,
	"preadv"_s,
	"pwritev"_s,
	"rt_tgsigqueueinfo"_s,
	"perf_event_open"_s,
	"recvmmsg"_s,
	"fanotify_init"_s,
	"fanotify_mark"_s,
	"prlimit64"_s,
	"name_to_handle_at"_s,
	"open_by_handle_at"_s,
	"clock_adjtime"_s,
	"syncfs"_s,
	"sendmmsg"_s,
	"setns"_s,
	"getcpu"_s,
	"process_vm_readv"_s,
	"process_vm_writev"_s,
	"kcmp"_s,
	"finit_module"_s
};
#endif

