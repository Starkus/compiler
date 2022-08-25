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
};

enum X64InstructionType
{
	X64_INT,

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

s32 copyMemoryProcIdx;
s32 zeroMemoryProcIdx;

struct BEInstruction
{
	X64InstructionType type;
	union
	{
		struct
		{
			IRValue dst;
			IRValue src;
		};
		IRLabel *label;
		struct
		{
			// Proc call info
			union
			{
				s32 procedureIdx;
				IRValue procedureIRValue;
			};
			Array<u32, PhaseAllocator> liveValues;

			// These tell the liveness analisis what registers to flag as live.
			Array<u32, PhaseAllocator> parameterValues;
		};
		u32 valueIdx;
		struct
		{
			BEInstruction *patch1;
			BEInstruction *patch2;
		};
		Array<BEInstruction, PhaseAllocator> patchInstructions;
		String comment;
	};
};
typedef BEInstruction X64Instruction;

struct X64Procedure
{
	String name;
	BucketArray<X64Instruction, PhaseAllocator, 1024> instructions;
	u64 stackSize;
	s64 allocatedParameterCount;
	u32 returnValueIdx;
	DynamicArray<u32, PhaseAllocator> spilledValues;
};

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

	X64Procedure *procedure;
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

X64Register x64ScratchRegisters[] = {
	RAX_idx, RBX_idx, RCX_idx, RDX_idx,
	RSI_idx, RDI_idx, R8_idx,  R9_idx,
	R10_idx, R11_idx, R12_idx, R13_idx,
	R14_idx, R15_idx };

u32 x64SpilledParametersWrite[32];
u32 x64SpilledParametersRead[32];

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

#if !_MSC_VER
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

X64InstructionStream X64InstructionStreamBegin(X64Procedure *proc)
{
	X64InstructionStream stream;
	stream.procedure = proc;
	stream.idx = -1;
	stream.instructionArrayCount = BucketArrayCount(&proc->instructions);
	stream.stack.size = 0;
	return stream;
}
X64Instruction *X64InstructionStreamAdvance(X64InstructionStream *iterator)
{
	X64Instruction *result = nullptr;

	while (true)
	{
		if (iterator->stack.size == 0)
		{
			++iterator->idx;
			if (iterator->idx >= iterator->instructionArrayCount)
				return nullptr;
			result = &iterator->procedure->instructions[iterator->idx];
			break;
		}
		else
		{
			X64InstructionStream::Frame *frame = &iterator->stack[iterator->stack.size - 1];
			if (frame->instruction->type == X64_Patch)
			{
				if (++frame->idx == 1)
				{
					result = frame->instruction->patch2;
					break;
				}
				else
				{
					--iterator->stack.size;
					continue;
				}
			}
			else if (frame->instruction->type == X64_Patch_Many)
			{
				if (++frame->idx < (s64)frame->instruction->patchInstructions.size)
				{
					result = &frame->instruction->patchInstructions[frame->idx];
					break;
				}
				else
				{
					--iterator->stack.size;
					continue;
				}
			}
		}
	}

	while (true)
	{
		if (result->type == X64_Patch)
		{
			*FixedArrayAdd(&iterator->stack) = { result, 0 };
			result = result->patch1;
		}
		else if (result->type == X64_Patch_Many)
		{
			*FixedArrayAdd(&iterator->stack) = { result, 0 };
			result = &result->patchInstructions[0];
		}
		else
			break;
	}
	return result;
}

s64 PrintOut(Context *context, const char *format, ...)
{
	char *buffer = (char *)g_memory->framePtr;

	va_list args;
	va_start(args, format);

	s64 size = stbsp_vsprintf(buffer, format, args);

	s64 bytesToWrite = size;
	const char *in = buffer;
	while (bytesToWrite > 0)
	{
		auto *lastBucket = DynamicArrayBack(&context->outputBuffer.buckets);
		s64 bytesLeftInBucket = OUTPUT_BUFFER_BUCKET_SIZE - lastBucket->size;
		u8 *bufferCursor = lastBucket->data + lastBucket->size;
		if (bytesToWrite > bytesLeftInBucket)
		{
			memcpy(bufferCursor, in, bytesLeftInBucket);
			in += bytesLeftInBucket;
			lastBucket->size += bytesLeftInBucket;
			bytesToWrite -= bytesLeftInBucket;

			lastBucket = DynamicArrayAdd(&context->outputBuffer.buckets);
			ArrayInit(lastBucket, OUTPUT_BUFFER_BUCKET_SIZE);
		}
		else
		{
			memcpy(bufferCursor, in, size);
			in += bytesLeftInBucket;
			lastBucket->size += bytesToWrite;
			bytesToWrite -= bytesToWrite;
		}
	}

#if DEBUG_BUILD
	memset(g_memory->framePtr, 0xCD, size + 1);
#endif

	va_end(args);
	return size;
}

String X64IRValueToStr(Context *context, IRValue value)
{
	String result = "???VALUE"_s;

	ASSERT(value.valueType != IRVALUETYPE_IMMEDIATE_FLOAT);
	ASSERT(value.valueType != IRVALUETYPE_IMMEDIATE_STRING);

	if (value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
	{
		result = TPrintF("%lld", value.immediate);
		return result;
	}

	u64 size = 0;
	TypeInfo typeInfo = GetTypeInfo(context, StripAllAliases(context, value.typeTableIdx));
	bool isXMM;
	size = typeInfo.size;
	Value v = GetValueRead(context, value.value.valueIdx);

	s64 offset = 0;
	if (value.valueType == IRVALUETYPE_VALUE || value.valueType == IRVALUETYPE_VALUE_DEREFERENCE)
		offset = value.value.offset;

	if (value.valueType == IRVALUETYPE_PROCEDURE)
	{
		result = GetProcedureRead(context, value.procedureIdx).name;
		goto decoratePtr;
	}
	else if (v.flags & (VALUEFLAGS_ON_STATIC_STORAGE | VALUEFLAGS_IS_EXTERNAL))
	{
		if (v.flags & VALUEFLAGS_IS_EXTERNAL)
			result = v.name;
		else
			result = StringConcat("g_"_s, v.name);

		if (offset > 0)
			result = TPrintF("%S+0%xh", result, offset);
		else if (offset < 0)
			result = TPrintF("%S-0%xh", result, -offset);

		// Array indexing
		if (value.value.elementSize > 0)
		{
			String indexRegisterStr = X64IRValueToStr(context, IRValueValue(context, value.value.indexValueIdx));
			result = TPrintF("%S+%S*%llu", result, indexRegisterStr, value.value.elementSize);
		}

		goto decoratePtr;
	}

	isXMM = typeInfo.size > 8 || typeInfo.typeCategory == TYPECATEGORY_FLOATING;

	if (v.flags & VALUEFLAGS_IS_ALLOCATED)
	{
		if (v.flags & VALUEFLAGS_IS_MEMORY)
		{
			ASSERT(!(v.flags & VALUEFLAGS_FORCE_REGISTER));
			offset += v.stackOffset;
			if (v.flags & VALUEFLAGS_BASE_RELATIVE)
				result = "rbp"_s;
			else
				result = "rsp"_s;

			if (offset > 0)
				result = TPrintF("%S+0%xh", result, offset);
			else if (offset < 0)
				result = TPrintF("%S-0%xh", result, -offset);

			// Array indexing
			if (value.value.elementSize > 0)
			{
				String indexRegisterStr = X64IRValueToStr(context, IRValueValue(context, value.value.indexValueIdx));
				result = TPrintF("%S+%S*%llu", result, indexRegisterStr, value.value.elementSize);
			}
		}
		else if (value.valueType == IRVALUETYPE_VALUE_DEREFERENCE)
		{
			switch (v.allocatedRegister)
			{
				case RAX_idx: result = "rax"_s; break;
				case RCX_idx: result = "rcx"_s; break;
				case RDX_idx: result = "rdx"_s; break;
				case RBX_idx: result = "rbx"_s; break;
				case RSI_idx: result = "rsi"_s; break;
				case RDI_idx: result = "rdi"_s; break;
				case RSP_idx: result = "rsp"_s; break;
				case RBP_idx: result = "rbp"_s; break;
				case R8_idx:  result = "r8"_s;  break;
				case R9_idx:  result = "r9"_s;  break;
				case R10_idx: result = "r10"_s; break;
				case R11_idx: result = "r11"_s; break;
				case R12_idx: result = "r12"_s; break;
				case R13_idx: result = "r13"_s; break;
				case R14_idx: result = "r14"_s; break;
				case R15_idx: result = "r15"_s; break;
				default:
					ASSERTF(false, "Value \"%S\" not allocated to GP register!", v.name);
			}

			if (offset > 0)
				result = TPrintF("%S+0%xh", result, offset);
			else if (offset < 0)
				result = TPrintF("%S-0%xh", result, -offset);

			// Array indexing
			if (value.value.elementSize > 0)
			{
				String indexRegisterStr = X64IRValueToStr(context, IRValueValue(context, value.value.indexValueIdx));
				result = TPrintF("%S+%S*%llu", result, indexRegisterStr, value.value.elementSize);
			}
		}
		else if (!isXMM)
		{
			s32 registerIdx = v.allocatedRegister;
			switch (size)
			{
			case 8:
				switch (registerIdx)
				{
				case RAX_idx: result = "rax"_s; break;
				case RCX_idx: result = "rcx"_s; break;
				case RDX_idx: result = "rdx"_s; break;
				case RBX_idx: result = "rbx"_s; break;
				case RSI_idx: result = "rsi"_s; break;
				case RDI_idx: result = "rdi"_s; break;
				case RSP_idx: result = "rsp"_s; break;
				case RBP_idx: result = "rbp"_s; break;
				case R8_idx:  result = "r8"_s;  break;
				case R9_idx:  result = "r9"_s;  break;
				case R10_idx: result = "r10"_s; break;
				case R11_idx: result = "r11"_s; break;
				case R12_idx: result = "r12"_s; break;
				case R13_idx: result = "r13"_s; break;
				case R14_idx: result = "r14"_s; break;
				case R15_idx: result = "r15"_s; break;
				default:
					ASSERTF(false, "Value \"%S\" not allocated to GP register!", v.name);
				}
				break;
			case 4:
				switch (registerIdx)
				{
				case RAX_idx: result = "eax"_s; break;
				case RCX_idx: result = "ecx"_s; break;
				case RDX_idx: result = "edx"_s; break;
				case RBX_idx: result = "ebx"_s; break;
				case RSI_idx: result = "esi"_s; break;
				case RDI_idx: result = "edi"_s; break;
				case RSP_idx: result = "esp"_s; break;
				case RBP_idx: result = "ebp"_s; break;
				case R8_idx:  result = "r8d"_s;  break;
				case R9_idx:  result = "r9d"_s;  break;
				case R10_idx: result = "r10d"_s; break;
				case R11_idx: result = "r11d"_s; break;
				case R12_idx: result = "r12d"_s; break;
				case R13_idx: result = "r13d"_s; break;
				case R14_idx: result = "r14d"_s; break;
				case R15_idx: result = "r15d"_s; break;
				default:
					ASSERTF(false, "Value \"%S\" not allocated to GP register!", v.name);
				}
				break;
			case 2:
				switch (registerIdx)
				{
				case RAX_idx: result = "ax"_s; break;
				case RCX_idx: result = "cx"_s; break;
				case RDX_idx: result = "dx"_s; break;
				case RBX_idx: result = "bx"_s; break;
				case RSI_idx: result = "si"_s; break;
				case RDI_idx: result = "di"_s; break;
				case RSP_idx: result = "sp"_s; break;
				case RBP_idx: result = "bp"_s; break;
				case R8_idx:  result = "r8w"_s;  break;
				case R9_idx:  result = "r9w"_s;  break;
				case R10_idx: result = "r10w"_s; break;
				case R11_idx: result = "r11w"_s; break;
				case R12_idx: result = "r12w"_s; break;
				case R13_idx: result = "r13w"_s; break;
				case R14_idx: result = "r14w"_s; break;
				case R15_idx: result = "r15w"_s; break;
				default:
					ASSERTF(false, "Value \"%S\" not allocated to GP register!", v.name);
				}
				break;
			case 1:
				switch (registerIdx)
				{
				case RAX_idx: result = "al"_s; break;
				case RCX_idx: result = "cl"_s; break;
				case RDX_idx: result = "dl"_s; break;
				case RBX_idx: result = "bl"_s; break;
				case RSI_idx: result = "sil"_s; break;
				case RDI_idx: result = "dil"_s; break;
				case RSP_idx: result = "spl"_s; break;
				case RBP_idx: result = "bpl"_s; break;
				case R8_idx:  result = "r8b"_s;  break;
				case R9_idx:  result = "r9b"_s;  break;
				case R10_idx: result = "r10b"_s; break;
				case R11_idx: result = "r11b"_s; break;
				case R12_idx: result = "r12b"_s; break;
				case R13_idx: result = "r13b"_s; break;
				case R14_idx: result = "r14b"_s; break;
				case R15_idx: result = "r15b"_s; break;
				default:
					ASSERTF(false, "Value \"%S\" not allocated to GP register!", v.name);
				}
				break;
			default:
				ASSERT(!"Invalid size for a register!");
			}

			if (offset > 0)
				result = TPrintF("%S+0%xh", result, offset);
			else if (offset < 0)
				result = TPrintF("%S-0%xh", result, -offset);

			// Array indexing
			if (value.value.elementSize > 0)
			{
				String indexRegisterStr = X64IRValueToStr(context, IRValueValue(context, value.value.indexValueIdx));
				result = TPrintF("%S+%S*%llu", result, indexRegisterStr, value.value.elementSize);
			}
		}
		else switch (v.allocatedRegister)
		{
		case XMM0_idx:  result = "xmm0"_s;  break;
		case XMM1_idx:  result = "xmm1"_s;  break;
		case XMM2_idx:  result = "xmm2"_s;  break;
		case XMM3_idx:  result = "xmm3"_s;  break;
		case XMM4_idx:  result = "xmm4"_s;  break;
		case XMM5_idx:  result = "xmm5"_s;  break;
		case XMM6_idx:  result = "xmm6"_s;  break;
		case XMM7_idx:  result = "xmm7"_s;  break;
		case XMM8_idx:  result = "xmm8"_s;  break;
		case XMM9_idx:  result = "xmm9"_s;  break;
		case XMM10_idx: result = "xmm10"_s; break;
		case XMM11_idx: result = "xmm11"_s; break;
		case XMM12_idx: result = "xmm12"_s; break;
		case XMM13_idx: result = "xmm13"_s; break;
		case XMM14_idx: result = "xmm14"_s; break;
		case XMM15_idx: result = "xmm15"_s; break;
		default:
			ASSERTF(false, "Value \"%S\" not allocated to XMM register!", v.name);
		}
	}
	// Not allocated
	else
	{
		if (v.name)
			result = TPrintF("$vr%d\"%S\"", value.value.valueIdx, v.name);
		else
			result = TPrintF("$vr%d", value.value.valueIdx);

		if (offset > 0)
			result = TPrintF("%S+0%xh", result, offset);
		else if (offset < 0)
			result = TPrintF("%S-0%xh", result, -offset);

		// Array indexing
		if (value.value.elementSize > 0)
		{
			String indexRegisterStr = X64IRValueToStr(context, IRValueValue(context, value.value.indexValueIdx));
			result = TPrintF("%S+%S*%llu", result, indexRegisterStr, value.value.elementSize);
		}
	}

	if (value.valueType != IRVALUETYPE_VALUE_DEREFERENCE && !(v.flags & VALUEFLAGS_IS_MEMORY))
		return result;

decoratePtr:
	{
#if _MSC_VER
		switch (size)
		{
		case 1:
			result = TPrintF("BYTE PTR [%S]", result);
			break;
		case 2:
			result = TPrintF("WORD PTR [%S]", result);
			break;
		case 4:
			result = TPrintF("DWORD PTR [%S]", result);
			break;
		case 8:
			result = TPrintF("QWORD PTR [%S]", result);
			break;
		case 16:
			result = TPrintF("XMMWORD PTR [%S]", result);
			break;
		default:
			ASSERT(!"Invalid register size");
		}
#else
		//result = TPrintF("[%S]", result);
		switch (size)
		{
		case 1:
			result = TPrintF("BYTE [%S]", result);
			break;
		case 2:
			result = TPrintF("WORD [%S]", result);
			break;
		case 4:
			result = TPrintF("DWORD [%S]", result);
			break;
		case 8:
			result = TPrintF("QWORD [%S]", result);
			break;
		case 16:
			result = TPrintF("OWORD [%S]", result);
			break;
		default:
			ASSERT(!"Invalid register size");
		}
#endif
	}
	return result;
}

bool IsValueInMemory(Context *context, IRValue irValue)
{
	if (irValue.valueType == IRVALUETYPE_VALUE_DEREFERENCE)
		return true;
	if (irValue.valueType != IRVALUETYPE_VALUE)
		return false;
	Value value = GetValueRead(context, irValue.value.valueIdx);
	if (value.flags & (VALUEFLAGS_FORCE_MEMORY | VALUEFLAGS_IS_MEMORY |
				VALUEFLAGS_ON_STATIC_STORAGE))
		return true;
	return false;
}

bool FitsInOperand(Context *context, u8 acceptableOperands, IRValue value)
{
	bool isImmediate = value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER ||
					   value.valueType == IRVALUETYPE_IMMEDIATE_FLOAT;
	if (isImmediate)
		return acceptableOperands & OPERANDTYPE_IMMEDIATE;
	if (!IsValueInMemory(context, value))
		return acceptableOperands & OPERANDTYPE_REGISTER;
	return acceptableOperands & OPERANDTYPE_MEMORY;
}

bool CanValueBeMemory(Context *context, IRValue value)
{
	if (value.valueType == IRVALUETYPE_VALUE_DEREFERENCE)
		return true;
	bool isImmediate = value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER ||
					   value.valueType == IRVALUETYPE_IMMEDIATE_FLOAT;
	if (isImmediate)
		return false;
	if (GetValueRead(context, value.value.valueIdx).flags & VALUEFLAGS_FORCE_REGISTER)
		return false;
	if ((GetValueRead(context, value.value.valueIdx).flags & (VALUEFLAGS_IS_ALLOCATED |
			VALUEFLAGS_IS_MEMORY)) == VALUEFLAGS_IS_ALLOCATED)
		return false;
	return true;
}

void X64Mov(Context *context, X64Procedure *x64Proc, IRValue dst, IRValue src);
void X64MovNoTmp(Context *context, X64Procedure *x64Proc, IRValue dst, IRValue src)
{
	X64Instruction result;
	TypeInfo dstType = GetTypeInfo(context, StripAllAliases(context, dst.typeTableIdx));
	TypeInfo srcType = GetTypeInfo(context, StripAllAliases(context, src.typeTableIdx));

	// MOVUPS
	if (dstType.size == 16)
	{
		ASSERT(srcType.size == 16);
		result.type = X64_MOVUPS;
		result.dst = dst;
		result.src = src;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	}

	ASSERT(dstType.size <= 8);
	ASSERT(srcType.size <= 8);

	if (dstType.typeCategory != TYPECATEGORY_FLOATING)
	{
		if (srcType.typeCategory != TYPECATEGORY_FLOATING)
		{
			result.type = X64_MOV;
			bool isSigned = srcType.typeCategory == TYPECATEGORY_INTEGER &&
				srcType.integerInfo.isSigned;
			if (srcType.size == 4)
			{
				if (isSigned && dstType.size > 4 && src.valueType != IRVALUETYPE_IMMEDIATE_INTEGER)
				{
					// MOVSXD is R-RM
					IRValue newValue = IRValueNewValue(context, "_movsxd_tmp"_s, dst.typeTableIdx,
							VALUEFLAGS_FORCE_REGISTER);
					*BucketArrayAdd(&x64Proc->instructions) = { X64_MOVSXD, newValue, src };
					src = newValue;
				}
				ASSERT(dstType.size >= 4);
				dst.typeTableIdx = src.typeTableIdx;
			}
			else if (srcType.size < dstType.size && src.valueType != IRVALUETYPE_IMMEDIATE_INTEGER)
			{
				X64InstructionType extendType = isSigned ? X64_MOVSX : X64_MOVZX;
				// MOVSX and MOVZX are R-RM
				IRValue newValue = IRValueNewValue(context, "_movzx_tmp"_s, dst.typeTableIdx,
						VALUEFLAGS_FORCE_REGISTER);
				*BucketArrayAdd(&x64Proc->instructions) = { extendType, newValue, src };
				src = newValue;
			}
			else if (srcType.size > dstType.size)
				src.typeTableIdx = dst.typeTableIdx;
		}
		else
		{
			// X64_CVTTSD2SI and CVTTSD2SI are R-RM
			ASSERT(dst.valueType == IRVALUETYPE_VALUE ||
				   dst.valueType == IRVALUETYPE_VALUE_DEREFERENCE);
			IRValue newValue = IRValueNewValue(context, "_cvttsd2si_tmp"_s, dst.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER | VALUEFLAGS_TRY_IMMITATE, dst.value.valueIdx);
			X64InstructionType type;

			if (srcType.size == 4)
				type = X64_CVTTSS2SI;
			else
			{
				ASSERT(srcType.size == 8);
				type = X64_CVTTSD2SI;
			}
			*BucketArrayAdd(&x64Proc->instructions) = { type, newValue, src };

			result.type = X64_MOV;
			src = newValue;
		}
	}
	else if (dstType.size == 4)
	{
		if (srcType.typeCategory != TYPECATEGORY_FLOATING)
		{
			// Immediates should be converted to float in previous stages.
			ASSERT(src.valueType != IRVALUETYPE_IMMEDIATE_INTEGER);
			if (srcType.size < 4)
			{
				bool isSigned = srcType.typeCategory == TYPECATEGORY_INTEGER &&
					srcType.integerInfo.isSigned;
				IRValue newValue = IRValueNewValue(context, "_cvt_tmp"_s, TYPETABLEIDX_U32,
						VALUEFLAGS_FORCE_REGISTER);
				X64InstructionType extendType = isSigned ? X64_MOVSX : X64_MOVZX;
				*BucketArrayAdd(&x64Proc->instructions) = { extendType, newValue, src };
				src = newValue;
			}
			result.type = X64_CVTSI2SS;
		}
		else if (srcType.size == 4)
			result.type = X64_MOVSS;
		else
		{
			ASSERT(srcType.size == 8);
			result.type = X64_CVTSD2SS;
		}
	}
	else
	{
		ASSERT(dstType.size == 8);
		if (srcType.typeCategory != TYPECATEGORY_FLOATING)
		{
			// Immediates should be converted to float in previous stages.
			ASSERT(src.valueType != IRVALUETYPE_IMMEDIATE_INTEGER);
			if (srcType.size < 4)
			{
				bool isSigned = srcType.typeCategory == TYPECATEGORY_INTEGER &&
					srcType.integerInfo.isSigned;
				IRValue newValue = IRValueNewValue(context, "_cvt_tmp"_s, TYPETABLEIDX_U32,
						VALUEFLAGS_FORCE_REGISTER);
				X64InstructionType extendType = isSigned ? X64_MOVSX : X64_MOVZX;
				*BucketArrayAdd(&x64Proc->instructions) = { extendType, newValue, src };
				src = newValue;
			}
			result.type = X64_CVTSI2SD;
		}
		else if (srcType.size == 4)
			result.type = X64_CVTSS2SD;
		else
		{
			ASSERT(srcType.size == 8);
			result.type = X64_MOVSD;
		}
	}

	result.dst = dst;
	result.src = src;
	*BucketArrayAdd(&x64Proc->instructions) = result;
}

void X64Mov(Context *context, X64Procedure *x64Proc, IRValue dst, IRValue src)
{
	if (CanValueBeMemory(context, dst) && CanValueBeMemory(context, src))
	{
		Value srcValue = GetValueRead(context, src.value.valueIdx);
		u32 srcUsedFlag = srcValue.flags & VALUEFLAGS_IS_USED;
		u32 immitateFlag = src.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
		IRValue tmp = IRValueNewValue(context, "_movtmp"_s, dst.typeTableIdx,
				VALUEFLAGS_FORCE_REGISTER | srcUsedFlag | immitateFlag, src.value.valueIdx);

		X64MovNoTmp(context, x64Proc, tmp, src);
		src = tmp;
	}
	// Can't directly mov a 64 bit immediate to a memory location
	else if (CanValueBeMemory(context, dst) &&
		src.valueType == IRVALUETYPE_IMMEDIATE_INTEGER &&
		src.immediate & 0xFFFFFFFF00000000)
	{
		IRValue tmp = IRValueNewValue(context, "_movimmtmp"_s, dst.typeTableIdx,
				VALUEFLAGS_FORCE_REGISTER);
		X64MovNoTmp(context, x64Proc, tmp, src);
		src = tmp;
	}

	X64MovNoTmp(context, x64Proc, dst, src);
}

void X64Test(Context *context, X64Procedure *x64Proc, IRValue value)
{
	X64FloatingType floatingType = X64FLOATINGTYPE_NONE;
	TypeInfo typeInfo = GetTypeInfo(context, value.typeTableIdx);
	if (typeInfo.typeCategory == TYPECATEGORY_FLOATING)
		floatingType = (X64FloatingType)(X64FLOATINGTYPE_F32 + (typeInfo.size == 8));

	X64Instruction cmpInst;
	cmpInst.dst = value;
	switch (floatingType)
	{
	case X64FLOATINGTYPE_NONE:
	{
		cmpInst.type = X64_CMP;
		cmpInst.src = IRValueImmediate(0);
	} break;
	case X64FLOATINGTYPE_F32:
	{
		cmpInst.type = X64_COMISS;
		IRValue zero = IRValueNewValue(context, "_zero"_s, cmpInst.dst.typeTableIdx, 0);
		*BucketArrayAdd(&x64Proc->instructions) = { X64_XORPS, zero, zero };
		cmpInst.src = zero;
	} break;
	case X64FLOATINGTYPE_F64:
	{
		cmpInst.type = X64_COMISD;
		IRValue zero = IRValueNewValue(context, "_zero"_s, cmpInst.dst.typeTableIdx, 0);
		*BucketArrayAdd(&x64Proc->instructions) = { X64_XORPD, zero, zero };
		cmpInst.src = zero;
	} break;
	default:
		ASSERT(false);
	}

	u8 accepted = x64InstructionInfos[cmpInst.type].operandTypesLeft;
	if (!FitsInOperand(context, accepted, cmpInst.dst))
	{
		ASSERT(accepted & OPERANDTYPE_REGISTER);
		IRValue newValue = IRValueNewValue(context, "_test_hlp"_s, cmpInst.dst.typeTableIdx,
				VALUEFLAGS_FORCE_REGISTER);
		X64Mov(context, x64Proc, newValue, cmpInst.dst);
		cmpInst.dst = newValue;
	}

	*BucketArrayAdd(&x64Proc->instructions) = cmpInst;
}

IRValue X64PushRegisterParameter(u32 typeTableIdx, s32 *numberOfGPR, s32 *numberOfXMM)
{
	bool isXMM = typeTableIdx == TYPETABLEIDX_F32 || typeTableIdx == TYPETABLEIDX_F64;

	if (!isXMM) switch((*numberOfGPR)++)
	{
		case 0: return RDI;
		case 1: return RSI;
		case 2: return RDX;
		case 3: return RCX;
		case 4: return R8;
		case 5: return R9;
	}
	else if (*numberOfXMM < 16)
	{
		return IRValueValue(XMM0.value.valueIdx + (*numberOfXMM)++, typeTableIdx);
	}
	return { IRVALUETYPE_INVALID };
}

void X64CopyMemory(Context *context, X64Procedure *x64Proc, IRValue dst, IRValue src, IRValue size)
{
	ASSERT(dst.valueType == IRVALUETYPE_VALUE ||
		   dst.valueType == IRVALUETYPE_VALUE_DEREFERENCE);
	ASSERT(src.valueType == IRVALUETYPE_VALUE ||
		   src.valueType == IRVALUETYPE_VALUE_DEREFERENCE);
	u32 dstIdx = dst.value.valueIdx;
	u32 srcIdx = src.value.valueIdx;

	// First attempt to copy manually
	if (size.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
	{
		TypeInfo dstTypeInfo = GetTypeInfo(context, GetValueRead(context, dstIdx).typeTableIdx);
		TypeInfo srcTypeInfo = GetTypeInfo(context, GetValueRead(context, srcIdx).typeTableIdx);
		s64 sizeImm = size.immediate;

		s64 copiedBytes = 0;
		while (sizeImm - copiedBytes >= 16)
		{
			X64Mov(context, x64Proc,
					IRValueDereference(dstIdx, TYPETABLEIDX_128, copiedBytes),
					IRValueDereference(srcIdx, TYPETABLEIDX_128, copiedBytes));
			copiedBytes += 16;
		}
		while (sizeImm - copiedBytes >= 8)
		{
			X64Mov(context, x64Proc,
					IRValueDereference(dstIdx, TYPETABLEIDX_S64, copiedBytes),
					IRValueDereference(srcIdx, TYPETABLEIDX_S64, copiedBytes));
			copiedBytes += 8;
		}
		while (sizeImm - copiedBytes >= 4)
		{
			X64Mov(context, x64Proc,
					IRValueDereference(dstIdx, TYPETABLEIDX_S32, copiedBytes),
					IRValueDereference(srcIdx, TYPETABLEIDX_S32, copiedBytes));
			copiedBytes += 4;
		}
		while (sizeImm - copiedBytes >= 1)
		{
			X64Mov(context, x64Proc,
					IRValueDereference(dstIdx, TYPETABLEIDX_S8, copiedBytes),
					IRValueDereference(srcIdx, TYPETABLEIDX_S8, copiedBytes));
			++copiedBytes;
		}
		return;
	}

	X64Mov(context, x64Proc, RCX, dst);
	X64Mov(context, x64Proc, RDX, src);
	X64Mov(context, x64Proc, R8,  size);
	X64Instruction result = { X64_CALL };
	result.procedureIdx = copyMemoryProcIdx;
	ArrayInit(&result.parameterValues, 3);
	*ArrayAdd(&result.parameterValues) = RCX.value.valueIdx;
	*ArrayAdd(&result.parameterValues) = RDX.value.valueIdx;
	*ArrayAdd(&result.parameterValues) = R8.value.valueIdx;
	*BucketArrayAdd(&x64Proc->instructions) = result;
}

bool X64WinABIShouldPassByCopy(Context *context, u32 typeTableIdx)
{
	TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
	// @Speed
	return  typeInfo.typeCategory == TYPECATEGORY_ARRAY ||
		  ((typeInfo.typeCategory == TYPECATEGORY_STRUCT ||
			typeInfo.typeCategory == TYPECATEGORY_UNION) &&
			typeInfo.size != 1 &&
			typeInfo.size != 2 &&
			typeInfo.size != 4 &&
			typeInfo.size != 8);
}

Array<u32, PhaseAllocator> X64ReadyWin64Parameters(Context *context, X64Procedure *x64Proc,
		ArrayView<IRValue> parameters, bool isCaller)
{
	int parameterCount = (int)parameters.size;

	Array<u32, PhaseAllocator> parameterValues;
	ArrayInit(&parameterValues, parameterCount * 2);

	for (int i = 0; i < parameterCount; ++i)
	{
		IRValue param = parameters[i];
		u32 paramTypeIdx = StripAllAliases(context, param.typeTableIdx);
		TypeInfo paramType = GetTypeInfo(context, paramTypeIdx);

		if (isCaller && X64WinABIShouldPassByCopy(context, paramTypeIdx))
		{
			static u32 voidPtrTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_VOID);

			u32 tmpValueIdx = NewValue(context, "paramcpy"_s, paramTypeIdx, 0);
			IRValue tmpValue = IRValueValue(tmpValueIdx, voidPtrTypeIdx);

			X64Instruction pushInst = { X64_Push_Value };
			pushInst.valueIdx = tmpValueIdx;
			*BucketArrayAdd(&x64Proc->instructions) = pushInst;

			IRValue ptr = IRValueNewValue(context, "paramptr"_s, TYPETABLEIDX_S64, 0);
			*BucketArrayAdd(&x64Proc->instructions) = { X64_LEA, ptr, tmpValue };

			X64CopyMemory(context, x64Proc, ptr, param, IRValueImmediate(paramType.size));
			param = ptr;
			paramTypeIdx = TYPETABLEIDX_S64;
		}

		bool isXMM = paramType.typeCategory == TYPECATEGORY_FLOATING;

		IRValue slot;
		switch(i)
		{
		case 0:
			slot = isXMM ? XMM0 : RCX;
			break;
		case 1:
			slot = isXMM ? XMM1 : RDX;
			break;
		case 2:
			slot = isXMM ? XMM2 : R8;
			break;
		case 3:
			slot = isXMM ? XMM3 : R9;
			break;
		default:
			if (isCaller)
				slot = IRValueValue(x64SpilledParametersWrite[i], TYPETABLEIDX_S64);
			else
				slot = IRValueValue(x64SpilledParametersRead[i], TYPETABLEIDX_S64);
		}
		slot.typeTableIdx = paramTypeIdx;

		if (isCaller)
			X64Mov(context, x64Proc, slot, param);
		else
		{
			if (X64WinABIShouldPassByCopy(context, paramTypeIdx))
			{
				u32 ptrTypeIdx = GetTypeInfoPointerOf(context, paramTypeIdx);
				param.typeTableIdx = ptrTypeIdx;
				slot.typeTableIdx = ptrTypeIdx;
				X64CopyMemory(context, x64Proc, param, slot,
						IRValueImmediate(paramType.size));
			}
			else
				X64Mov(context, x64Proc, param, slot);
		}

		*ArrayAdd(&parameterValues) = slot.value.valueIdx;
	}

	if (isCaller)
	{
		if (x64Proc->allocatedParameterCount < parameterCount)
			x64Proc->allocatedParameterCount = parameterCount;
	}

	return parameterValues;
}

Array<u32, PhaseAllocator> X64ReadyLinuxParameters(Context *context, X64Procedure *x64Proc,
		ArrayView<IRValue> parameters, bool isCaller)
{
	int parameterCount = (int)parameters.size;

	Array<u32, PhaseAllocator> parameterValues;
	ArrayInit(&parameterValues, parameterCount * 2);

	s32 numberOfGPR = 0;
	s32 numberOfXMM = 0;
	s32 numberOfSpilled = 0;
	for (int i = 0; i < parameterCount; ++i)
	{
		IRValue param = parameters[i];
		u32 paramTypeIdx = param.typeTableIdx;

		TypeInfo paramTypeInfo = GetTypeInfo(context, paramTypeIdx);
		bool isStruct = paramTypeInfo.typeCategory == TYPECATEGORY_STRUCT ||
			(paramTypeInfo.typeCategory == TYPECATEGORY_ARRAY && paramTypeInfo.arrayInfo.count == 0);
		if (isStruct && paramTypeInfo.size <= 16)
		{
			Array<StructMember, FrameAllocator> members;
			if (paramTypeInfo.typeCategory == TYPECATEGORY_ARRAY)
				members = GetTypeInfo(context, TYPETABLEIDX_ARRAY_STRUCT).structInfo.members;
			else
				members = paramTypeInfo.structInfo.members;

			IRValue first, second = {};
			int regCount = 1;

			first = IRValueDereference(param.value.valueIdx, TYPETABLEIDX_S64, 0);
			if (members[0].typeTableIdx == TYPETABLEIDX_F64 ||
			   (members.size == 2 &&
				members[0].typeTableIdx == TYPETABLEIDX_F32 &&
				members[1].typeTableIdx == TYPETABLEIDX_F32))
			{
				// An F64 or two consecutive F32s into an XMM register.
				first.typeTableIdx = TYPETABLEIDX_F64;
			}
			else if (members[0].typeTableIdx == TYPETABLEIDX_F32 &&
					(members.size == 1 || members[1].offset >= 8))
			{
				// An only F32 into XMM register
				first.typeTableIdx = TYPETABLEIDX_F32;
			}
			else
			{
				if (paramTypeInfo.size > 4)
					first.typeTableIdx = TYPETABLEIDX_S64;
				else if (paramTypeInfo.size > 2)
					first.typeTableIdx = TYPETABLEIDX_S32;
				else if (paramTypeInfo.size > 1)
					first.typeTableIdx = TYPETABLEIDX_S16;
				else
					first.typeTableIdx = TYPETABLEIDX_S8;
			}

			int firstMember = 0;
			for (; firstMember < members.size; ++firstMember)
			{
				if (members[firstMember].offset >= 8)
					break;
			}
			if (firstMember > 0 && firstMember < members.size)
			{
				second = IRValueDereference(param.value.valueIdx, TYPETABLEIDX_S64, 8);
				regCount = 2;

				if (members[firstMember].typeTableIdx == TYPETABLEIDX_F64 ||
				   (firstMember < members.size - 1 &&
					members[firstMember].typeTableIdx   == TYPETABLEIDX_F32 &&
					members[firstMember+1].typeTableIdx == TYPETABLEIDX_F32))
				{
					// An F64 or two consecutive F32s into an XMM register.
					second.typeTableIdx = TYPETABLEIDX_F64;
				}
				else if (members[firstMember].typeTableIdx == TYPETABLEIDX_F32 &&
						(firstMember == members.size - 1 ||
						 members[firstMember+1].offset >= 8))
				{
					// An only F32 into XMM register
					second.typeTableIdx = TYPETABLEIDX_F32;
				}
				else
				{
					if (paramTypeInfo.size > 12)
						second.typeTableIdx = TYPETABLEIDX_S64;
					else if (paramTypeInfo.size > 10)
						second.typeTableIdx = TYPETABLEIDX_S32;
					else if (paramTypeInfo.size > 9)
						second.typeTableIdx = TYPETABLEIDX_S16;
					else
						second.typeTableIdx = TYPETABLEIDX_S8;
				}
			}

			s32 oldNumberOfGPR = numberOfGPR;
			s32 oldNumberOfXMM = numberOfXMM;
			if (regCount >= 1)
			{
				IRValue firstSlot = X64PushRegisterParameter(first.typeTableIdx, &numberOfGPR, &numberOfXMM);
				IRValue secondSlot = { IRVALUETYPE_INVALID };
				if (regCount >= 2)
					secondSlot = X64PushRegisterParameter(second.typeTableIdx, &numberOfGPR, &numberOfXMM);

				if (firstSlot.valueType != IRVALUETYPE_INVALID &&
				   (regCount < 2 || secondSlot.valueType != IRVALUETYPE_INVALID))
				{
					if (isCaller)
					{
						X64Mov(context, x64Proc, firstSlot,  first);
						if (secondSlot.valueType != IRVALUETYPE_INVALID)
							X64Mov(context, x64Proc, secondSlot, second);
					}
					else
					{
						X64Mov(context, x64Proc, first,  firstSlot);
						if (secondSlot.valueType != IRVALUETYPE_INVALID)
							X64Mov(context, x64Proc, second, secondSlot);
					}

					*ArrayAdd(&parameterValues) = firstSlot.value.valueIdx;
					*ArrayAdd(&parameterValues) = secondSlot.value.valueIdx;

					continue;
				}
				else
				{
					// Restore number of used registers and keep going.
					numberOfGPR = oldNumberOfGPR;
					numberOfXMM = oldNumberOfXMM;
				}
			}
		}

		if (paramTypeInfo.size > 8)
		{
			int sizeLeft = (int)paramTypeInfo.size;
			while (sizeLeft > 0)
			{
				u32 typeTableIdx = TYPETABLEIDX_S8;
				if (sizeLeft > 4)
					typeTableIdx = TYPETABLEIDX_S64;
				else if (sizeLeft > 2)
					typeTableIdx = TYPETABLEIDX_S32;
				else if (sizeLeft > 1)
					typeTableIdx = TYPETABLEIDX_S16;
				param.typeTableIdx = typeTableIdx;

				if (isCaller)
				{
					IRValue slot = IRValueDereference(x64SpilledParametersWrite[numberOfSpilled++],
							typeTableIdx);
					X64Mov(context, x64Proc, slot, param);
				}
				else
				{
					IRValue slot = IRValueDereference(x64SpilledParametersRead[numberOfSpilled++],
							typeTableIdx);
					X64Mov(context, x64Proc, param, slot);
				}
				param.value.offset += 8;
				sizeLeft -= 8;
			}
		}
		else
		{
			IRValue slot;
			if (isCaller)
			{
				slot = X64PushRegisterParameter(param.typeTableIdx, &numberOfGPR, &numberOfXMM);
				if (slot.valueType == IRVALUETYPE_INVALID)
					slot = IRValueDereference(x64SpilledParametersWrite[numberOfSpilled++],
							TYPETABLEIDX_S64);

				X64Mov(context, x64Proc, slot, param);
			}
			else
			{
				slot = X64PushRegisterParameter(param.typeTableIdx, &numberOfGPR, &numberOfXMM);
				if (slot.valueType == IRVALUETYPE_INVALID)
					slot = IRValueDereference(x64SpilledParametersRead[numberOfSpilled++],
							TYPETABLEIDX_S64);

				X64Mov(context, x64Proc, param, slot);
			}

			*ArrayAdd(&parameterValues) = slot.value.valueIdx;
		}
	}

	if (isCaller)
	{
		if (x64Proc->allocatedParameterCount < numberOfSpilled)
			x64Proc->allocatedParameterCount = numberOfSpilled;
	}

	return parameterValues;
}

void X64ConvertInstruction(Context *context, IRInstruction inst, X64Procedure *x64Proc)
{
	X64Instruction result = {};

	X64FloatingType floatingType = X64FLOATINGTYPE_NONE;
	bool isSigned = false;
	{
		u32 typeTableIdx = TYPETABLEIDX_S64;
		if      (inst.type >= IRINSTRUCTIONTYPE_COMPARE_BEGIN &&
				 inst.type <  IRINSTRUCTIONTYPE_COMPARE_END)
			typeTableIdx = inst.binaryOperation.left.typeTableIdx;
		else if (inst.type >= IRINSTRUCTIONTYPE_BINARY_BEGIN &&
				 inst.type <  IRINSTRUCTIONTYPE_BINARY_END)
			typeTableIdx = inst.binaryOperation.left.typeTableIdx;
		else if (inst.type >= IRINSTRUCTIONTYPE_UNARY_BEGIN &&
				 inst.type <  IRINSTRUCTIONTYPE_UNARY_END)
			typeTableIdx = inst.unaryOperation.in.typeTableIdx;
		else if (inst.type >= IRINSTRUCTIONTYPE_COMPARE_JUMP_BEGIN &&
				 inst.type <  IRINSTRUCTIONTYPE_COMPARE_JUMP_END)
			typeTableIdx = inst.conditionalJump2.left.typeTableIdx;
		else if (inst.type == IRINSTRUCTIONTYPE_ASSIGNMENT)
			typeTableIdx = inst.assignment.dst.typeTableIdx;

		typeTableIdx = StripAllAliases(context, typeTableIdx);
		TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);

		if (typeInfo.typeCategory == TYPECATEGORY_FLOATING)
		{
			floatingType = (X64FloatingType)(X64FLOATINGTYPE_F32 + (typeInfo.size == 8));
			isSigned = true;
		}
		else if (typeInfo.typeCategory == TYPECATEGORY_INTEGER)
			isSigned = typeInfo.integerInfo.isSigned;
	}

	switch (inst.type)
	{
	case IRINSTRUCTIONTYPE_ASSIGNMENT:
	{
		X64Mov(context, x64Proc, inst.assignment.dst, inst.assignment.src);
		return;
	}
	case IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS:
	{
		static u32 voidPtrTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_VOID);

		IRValue dst = inst.assignment.dst;
		IRValue src = inst.assignment.src;
		src.typeTableIdx = voidPtrTypeIdx;
		if (IsValueInMemory(context, dst))
		{
			IRValue tmp = IRValueNewValue(context, "_lea_mm_tmp"_s, dst.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER);
			*BucketArrayAdd(&x64Proc->instructions) = { X64_LEA, tmp, src };
			X64Mov(context, x64Proc, dst, tmp);
			src = tmp;
		}
		else
			*BucketArrayAdd(&x64Proc->instructions) = { X64_LEA, dst, src };
		return;
	}
	case IRINSTRUCTIONTYPE_ADD:
		switch (floatingType)
		{
		case X64FLOATINGTYPE_NONE:
			result.type = X64_ADD;
			goto doRM_RMI;
		case X64FLOATINGTYPE_F32:
			result.type = X64_ADDSS;
			goto doX_XM;
		case X64FLOATINGTYPE_F64:
			result.type = X64_ADDSD;
			goto doX_XM;
		}
	case IRINSTRUCTIONTYPE_SUBTRACT:
		switch (floatingType)
		{
		case X64FLOATINGTYPE_NONE:
			result.type = X64_SUB;
			goto doRM_RMI;
		case X64FLOATINGTYPE_F32:
			result.type = X64_SUBSS;
			goto doX_XM;
		case X64FLOATINGTYPE_F64:
			result.type = X64_SUBSD;
			goto doX_XM;
		}
	case IRINSTRUCTIONTYPE_SUBTRACT_UNARY:
	{
		X64Mov(context, x64Proc, inst.unaryOperation.out, inst.unaryOperation.in);
		switch (floatingType)
		{
		case X64FLOATINGTYPE_NONE:
			result.type = X64_NEG;
			goto doRM;
		case X64FLOATINGTYPE_F32:
			result.type = X64_XORPS;
			result.src = IRValueImmediateFloat(context, -0.0, TYPETABLEIDX_F32);
			result.src.typeTableIdx = TYPETABLEIDX_128;
			break;
		case X64FLOATINGTYPE_F64:
			result.type = X64_XORPD;
			result.src = IRValueImmediateFloat(context, -0.0, TYPETABLEIDX_128);
			break;
		}
		result.dst = inst.unaryOperation.out;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	}
	case IRINSTRUCTIONTYPE_BITWISE_AND:
		result.type = X64_AND;
		goto doRM_RMI;
	case IRINSTRUCTIONTYPE_BITWISE_OR:
		result.type = X64_OR;
		goto doRM_RMI;
	case IRINSTRUCTIONTYPE_BITWISE_XOR:
		result.type = X64_XOR;
		goto doRM_RMI;
	case IRINSTRUCTIONTYPE_BITWISE_NOT:
		result.type = X64_NOT;
		goto doRM;
	case IRINSTRUCTIONTYPE_MULTIPLY:
		switch (floatingType)
		{
		case X64FLOATINGTYPE_NONE:
		{
			IRValue left  = inst.binaryOperation.left;
			IRValue right = inst.binaryOperation.right;
			IRValue out   = inst.binaryOperation.out;

			if (left.valueType == IRVALUETYPE_IMMEDIATE_INTEGER && left.immediate > 0 &&
					IsPowerOf264(left.immediate))
			{
				IRValue tmp = left;
				left = right;
				right = tmp;
			}

			if (right.valueType == IRVALUETYPE_IMMEDIATE_INTEGER && right.immediate > 0 &&
					IsPowerOf264(right.immediate))
			{
				u32 immitateFlag = left.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
				IRValue tmp = IRValueNewValue(context, "_mulshfttmp"_s, left.typeTableIdx,
						immitateFlag, left.value.valueIdx);

				X64Mov(context, x64Proc, tmp, left);
				*BucketArrayAdd(&x64Proc->instructions) = { X64_SAL, tmp,
						IRValueImmediate(63 - Nlz64(right.immediate)) };
				X64Mov(context, x64Proc, out, tmp);
				return;
			}
			else
			{
				if (isSigned)
				{
					result.type = X64_IMUL;
					goto doRM_RMI;
				}
				else
				{
					X64Mov(context, x64Proc, RAX, left);

					*BucketArrayAdd(&x64Proc->instructions) = { X64_XOR, RDX, RDX };

					IRValue multiplier = right;
					u8 accepted = x64InstructionInfos[X64_MUL].operandTypesLeft;
					if (!FitsInOperand(context, accepted, multiplier))
					{
						ASSERT(accepted & OPERANDTYPE_REGISTER);
						IRValue newValue = IRValueNewValue(context, multiplier.typeTableIdx,
								VALUEFLAGS_FORCE_REGISTER);
						X64Mov(context, x64Proc, newValue, multiplier);
						multiplier = newValue;
					}
					result.type = X64_MUL;
					result.dst = multiplier;
					*BucketArrayAdd(&x64Proc->instructions) = result;

					X64Mov(context, x64Proc, out, RAX);
					return;
				}
			}
		}
		case X64FLOATINGTYPE_F32:
			result.type = X64_MULSS;
			goto doX_XM;
		case X64FLOATINGTYPE_F64:
			result.type = X64_MULSD;
			goto doX_XM;
		}
	case IRINSTRUCTIONTYPE_DIVIDE:
		switch (floatingType)
		{
		case X64FLOATINGTYPE_NONE:
		{
			IRValue left  = inst.binaryOperation.left;
			IRValue right = inst.binaryOperation.right;
			IRValue out   = inst.binaryOperation.out;

			if (right.valueType == IRVALUETYPE_IMMEDIATE_INTEGER && IsPowerOf264(right.immediate))
			{
				u32 immitateFlag = left.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
				IRValue tmp = IRValueNewValue(context, "_mulshfttmp"_s, left.typeTableIdx,
						immitateFlag, left.value.valueIdx);

				TypeInfo leftType = GetTypeInfo(context, left.typeTableIdx);
				X64InstructionType shiftType = isSigned ? X64_SAR : X64_SHR;

				X64Mov(context, x64Proc, tmp, left);
				*BucketArrayAdd(&x64Proc->instructions) = { shiftType, tmp,
						IRValueImmediate(63 - Nlz64(right.immediate)) };
				X64Mov(context, x64Proc, out, tmp);
			}
			else
			{
				X64Mov(context, x64Proc, RAX, left);

				if (isSigned)
					*BucketArrayAdd(&x64Proc->instructions) = { X64_CQO };
				else
					*BucketArrayAdd(&x64Proc->instructions) = { X64_XOR, RDX, RDX };

				IRValue divisor = right;
				u8 accepted = x64InstructionInfos[X64_DIV].operandTypesLeft;
				if (!FitsInOperand(context, accepted, divisor))
				{
					ASSERT(accepted & OPERANDTYPE_REGISTER);
					IRValue newValue = IRValueNewValue(context, divisor.typeTableIdx,
							VALUEFLAGS_FORCE_REGISTER);
					X64Mov(context, x64Proc, newValue, divisor);
					divisor = newValue;
				}
				result.type = isSigned ? X64_IDIV : X64_DIV;
				result.dst = divisor;
				*BucketArrayAdd(&x64Proc->instructions) = result;

				X64Mov(context, x64Proc, out, RAX);
			}
			return;
		}
		case X64FLOATINGTYPE_F32:
			result.type = X64_DIVSS;
			goto doX_XM;
		case X64FLOATINGTYPE_F64:
			result.type = X64_DIVSD;
			goto doX_XM;
		}
	case IRINSTRUCTIONTYPE_MODULO:
	{
		IRValue left  = inst.binaryOperation.left;
		IRValue right = inst.binaryOperation.right;
		IRValue out   = inst.binaryOperation.out;

		if (right.valueType == IRVALUETYPE_IMMEDIATE_INTEGER && IsPowerOf264(right.immediate))
		{
			u32 immitateFlag = left.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
			IRValue tmp = IRValueNewValue(context, "_mulshfttmp"_s, left.typeTableIdx, immitateFlag,
					left.value.valueIdx);

			X64Mov(context, x64Proc, tmp, left);
			*BucketArrayAdd(&x64Proc->instructions) = { X64_AND, tmp,
					IRValueImmediate(right.immediate - 1) };
			X64Mov(context, x64Proc, out, tmp);
		}
		else
		{
			X64Mov(context, x64Proc, RAX, left);
			if (isSigned)
				*BucketArrayAdd(&x64Proc->instructions) = { X64_CQO };
			else
				*BucketArrayAdd(&x64Proc->instructions) = { X64_XOR, RDX, RDX };

			IRValue divisor = right;
			u8 accepted = x64InstructionInfos[X64_DIV].operandTypesLeft;
			if (!FitsInOperand(context, accepted, divisor))
			{
				ASSERT(accepted & OPERANDTYPE_REGISTER);
				IRValue newValue = IRValueNewValue(context, divisor.typeTableIdx,
						VALUEFLAGS_FORCE_REGISTER);
				X64Mov(context, x64Proc, newValue, divisor);
				divisor = newValue;
			}
			result.type = isSigned ? X64_IDIV : X64_DIV;
			result.dst = divisor;

			*BucketArrayAdd(&x64Proc->instructions) = result;
			X64Mov(context, x64Proc, out, RDX);
		}
		return;
	}
	case IRINSTRUCTIONTYPE_SHIFT_LEFT:
	{
		TypeInfo leftType = GetTypeInfo(context, inst.binaryOperation.left.typeTableIdx);
		result.type = isSigned ? X64_SAL : X64_SHL;
		goto doShift;
	}
	case IRINSTRUCTIONTYPE_SHIFT_RIGHT:
	{
		TypeInfo leftType = GetTypeInfo(context, inst.binaryOperation.left.typeTableIdx);
		result.type = isSigned ? X64_SAR : X64_SHR;
		goto doShift;
	}
	case IRINSTRUCTIONTYPE_LABEL:
		result.type = X64_Label;
		result.label = inst.label;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	case IRINSTRUCTIONTYPE_JUMP:
		result.type = X64_JMP;
		result.label = inst.jump.label;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	case IRINSTRUCTIONTYPE_JUMP_IF_ZERO:
		result.type = X64_JE;
		goto doConditionalJump;
	case IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO:
		result.type = X64_JNE;
		goto doConditionalJump;
	case IRINSTRUCTIONTYPE_JUMP_IF_EQUALS:
		result.type = X64_JE;
		goto doConditionalJump2;
	case IRINSTRUCTIONTYPE_JUMP_IF_NOT_EQUALS:
		result.type = X64_JNE;
		goto doConditionalJump2;
	case IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN:
		if (isSigned && floatingType == X64FLOATINGTYPE_NONE)
			result.type = X64_JG;
		else
			result.type = X64_JA;
		goto doConditionalJump2;
	case IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN:
		if (isSigned && floatingType == X64FLOATINGTYPE_NONE)
			result.type = X64_JL;
		else
			result.type = X64_JB;
		goto doConditionalJump2;
	case IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN_OR_EQUALS:
		if (isSigned && floatingType == X64FLOATINGTYPE_NONE)
			result.type = X64_JGE;
		else
			result.type = X64_JAE;
		goto doConditionalJump2;
	case IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN_OR_EQUALS:
		if (isSigned && floatingType == X64FLOATINGTYPE_NONE)
			result.type = X64_JLE;
		else
			result.type = X64_JBE;
		goto doConditionalJump2;
	case IRINSTRUCTIONTYPE_GREATER_THAN:
		if (isSigned && floatingType == X64FLOATINGTYPE_NONE)
			result.type = X64_SETG;
		else
			result.type = X64_SETA;
		goto doConditionalSet;
	case IRINSTRUCTIONTYPE_LESS_THAN:
		if (isSigned && floatingType == X64FLOATINGTYPE_NONE)
			result.type = X64_SETL;
		else
			result.type = X64_SETB;
		goto doConditionalSet;
	case IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS:
		if (isSigned && floatingType == X64FLOATINGTYPE_NONE)
			result.type = X64_SETGE;
		else
			result.type = X64_SETAE;
		goto doConditionalSet;
	case IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS:
		if (isSigned && floatingType == X64FLOATINGTYPE_NONE)
			result.type = X64_SETBE;
		else
			result.type = X64_SETBE;
		goto doConditionalSet;
	case IRINSTRUCTIONTYPE_EQUALS:
		result.type = X64_SETE;
		goto doConditionalSet;
	case IRINSTRUCTIONTYPE_NOT_EQUALS:
		result.type = X64_SETNE;
		goto doConditionalSet;
	case IRINSTRUCTIONTYPE_NOT:
	{
		X64Test(context, x64Proc, inst.unaryOperation.in);

		result.type = X64_SETE;
		result.dst = inst.unaryOperation.out;
		if (GetTypeInfo(context, result.dst.typeTableIdx).size != 1)
		{
			X64Mov(context, x64Proc, result.dst, IRValueImmediate(0));
			result.dst.typeTableIdx = TYPETABLEIDX_S8;
		}

		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	}
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL:
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL_INDIRECT:
	{
		CallingConvention callingConvention;

		// At this point, we have the actual values that go into registers/stack slots. If something
		// is passed by copy, we already have the pointer to the copy as argument value, so we don't
		// care.
		if (inst.type == IRINSTRUCTIONTYPE_PROCEDURE_CALL)
		{
			result.type = X64_CALL;
			result.procedureIdx = inst.procedureCall.procedureIdx;

			u32 procTypeIdx = GetProcedureRead(context, result.procedureIdx).typeTableIdx;
			callingConvention = GetTypeInfo(context, procTypeIdx).procedureInfo.callingConvention;
		}
		else
		{
			result.type = X64_CALL_Indirect;
			result.procedureIRValue = inst.procedureCall.procIRValue;

			u32 procTypeIdx = inst.procedureCall.procIRValue.typeTableIdx;
			callingConvention = GetTypeInfo(context, procTypeIdx).procedureInfo.callingConvention;
		}

		// At worst each parameter should add 2 values to this array, this is why we multiply
		// capacity by 2.
		// @Improve: dynamic array.
		ArrayInit(&result.parameterValues, inst.procedureCall.parameters.size * 2);

		Array<u32, PhaseAllocator> paramValues;
		switch (callingConvention)
		{
			case CC_WIN64:
				paramValues =
					X64ReadyWin64Parameters(context, x64Proc, inst.procedureCall.parameters, true);
				break;
			case CC_DEFAULT:
			case CC_LINUX64:
			default:
				paramValues =
					X64ReadyLinuxParameters(context, x64Proc, inst.procedureCall.parameters, true);
		}

		result.parameterValues.data = paramValues.data;
		result.parameterValues.size = paramValues.size;

#if !_MSC_VER
		// Check syscalls
		if (inst.type == IRINSTRUCTIONTYPE_PROCEDURE_CALL)
		{
			String procName = GetProcedureRead(context, inst.procedureCall.procedureIdx).name;
			int syscallCount = ArrayCount(x64LinuxSyscallNames);
			for (int i = 0; i < syscallCount; ++i)
			{
				if (StringEquals(procName, x64LinuxSyscallNames[i]))
				{
					result.type = X64_SYSCALL;
					X64Mov(context, x64Proc, RAX, IRValueImmediate(i));
					break;
				}
			}
		}
#endif

		*BucketArrayAdd(&x64Proc->instructions) = result;

		if (inst.procedureCall.out.valueType != IRVALUETYPE_INVALID)
		{
			u32 returnTypeIdx = inst.procedureCall.out.typeTableIdx;
			if (GetTypeInfo(context, returnTypeIdx).typeCategory == TYPECATEGORY_FLOATING)
			{
				IRValue typedXmm0 = XMM0;
				typedXmm0.typeTableIdx = returnTypeIdx;
				X64Mov(context, x64Proc, inst.procedureCall.out, typedXmm0);
			}
			else
				X64Mov(context, x64Proc, inst.procedureCall.out, RAX);
		}
		return;
	}
	case IRINSTRUCTIONTYPE_INTRINSIC:
	{
		switch (inst.intrinsic.type)
		{
		case INTRINSIC_BREAKPOINT:
			*BucketArrayAdd(&x64Proc->instructions) = { X64_INT, IRValueImmediate(3) };
			return;
		case INTRINSIC_SQRT32:
			result.type = X64_SQRTSS;
			goto doTwoArgIntrinsic;
		case INTRINSIC_SQRT64:
			result.type = X64_SQRTSD;
			goto doTwoArgIntrinsic;
		default:
			ASSERT(!"Invalid intrinsic");
		}
		return;
	}
	case IRINSTRUCTIONTYPE_COPY_MEMORY:
	{
		X64CopyMemory(context, x64Proc, inst.copyMemory.dst, inst.copyMemory.src,
				inst.copyMemory.size);
		return;
	}
	case IRINSTRUCTIONTYPE_ZERO_MEMORY:
	{
		ASSERT(inst.zeroMemory.dst.valueType  == IRVALUETYPE_VALUE ||
			   inst.zeroMemory.dst.valueType  == IRVALUETYPE_VALUE_DEREFERENCE);
		u32 dstIdx = inst.zeroMemory.dst.value.valueIdx;

		// First attempt to zero manually
		if (inst.zeroMemory.size.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		{
			TypeInfo dstTypeInfo = GetTypeInfo(context, GetValueRead(context, dstIdx).typeTableIdx);
			s64 size = inst.zeroMemory.size.immediate;

			s64 copiedBytes = 0;
			if (size - copiedBytes >= 16)
			{
				IRValue zeroXmmReg = IRValueNewValue(context, "_zeroxmm"_s, TYPETABLEIDX_128,
						VALUEFLAGS_FORCE_REGISTER);
				*BucketArrayAdd(&x64Proc->instructions) = { X64_XORPS, zeroXmmReg, zeroXmmReg };
				while (size - copiedBytes >= 16)
				{
					X64Mov(context, x64Proc,
							IRValueDereference(dstIdx, TYPETABLEIDX_128, copiedBytes), zeroXmmReg);
					copiedBytes += 16;
				}
			}
			if (size - copiedBytes >= 1)
			{
				IRValue zeroReg = IRValueNewValue(context, "_zeroreg"_s, TYPETABLEIDX_S64,
						VALUEFLAGS_FORCE_REGISTER);
				*BucketArrayAdd(&x64Proc->instructions) = { X64_XOR, zeroReg, zeroReg };
				while (size - copiedBytes >= 8)
				{
					X64Mov(context, x64Proc,
							IRValueDereference(dstIdx, TYPETABLEIDX_S64, copiedBytes), zeroReg);
					copiedBytes += 8;
				}
				while (size - copiedBytes >= 4)
				{
					X64Mov(context, x64Proc,
							IRValueDereference(dstIdx, TYPETABLEIDX_S32, copiedBytes), zeroReg);
					copiedBytes += 4;
				}
				while (size - copiedBytes >= 1)
				{
					X64Mov(context, x64Proc,
							IRValueDereference(dstIdx, TYPETABLEIDX_S8, copiedBytes), zeroReg);
					++copiedBytes;
				}
			}
			return;
		}

		X64Mov(context, x64Proc, RCX, inst.zeroMemory.dst);
		X64Mov(context, x64Proc, RDX,  inst.zeroMemory.size);
		result.type = X64_CALL;
		result.procedureIdx = zeroMemoryProcIdx;
		ArrayInit(&result.parameterValues, 2);
		*ArrayAdd(&result.parameterValues) = RCX.value.valueIdx;
		*ArrayAdd(&result.parameterValues) = RDX.value.valueIdx;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	}
	case IRINSTRUCTIONTYPE_PUSH_SCOPE:
		result.type = X64_Push_Scope;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	case IRINSTRUCTIONTYPE_POP_SCOPE:
		result.type = X64_Pop_Scope;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	case IRINSTRUCTIONTYPE_PUSH_VALUE:
		result.type = X64_Push_Value;
		result.valueIdx = inst.pushValue.valueIdx;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	case IRINSTRUCTIONTYPE_COMMENT:
		result.type = X64_Comment;
		result.comment = inst.comment;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	case IRINSTRUCTIONTYPE_RETURN:
		return;
	default:
		ASSERT(!"Unrecognized IR instruction type");
		return;
	}

doRM:
	{
		IRValue operand = inst.unaryOperation.out;

		if (operand.valueType == IRVALUETYPE_IMMEDIATE_INTEGER &&
			(operand.immediate & 0xFFFFFFFF00000000))
		{
			IRValue tmp = IRValueNewValue(context, operand.typeTableIdx, 0);
			X64Mov(context, x64Proc, tmp, operand);
			operand = tmp;
		}

		X64Mov(context, x64Proc, operand, inst.unaryOperation.in);

		result.dst = operand;
		*BucketArrayAdd(&x64Proc->instructions) = result;

		return;
	}
doRM_RMI:
	{
		IRValue left  = inst.binaryOperation.left;
		IRValue right = inst.binaryOperation.right;
		IRValue out   = inst.binaryOperation.out;

		if (right.valueType == IRVALUETYPE_IMMEDIATE_INTEGER &&
			(right.immediate & 0xFFFFFFFF00000000))
		{
			IRValue tmp = IRValueNewValue(context, "_biglittmp"_s, right.typeTableIdx, 0);
			X64Mov(context, x64Proc, tmp, right);
			right = tmp;
		}

		u32 immitateFlag = left.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
		IRValue tmp = IRValueNewValue(context, "_rmrmitmp"_s, left.typeTableIdx,
				VALUEFLAGS_FORCE_REGISTER | immitateFlag, left.value.valueIdx);

		X64MovNoTmp(context, x64Proc, tmp, left);

		result.dst = tmp;
		result.src = right;
		*BucketArrayAdd(&x64Proc->instructions) = result;

		X64Mov(context, x64Proc, out, tmp);

		return;
	}
doX_XM:
	{
		IRValue left  = inst.binaryOperation.left;
		IRValue right = inst.binaryOperation.right;
		IRValue out = inst.binaryOperation.out;

		u32 immitateFlagLeft = out.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
		IRValue tmp = IRValueNewValue(context, left.typeTableIdx,
				VALUEFLAGS_FORCE_REGISTER | immitateFlagLeft, out.value.valueIdx);

		X64MovNoTmp(context, x64Proc, tmp, left);

		u8 accepted = x64InstructionInfos[result.type].operandTypesRight;
		if (!FitsInOperand(context, accepted, right) ||
			right.typeTableIdx != left.typeTableIdx)
		{
			ASSERT(accepted & OPERANDTYPE_REGISTER);
			u32 immitateFlagRight = right.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
			IRValue newValue = IRValueNewValue(context, out.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER | immitateFlagRight, right.value.valueIdx);
			X64Mov(context, x64Proc, newValue, right);
			right = newValue;
		}

		result.dst = tmp;
		result.src = right;
		*BucketArrayAdd(&x64Proc->instructions) = result;

		X64Mov(context, x64Proc, out, tmp);

		return;
	}
doShift:
	{
		IRValue left  = inst.binaryOperation.left;
		IRValue right = inst.binaryOperation.right;
		IRValue out   = inst.binaryOperation.out;

		IRValue tmp = IRValueNewValue(context, left.typeTableIdx, 0);

		X64Mov(context, x64Proc, tmp, left);

		if (right.valueType != IRVALUETYPE_IMMEDIATE_INTEGER)
		{
			X64Mov(context, x64Proc, RCX, right);
			right = CL;
		}

		result.dst = tmp;
		result.src = right;
		*BucketArrayAdd(&x64Proc->instructions) = result;

		X64Mov(context, x64Proc, out, tmp);

		return;
	}
doConditionalJump:
	{
		X64Test(context, x64Proc, inst.conditionalJump.condition);

		result.label = inst.conditionalJump.label;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	}
doConditionalJump2:
	{
		X64Instruction cmpInst;

		switch (floatingType)
		{
		case X64FLOATINGTYPE_NONE:
			cmpInst.type = X64_CMP;
			break;
		case X64FLOATINGTYPE_F32:
			cmpInst.type = X64_COMISS;
			break;
		case X64FLOATINGTYPE_F64:
			cmpInst.type = X64_COMISD;
			break;
		default:
			ASSERT(false);
		}

		IRValue left  = inst.conditionalJump2.left;
		u8 accepted = x64InstructionInfos[cmpInst.type].operandTypesLeft;
		if (!FitsInOperand(context, accepted, left))
		{
			ASSERT(accepted & OPERANDTYPE_REGISTER);
			u32 immitateFlagLeft = left.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
			IRValue newValue = IRValueNewValue(context, "_jump_hlp"_s, left.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER | immitateFlagLeft, left.value.valueIdx);
			X64Mov(context, x64Proc, newValue, left);
			left = newValue;
		}

		IRValue right = inst.conditionalJump2.right;
		accepted = x64InstructionInfos[cmpInst.type].operandTypesRight;
		if (!FitsInOperand(context, accepted, right) ||
			right.typeTableIdx != left.typeTableIdx ||
			(IsValueInMemory(context, left) && IsValueInMemory(context, right)) ||
			(right.valueType == IRVALUETYPE_IMMEDIATE_INTEGER && (right.immediate & 0xFFFFFFFF00000000)))
		{
			ASSERT(accepted & OPERANDTYPE_REGISTER);
			u32 immitateFlagRight = right.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
			IRValue newValue = IRValueNewValue(context, "_jump_hlp"_s, left.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER | immitateFlagRight, right.value.valueIdx);
			X64Mov(context, x64Proc, newValue, right);
			right = newValue;
		}
		cmpInst.dst = left;
		cmpInst.src = right;

		result.label = inst.conditionalJump2.label;

		*BucketArrayAdd(&x64Proc->instructions) = cmpInst;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	}
doConditionalSet:
	{
		X64Instruction cmpInst;
		switch (floatingType)
		{
		case X64FLOATINGTYPE_NONE:
			cmpInst.type = X64_CMP;
			break;
		case X64FLOATINGTYPE_F32:
			cmpInst.type = X64_COMISS;
			break;
		case X64FLOATINGTYPE_F64:
			cmpInst.type = X64_COMISD;
			break;
		default:
			ASSERT(false);
		}

		cmpInst.dst = inst.binaryOperation.left;
		cmpInst.src = inst.binaryOperation.right;

		if (cmpInst.src.valueType == IRVALUETYPE_IMMEDIATE_INTEGER &&
			(cmpInst.src.immediate & 0xFFFFFFFF00000000))
		{
			IRValue tmp = IRValueNewValue(context, cmpInst.src.typeTableIdx, 0);
			X64Mov(context, x64Proc, tmp, cmpInst.src);
			cmpInst.src = tmp;
		}

		u8 accepted = x64InstructionInfos[cmpInst.type].operandTypesLeft;
		if (!FitsInOperand(context, accepted, cmpInst.dst) ||
			(IsValueInMemory(context, cmpInst.dst) && IsValueInMemory(context, cmpInst.src)))
		{
			ASSERT(accepted & OPERANDTYPE_REGISTER);
			IRValue newValue = IRValueNewValue(context, "_setcc_hlp"_s, cmpInst.dst.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER);
			X64Mov(context, x64Proc, newValue, cmpInst.dst);
			cmpInst.dst = newValue;
		}

		*BucketArrayAdd(&x64Proc->instructions) = cmpInst;

		result.dst = inst.binaryOperation.out;
		if (GetTypeInfo(context, result.dst.typeTableIdx).size != 1)
		{
			X64Mov(context, x64Proc, result.dst, IRValueImmediate(0));
			result.dst.typeTableIdx = TYPETABLEIDX_S8;
		}
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	}
doTwoArgIntrinsic:
	{
		IRValue left  = inst.intrinsic.parameters[0];
		IRValue right = inst.intrinsic.parameters[1];
		IRValue out   = inst.intrinsic.parameters[0];

		IRValue tmp = IRValueNewValue(context, left.typeTableIdx, 0);

		result.dst = tmp;
		result.src = right;
		*BucketArrayAdd(&x64Proc->instructions) = result;

		X64Mov(context, x64Proc, out, tmp);
		return;
	}
}

String X64InstructionToStr(Context *context, X64Instruction inst)
{
	String mnemonic = x64InstructionInfos[inst.type].mnemonic;
	switch (inst.type)
	{
	case X64_CALL:
		return TPrintF("call %S", GetProcedureRead(context, inst.procedureIdx).name);
	case X64_CALL_Indirect:
	{
		String proc = X64IRValueToStr(context, inst.procedureIRValue);
		return TPrintF("call %S", proc);
	}
	case X64_JMP:
	case X64_JE:
	case X64_JNE:
	case X64_JG:
	case X64_JL:
	case X64_JGE:
	case X64_JLE:
	case X64_JA:
	case X64_JB:
	case X64_JAE:
	case X64_JBE:
		goto printLabel;
	case X64_Label:
		return TPrintF("%S:", inst.label->name);
	case X64_Comment:
		return TPrintF("; %S", inst.comment);
	case X64_Ignore:
	case X64_Push_Scope:
	case X64_Pop_Scope:
	case X64_Push_Value:
		return {};
	default:
	{
		X64InstructionInfo instInfo = x64InstructionInfos[inst.type];
		if (instInfo.operandTypesLeft != OPERANDTYPE_NONE)
		{
			if (instInfo.operandTypesRight != OPERANDTYPE_NONE)
				goto printDstSrc;
			else
				goto printDst;
		}
		else
			return mnemonic;
	}
	}

printDst:
	{
		String dst = X64IRValueToStr(context, inst.dst);
		return TPrintF("%S %S", mnemonic, dst);
	}
printDstSrc:
	{
		String dst = X64IRValueToStr(context, inst.dst);
		String src = X64IRValueToStr(context, inst.src);
		return TPrintF("%S %S, %S", mnemonic, dst, src);
	}
printLabel:
	{
		return TPrintF("%S %S", mnemonic, inst.label->name);
	}
}

inline s64 X64PrintInstruction(Context *context, X64Instruction inst)
{
	return PrintOut(context, "%S", X64InstructionToStr(context, inst));
}

#include "X64RegisterAllocation.cpp"

void X64PrintInstructions(Context *context, Array<X64Procedure, PhaseAllocator> x64Procedures)
{
	for (int procedureIdx = 1; procedureIdx < x64Procedures.size; ++procedureIdx)
	{
		X64Procedure x64Proc = x64Procedures[procedureIdx];

#if _MSC_VER
		if (GetProcedureRead(context, procedureIdx).isExported)
			PrintOut(context, "\n%S PROC PUBLIC\n", x64Proc.name);
		else
			PrintOut(context, "\n%S PROC PRIVATE\n", x64Proc.name);
#else
		PrintOut(context, "\n%S:\n", x64Proc.name);
#endif
		PrintOut(context, "push rbp\n");
		PrintOut(context, "mov rbp, rsp\n");
		if (x64Proc.stackSize > 0)
			PrintOut(context, "sub rsp, 0%llxh\n", x64Proc.stackSize);

		X64InstructionStream stream = X64InstructionStreamBegin(&x64Proc);
		X64Instruction *inst = X64InstructionStreamAdvance(&stream);
		while (inst)
		{
			if (X64PrintInstruction(context, *inst))
				PrintOut(context, "\n");
			inst = X64InstructionStreamAdvance(&stream);
		}

		PrintOut(context, "leave\n");
		PrintOut(context, "ret\n");
#if _MSC_VER
		PrintOut(context, "%S ENDP\n", x64Proc.name);
#endif
	}
}

inline void X64StaticDataAlignTo(Context *context, s64 alignment, bool initialize)
{
#if _MSC_VER
	PrintOut(context, "ALIGN %d\n", alignment);
#else
	if (initialize)
		PrintOut(context, "ALIGN %d\n", alignment);
	else
		PrintOut(context, "ALIGNB %d\n", alignment);
#endif
}

void X64PrintStaticData(Context *context, String name, IRValue value, u32 typeTableIdx,
		int alignmentOverride = -1)
{
	switch (value.valueType)
	{
	case IRVALUETYPE_IMMEDIATE_STRING:
	{
		int alignment = alignmentOverride < 0 ? 8 : alignmentOverride;
		X64StaticDataAlignTo(context, alignment, true);

		String str = context->stringLiterals[value.immediateStringIdx];
		s64 size = str.size;
		if (size == 0)
			PrintOut(context, "%S DQ 0H, 0H\n", name);
		else
		{
			for (int i = 0; i < str.size; ++i)
				if (str.data[i] == '\\') --size;
			PrintOut(context, "%S DQ %.16llxH, _str_%d\n", name, size, value.immediateStringIdx);
		}
	} break;
	case IRVALUETYPE_IMMEDIATE_FLOAT:
	{
		TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
		int alignment = alignmentOverride < 0 ? (int)typeInfo.size : alignmentOverride;
		X64StaticDataAlignTo(context, alignment, true);
		switch (typeInfo.size)
		{
		case 4:
		{
			union { u32 asU32; f32 asF32; };
			asF32 = (f32)value.immediateFloat;
			PrintOut(context, "%S DD 0%.8xH\n", name, asU32);
		} break;
		case 8:
		default:
			PrintOut(context, "%S DQ 0%.16llxH\n", name,
					value.immediate);
			break;
		}
	} break;
	case IRVALUETYPE_IMMEDIATE_GROUP:
	{
		int alignment = alignmentOverride < 0 ? 8 : alignmentOverride;
		X64StaticDataAlignTo(context, alignment, true);

		bool isArray = false;
		u32 elementTypeIdx = TYPETABLEIDX_UNSET;
		if (typeTableIdx > 0)
		{
			TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
			isArray = typeInfo.typeCategory == TYPECATEGORY_ARRAY;
			elementTypeIdx = typeInfo.arrayInfo.elementTypeTableIdx;
		}

		Array<IRValue, FrameAllocator> members = value.immediateStructMembers;
		for (int memberIdx = 0; memberIdx < members.size; ++memberIdx)
		{
			String memberName = memberIdx ? "   "_s : name;
			u32 memberTypeIdx = isArray ? elementTypeIdx : members[memberIdx].typeTableIdx;
			X64PrintStaticData(context, memberName, members[memberIdx], memberTypeIdx);
		}
	} break;
	case IRVALUETYPE_VALUE_DEREFERENCE:
	{
		// @Improve: We are kinda using this to mean 'this is a value in data section, just put the
		// name in' which has nothing to do with 'VALUE_DEREFERENCE'...
		int alignment = alignmentOverride < 0 ? 8 : alignmentOverride;
		X64StaticDataAlignTo(context, alignment, true);

		Value v = GetValueRead(context, value.value.valueIdx);
		ASSERT(v.flags & VALUEFLAGS_ON_STATIC_STORAGE);
		ASSERT(v.name.size);
		PrintOut(context, "%S DQ g_%S\n", name, v.name);
	} break;
	case IRVALUETYPE_INVALID:
	{
		TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
#if _MSC_VER
		PrintOut(context, "COMM %S:BYTE:0%llxH\n", name, typeInfo.size);
#else
		PrintOut(context, "%S: RESB %llxH\n", name, typeInfo.size);
#endif
	} break;
	default:
	{
		TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
		int alignment = alignmentOverride < 0 ? (int)typeInfo.size : alignmentOverride;
		X64StaticDataAlignTo(context, alignment, true);
		switch (typeInfo.size)
		{
		case 1:
			PrintOut(context, "%S DB %.2llxH\n", name,
					value.immediate);
			break;
		case 2:
			PrintOut(context, "%S DW %.4llxH\n", name,
					value.immediate);
			break;
		case 4:
			PrintOut(context, "%S DD %.8llxH\n", name,
					value.immediate);
			break;
		case 8:
			PrintOut(context, "%S DQ %.16llxH\n", name,
					value.immediate);
			break;
		default:
			ASSERT(!"Invalid immediate size");
		}
	}
	}
}

void X64PrintStaticDataUninitialized(Context *context, String name, IRValue value, u32 typeTableIdx,
		int alignmentOverride = -1)
{
	switch (value.valueType)
	{
	case IRVALUETYPE_IMMEDIATE_STRING:
	{
		int alignment = alignmentOverride < 0 ? 8 : alignmentOverride;
		X64StaticDataAlignTo(context, alignment, false);

		String str = context->stringLiterals[value.immediateStringIdx];
		ASSERT(str.size == 0);
#if _MSC_VER
		PrintOut(context, "COMM %S:QWORD:02H\n", name);
#else
		PrintOut(context, "%S: RESQ 02H\n", name);
#endif
	} break;
	case IRVALUETYPE_IMMEDIATE_FLOAT:
	{
		ASSERT(value.immediateFloat == 0);
		TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
		int alignment = alignmentOverride < 0 ? (int)typeInfo.size : alignmentOverride;
		X64StaticDataAlignTo(context, alignment, false);
#if _MSC_VER
		PrintOut(context, "COMM %S:BYTE:0%llxH\n", name, typeInfo.size);
#else
		PrintOut(context, "%S: RESB 0%llxH\n", name, typeInfo.size);
#endif
	} break;
	case IRVALUETYPE_IMMEDIATE_GROUP:
	{
		int alignment = alignmentOverride < 0 ? 8 : alignmentOverride;
		X64StaticDataAlignTo(context, alignment, false);

		bool isArray = false;
		u32 elementTypeIdx = TYPETABLEIDX_UNSET;
		if (typeTableIdx > 0)
		{
			TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
			isArray = typeInfo.typeCategory == TYPECATEGORY_ARRAY;
			elementTypeIdx = typeInfo.arrayInfo.elementTypeTableIdx;
		}

		Array<IRValue, FrameAllocator> members = value.immediateStructMembers;
		for (int memberIdx = 0; memberIdx < members.size; ++memberIdx)
		{
			String memberName = memberIdx ? "   "_s : name;
			u32 memberTypeIdx = isArray ? elementTypeIdx : members[memberIdx].typeTableIdx;
			X64PrintStaticDataUninitialized(context, memberName, members[memberIdx], memberTypeIdx);
		}
	} break;
	case IRVALUETYPE_VALUE_DEREFERENCE:
	{
		ASSERT(!"Shouldn't be calling this with a dereference value");
	} break;
	case IRVALUETYPE_INVALID:
	default:
	{
		ASSERT(value.immediate == 0);
		TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);
#if _MSC_VER
		PrintOut(context, "COMM %S:BYTE:0%llxH\n", name, typeInfo.size);
#else
		PrintOut(context, "%S: RESB 0%llxH\n", name, typeInfo.size);
#endif
	} break;
	}
}

void WriteOutOutputBuffer(Context *context, String filename)
{
	FileHandle outputFile = SYSOpenFileWrite(filename);
	for (int i = 0; i < context->outputBuffer.buckets.size; ++i)
	{
		SYSWriteFile(outputFile,
				context->outputBuffer.buckets[i].data,
				context->outputBuffer.buckets[i].size);
	}
	SYSCloseFile(outputFile);
}

void BackendMain(Context *context)
{
	BucketArrayInit(&context->bePatchedInstructions);

	// Hard coded CopyMemory and ZeroMemory external procedures
	{
		u32 voidPtrIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_VOID);

		TypeInfo t = { TYPECATEGORY_PROCEDURE };
		t.procedureInfo.returnTypeTableIdx = TYPETABLEIDX_UNSET;
		ArrayInit(&t.procedureInfo.parameters, 3);
		t.procedureInfo.parameters.size = 3;
		t.procedureInfo.parameters[0] = { voidPtrIdx, {} };
		t.procedureInfo.parameters[1] = { voidPtrIdx, {} };
		t.procedureInfo.parameters[2] = { TYPETABLEIDX_S64, {} };
		u32 typeTableIdx = FindOrAddTypeTableIdx(context, t);

		auto externalProcedures = context->externalProcedures.GetForWrite();
		copyMemoryProcIdx = -(s32)BucketArrayCount(&externalProcedures);
		Procedure *copyMemory = BucketArrayAdd(&externalProcedures);
		*copyMemory = {};
		copyMemory->name = "CopyMemory"_s;
		copyMemory->typeTableIdx = typeTableIdx;
		copyMemory->returnValueIdx = U32_MAX;

		zeroMemoryProcIdx = -(s32)BucketArrayCount(&externalProcedures);
		Procedure *zeroMemory = BucketArrayAdd(&externalProcedures);
		*zeroMemory = {};
		zeroMemory->name = "ZeroMemory"_s;
		zeroMemory->typeTableIdx = typeTableIdx;
		zeroMemory->returnValueIdx = U32_MAX;
	}

	x64InstructionInfos[X64_INT] =       { "int"_s,       OPERANDTYPE_IMMEDIATE, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MOV] =       { "mov"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE, OPERANDTYPE_ALL,    OPERANDACCESS_READ };
	x64InstructionInfos[X64_MOVZX] =     { "movzx"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MOVSX] =     { "movsx"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MOVSXD] =    { "movsxd"_s,    OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CQO] =       { "cqo"_s };
	x64InstructionInfos[X64_PUSH] =      { "push"_s,      OPERANDTYPE_ALL,       OPERANDACCESS_READ };
	x64InstructionInfos[X64_POP] =       { "pop"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_JMP] =       { "jmp"_s };
	x64InstructionInfos[X64_JE] =        { "je"_s };
	x64InstructionInfos[X64_JNE] =       { "jne"_s };
	x64InstructionInfos[X64_JG] =        { "jg"_s };
	x64InstructionInfos[X64_JL] =        { "jl"_s };
	x64InstructionInfos[X64_JGE] =       { "jge"_s };
	x64InstructionInfos[X64_JLE] =       { "jle"_s };
	x64InstructionInfos[X64_JA] =        { "ja"_s };
	x64InstructionInfos[X64_JB] =        { "jb"_s };
	x64InstructionInfos[X64_JAE] =       { "jae"_s };
	x64InstructionInfos[X64_JBE] =       { "jbe"_s };
	x64InstructionInfos[X64_CALL] =      { "call"_s };
	x64InstructionInfos[X64_CALL_Indirect] = { "call"_s,  OPERANDTYPE_REGMEM,    OPERANDACCESS_READ };
	x64InstructionInfos[X64_SYSCALL] =   { "syscall"_s };
	x64InstructionInfos[X64_LEAVE] =     { "leave"_s };
	x64InstructionInfos[X64_RET] =       { "ret"_s };
	x64InstructionInfos[X64_LEA] =       { "lea"_s,       OPERANDTYPE_REGISTER,  OPERANDACCESS_WRITE,     OPERANDTYPE_MEMORY, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CMP] =       { "cmp"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READ,      OPERANDTYPE_ALL,    OPERANDACCESS_READ };
	x64InstructionInfos[X64_TEST] =      { "test"_s,      OPERANDTYPE_REGMEM,    OPERANDACCESS_READ,      OPERANDTYPE_ALL,    OPERANDACCESS_READ };
	x64InstructionInfos[X64_SETG] =      { "setg"_s,      OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETL] =      { "setl"_s,      OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETGE] =     { "setge"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETLE] =     { "setle"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETA] =      { "seta"_s,      OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETB] =      { "setb"_s,      OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETAE] =     { "setae"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETBE] =     { "setbe"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETE] =      { "sete"_s,      OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETNE] =     { "setne"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_ADD] =       { "add"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SUB] =       { "sub"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MUL] =       { "mul"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READ };
	x64InstructionInfos[X64_IMUL] =      { "imul"_s,      OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_DIV] =       { "div"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READ };
	x64InstructionInfos[X64_IDIV] =      { "idiv"_s,      OPERANDTYPE_REGMEM,    OPERANDACCESS_READ };
	x64InstructionInfos[X64_SAR] =       { "sar"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SAL] =       { "sal"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SHR] =       { "shr"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SHL] =       { "shl"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_AND] =       { "and"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_OR] =        { "or"_s,        OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_XOR] =       { "xor"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_NOT] =       { "not"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE };
	x64InstructionInfos[X64_NEG] =       { "neg"_s,       OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE };
	x64InstructionInfos[X64_MOVSS] =     { "movss"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MOVSD] =     { "movsd"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_ADDSS] =     { "addss"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_ADDSD] =     { "addsd"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SUBSS] =     { "subss"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SUBSD] =     { "subsd"_s,     OPERANDTYPE_REGMEM,    OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MULSS] =     { "mulss"_s,     OPERANDTYPE_REGISTER,  OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MULSD] =     { "mulsd"_s,     OPERANDTYPE_REGISTER,  OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_DIVSS] =     { "divss"_s,     OPERANDTYPE_REGISTER,  OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_DIVSD] =     { "divsd"_s,     OPERANDTYPE_REGISTER,  OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_XORPS] =     { "xorps"_s,     OPERANDTYPE_REGISTER,  OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_XORPD] =     { "xorpd"_s,     OPERANDTYPE_REGISTER,  OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SQRTSS] =    { "sqrtss"_s,    OPERANDTYPE_REGISTER,  OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SQRTSD] =    { "sqrtsd"_s,    OPERANDTYPE_REGISTER,  OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_COMISS] =    { "comiss"_s,    OPERANDTYPE_REGISTER,  OPERANDACCESS_READ,      OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_COMISD] =    { "comisd"_s,    OPERANDTYPE_REGISTER,  OPERANDACCESS_READ,      OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CVTSI2SS] =  { "cvtsi2ss"_s,  OPERANDTYPE_REGISTER,  OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CVTSI2SD] =  { "cvtsi2sd"_s,  OPERANDTYPE_REGISTER,  OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CVTTSS2SI] = { "cvttss2si"_s, OPERANDTYPE_REGISTER,  OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CVTTSD2SI] = { "cvttsd2si"_s, OPERANDTYPE_REGISTER,  OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CVTSS2SD] =  { "cvtss2sd"_s,  OPERANDTYPE_REGISTER,  OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CVTSD2SS] =  { "cvtsd2ss"_s,  OPERANDTYPE_REGISTER,  OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MOVUPS] =    { "movups"_s,    OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MOVAPS] =    { "movaps"_s,    OPERANDTYPE_REGMEM,    OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };

	const u8 regValueFlags = VALUEFLAGS_IS_USED | VALUEFLAGS_IS_ALLOCATED;
	u32 RAX_valueIdx = NewValue(context, "RAX"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 RCX_valueIdx = NewValue(context, "RCX"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 RDX_valueIdx = NewValue(context, "RDX"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 RBX_valueIdx = NewValue(context, "RBX"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 RSI_valueIdx = NewValue(context, "RSI"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 RDI_valueIdx = NewValue(context, "RDI"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 RSP_valueIdx = NewValue(context, "RSP"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 RBP_valueIdx = NewValue(context, "RBP"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 R8_valueIdx  = NewValue(context, "R8"_s,  TYPETABLEIDX_S64, regValueFlags);
	u32 R9_valueIdx  = NewValue(context, "R9"_s,  TYPETABLEIDX_S64, regValueFlags);
	u32 R10_valueIdx = NewValue(context, "R10"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 R11_valueIdx = NewValue(context, "R11"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 R12_valueIdx = NewValue(context, "R12"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 R13_valueIdx = NewValue(context, "R13"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 R14_valueIdx = NewValue(context, "R14"_s, TYPETABLEIDX_S64, regValueFlags);
	u32 R15_valueIdx = NewValue(context, "R15"_s, TYPETABLEIDX_S64, regValueFlags);

	u32 XMM0_valueIdx =  NewValue(context, "XMM0"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM1_valueIdx =  NewValue(context, "XMM1"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM2_valueIdx =  NewValue(context, "XMM2"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM3_valueIdx =  NewValue(context, "XMM3"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM4_valueIdx =  NewValue(context, "XMM4"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM5_valueIdx =  NewValue(context, "XMM5"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM6_valueIdx =  NewValue(context, "XMM6"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM7_valueIdx =  NewValue(context, "XMM7"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM8_valueIdx =  NewValue(context, "XMM8"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM9_valueIdx =  NewValue(context, "XMM9"_s,  TYPETABLEIDX_F64, regValueFlags);
	u32 XMM10_valueIdx = NewValue(context, "XMM10"_s, TYPETABLEIDX_F64, regValueFlags);
	u32 XMM11_valueIdx = NewValue(context, "XMM11"_s, TYPETABLEIDX_F64, regValueFlags);
	u32 XMM12_valueIdx = NewValue(context, "XMM12"_s, TYPETABLEIDX_F64, regValueFlags);
	u32 XMM13_valueIdx = NewValue(context, "XMM13"_s, TYPETABLEIDX_F64, regValueFlags);
	u32 XMM14_valueIdx = NewValue(context, "XMM14"_s, TYPETABLEIDX_F64, regValueFlags);
	u32 XMM15_valueIdx = NewValue(context, "XMM15"_s, TYPETABLEIDX_F64, regValueFlags);

	for (int i = 0; i < X64REGISTER_Count; ++i)
		GetValueWrite(context, RAX_valueIdx + i)->allocatedRegister = i;

	RAX  = IRValueValue(RAX_valueIdx, TYPETABLEIDX_S64);
	RCX  = IRValueValue(RCX_valueIdx, TYPETABLEIDX_S64);
	RDX  = IRValueValue(RDX_valueIdx, TYPETABLEIDX_S64);
	RBX  = IRValueValue(RBX_valueIdx, TYPETABLEIDX_S64);
	RSI  = IRValueValue(RSI_valueIdx, TYPETABLEIDX_S64);
	RDI  = IRValueValue(RDI_valueIdx, TYPETABLEIDX_S64);
	RSP  = IRValueValue(RSP_valueIdx, TYPETABLEIDX_S64);
	RBP  = IRValueValue(RBP_valueIdx, TYPETABLEIDX_S64);
	R8   = IRValueValue(R8_valueIdx,  TYPETABLEIDX_S64);
	R9   = IRValueValue(R9_valueIdx,  TYPETABLEIDX_S64);
	R10  = IRValueValue(R10_valueIdx, TYPETABLEIDX_S64);
	R11  = IRValueValue(R11_valueIdx, TYPETABLEIDX_S64);
	R12  = IRValueValue(R12_valueIdx, TYPETABLEIDX_S64);
	R13  = IRValueValue(R13_valueIdx, TYPETABLEIDX_S64);
	R14  = IRValueValue(R14_valueIdx, TYPETABLEIDX_S64);
	R15  = IRValueValue(R15_valueIdx, TYPETABLEIDX_S64);

	EAX  = IRValueValue(RAX_valueIdx, TYPETABLEIDX_S32);
	ECX  = IRValueValue(RCX_valueIdx, TYPETABLEIDX_S32);
	EDX  = IRValueValue(RDX_valueIdx, TYPETABLEIDX_S32);
	EBX  = IRValueValue(RBX_valueIdx, TYPETABLEIDX_S32);
	ESI  = IRValueValue(RSI_valueIdx, TYPETABLEIDX_S32);
	EDI  = IRValueValue(RDI_valueIdx, TYPETABLEIDX_S32);
	ESP  = IRValueValue(RSP_valueIdx, TYPETABLEIDX_S32);
	EBP  = IRValueValue(RBP_valueIdx, TYPETABLEIDX_S32);
	R8D  = IRValueValue(R8_valueIdx,  TYPETABLEIDX_S32);
	R9D  = IRValueValue(R9_valueIdx,  TYPETABLEIDX_S32);
	R10D = IRValueValue(R10_valueIdx, TYPETABLEIDX_S32);
	R11D = IRValueValue(R11_valueIdx, TYPETABLEIDX_S32);
	R12D = IRValueValue(R12_valueIdx, TYPETABLEIDX_S32);
	R13D = IRValueValue(R13_valueIdx, TYPETABLEIDX_S32);
	R14D = IRValueValue(R14_valueIdx, TYPETABLEIDX_S32);
	R15D = IRValueValue(R15_valueIdx, TYPETABLEIDX_S32);

	AX   = IRValueValue(RAX_valueIdx, TYPETABLEIDX_S16);
	CX   = IRValueValue(RCX_valueIdx, TYPETABLEIDX_S16);
	DX   = IRValueValue(RDX_valueIdx, TYPETABLEIDX_S16);
	BX   = IRValueValue(RBX_valueIdx, TYPETABLEIDX_S16);
	SI   = IRValueValue(RSI_valueIdx, TYPETABLEIDX_S16);
	DI   = IRValueValue(RDI_valueIdx, TYPETABLEIDX_S16);
	SP   = IRValueValue(RSP_valueIdx, TYPETABLEIDX_S16);
	BP   = IRValueValue(RBP_valueIdx, TYPETABLEIDX_S16);
	R8W  = IRValueValue(R8_valueIdx,  TYPETABLEIDX_S16);
	R9W  = IRValueValue(R9_valueIdx,  TYPETABLEIDX_S16);
	R10W = IRValueValue(R10_valueIdx, TYPETABLEIDX_S16);
	R11W = IRValueValue(R11_valueIdx, TYPETABLEIDX_S16);
	R12W = IRValueValue(R12_valueIdx, TYPETABLEIDX_S16);
	R13W = IRValueValue(R13_valueIdx, TYPETABLEIDX_S16);
	R14W = IRValueValue(R14_valueIdx, TYPETABLEIDX_S16);
	R15W = IRValueValue(R15_valueIdx, TYPETABLEIDX_S16);

	AL   = IRValueValue(RAX_valueIdx, TYPETABLEIDX_S8);
	CL   = IRValueValue(RCX_valueIdx, TYPETABLEIDX_S8);
	DL   = IRValueValue(RDX_valueIdx, TYPETABLEIDX_S8);
	BL   = IRValueValue(RBX_valueIdx, TYPETABLEIDX_S8);
	SIL  = IRValueValue(RSI_valueIdx, TYPETABLEIDX_S8);
	DIL  = IRValueValue(RDI_valueIdx, TYPETABLEIDX_S8);
	SPL  = IRValueValue(RSP_valueIdx, TYPETABLEIDX_S8);
	BPL  = IRValueValue(RBP_valueIdx, TYPETABLEIDX_S8);
	R8B  = IRValueValue(R8_valueIdx,  TYPETABLEIDX_S8);
	R9B  = IRValueValue(R9_valueIdx,  TYPETABLEIDX_S8);
	R10B = IRValueValue(R10_valueIdx, TYPETABLEIDX_S8);
	R11B = IRValueValue(R11_valueIdx, TYPETABLEIDX_S8);
	R12B = IRValueValue(R12_valueIdx, TYPETABLEIDX_S8);
	R13B = IRValueValue(R13_valueIdx, TYPETABLEIDX_S8);
	R14B = IRValueValue(R14_valueIdx, TYPETABLEIDX_S8);
	R15B = IRValueValue(R15_valueIdx, TYPETABLEIDX_S8);

	XMM0 =  IRValueValue(XMM0_valueIdx,  TYPETABLEIDX_F64);
	XMM1 =  IRValueValue(XMM1_valueIdx,  TYPETABLEIDX_F64);
	XMM2 =  IRValueValue(XMM2_valueIdx,  TYPETABLEIDX_F64);
	XMM3 =  IRValueValue(XMM3_valueIdx,  TYPETABLEIDX_F64);
	XMM4 =  IRValueValue(XMM4_valueIdx,  TYPETABLEIDX_F64);
	XMM5 =  IRValueValue(XMM5_valueIdx,  TYPETABLEIDX_F64);
	XMM6 =  IRValueValue(XMM6_valueIdx,  TYPETABLEIDX_F64);
	XMM7 =  IRValueValue(XMM7_valueIdx,  TYPETABLEIDX_F64);
	XMM8 =  IRValueValue(XMM8_valueIdx,  TYPETABLEIDX_F64);
	XMM9 =  IRValueValue(XMM9_valueIdx,  TYPETABLEIDX_F64);
	XMM10 = IRValueValue(XMM10_valueIdx, TYPETABLEIDX_F64);
	XMM11 = IRValueValue(XMM11_valueIdx, TYPETABLEIDX_F64);
	XMM12 = IRValueValue(XMM12_valueIdx, TYPETABLEIDX_F64);
	XMM13 = IRValueValue(XMM13_valueIdx, TYPETABLEIDX_F64);
	XMM14 = IRValueValue(XMM14_valueIdx, TYPETABLEIDX_F64);
	XMM15 = IRValueValue(XMM15_valueIdx, TYPETABLEIDX_F64);

	x64Registers[0]  = RAX;
	x64Registers[1]  = RCX;
	x64Registers[2]  = RDX;
	x64Registers[3]  = RBX;
	x64Registers[4]  = RSI;
	x64Registers[5]  = RDI;
	x64Registers[6]  = RSP;
	x64Registers[7]  = RBP;
	x64Registers[8]  = R8;
	x64Registers[9]  = R9;
	x64Registers[10] = R10;
	x64Registers[11] = R11;
	x64Registers[12] = R12;
	x64Registers[13] = R13;
	x64Registers[14] = R14;
	x64Registers[15] = R15;
	x64Registers[16] = XMM0;
	x64Registers[17] = XMM1;
	x64Registers[18] = XMM2;
	x64Registers[19] = XMM3;
	x64Registers[20] = XMM4;
	x64Registers[21] = XMM5;
	x64Registers[22] = XMM6;
	x64Registers[23] = XMM7;
	x64Registers[24] = XMM8;
	x64Registers[25] = XMM9;
	x64Registers[26] = XMM10;
	x64Registers[27] = XMM11;
	x64Registers[28] = XMM12;
	x64Registers[29] = XMM13;
	x64Registers[30] = XMM14;
	x64Registers[31] = XMM15;

	for (int paramIdx = 0; paramIdx < 32; ++paramIdx)
	{
		Value newValue = {};
		newValue.name = TPrintF("_param%d", paramIdx);
		newValue.typeTableIdx = TYPETABLEIDX_S64;
		newValue.flags = VALUEFLAGS_IS_ALLOCATED | VALUEFLAGS_IS_MEMORY | VALUEFLAGS_BASE_RELATIVE;
		newValue.stackOffset = 16 + paramIdx * 8; // Add 16, 8 for return address, and 8 because we push RBP
		u32 newValueIdx = NewValue(context, newValue);
		x64SpilledParametersRead[paramIdx] = newValueIdx;
	}

	for (int paramIdx = 0; paramIdx < 32; ++paramIdx)
	{
		Value newValue = {};
		newValue.name = TPrintF("_param%d", paramIdx);
		newValue.typeTableIdx = TYPETABLEIDX_S64;
		newValue.flags = VALUEFLAGS_IS_ALLOCATED | VALUEFLAGS_IS_MEMORY;
		newValue.stackOffset = paramIdx * 8; // Add 16, 8 for return address, and 8 because we push RBP
		u32 newValueIdx = NewValue(context, newValue);
		x64SpilledParametersWrite[paramIdx] = newValueIdx;
	}

	Array<X64Procedure, PhaseAllocator> x64Procedures;

	u64 procedureCount = BucketArrayCount(&context->procedures.LockForRead());
	context->procedures.UnlockForRead();

	ArrayInit(&x64Procedures, procedureCount);
	x64Procedures.size = procedureCount;

	// Create X64Procedures
	for (int procedureIdx = 1; procedureIdx < procedureCount; ++procedureIdx)
	{
		Procedure proc = GetProcedureRead(context, procedureIdx);
		X64Procedure *x64Proc = &x64Procedures[procedureIdx];
		ASSERT(GetTypeInfo(context, proc.typeTableIdx).typeCategory == TYPECATEGORY_PROCEDURE);
		TypeInfoProcedure procTypeInfo = GetTypeInfo(context, proc.typeTableIdx).procedureInfo;

		x64Proc->name = proc.name;
		x64Proc->allocatedParameterCount = 0;
		x64Proc->returnValueIdx = proc.returnValueIdx;
		x64Proc->stackSize = 0;
		DynamicArrayInit(&x64Proc->spilledValues, 8);

		BucketArrayInit(&x64Proc->instructions);

		bool returnByCopy = procTypeInfo.returnTypeTableIdx != TYPETABLEIDX_VOID &&
				IRShouldPassByCopy(context, procTypeInfo.returnTypeTableIdx);
		int paramCount = (int)proc.parameterValues.size;

		// Allocate parameters
		Array<IRValue, PhaseAllocator> params;
		ArrayInit(&params, paramCount + returnByCopy);

		// Pointer to return value
		if (returnByCopy)
			*ArrayAdd(&params) = IRValueValue(proc.returnValueIdx, TYPETABLEIDX_S64);

		for (int paramIdx = 0; paramIdx < paramCount; ++paramIdx)
			*ArrayAdd(&params) = IRValueValue(context, proc.parameterValues[paramIdx]);

		switch (procTypeInfo.callingConvention)
		{
			case CC_WIN64:
				X64ReadyWin64Parameters(context, x64Proc, params, false);
				break;
			case CC_DEFAULT:
			case CC_LINUX64:
				X64ReadyLinuxParameters(context, x64Proc, params, false);
		}

		auto &irInstructions = context->irProcedureInstructions[procedureIdx];
		u64 instructionCount = BucketArrayCount(&irInstructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			IRInstruction inst = irInstructions[instructionIdx];
			X64ConvertInstruction(context, inst, x64Proc);
		}

		if (procTypeInfo.returnTypeTableIdx != TYPETABLEIDX_VOID)
		{
			u32 returnTypeTableIdx = GetValueRead(context, proc.returnValueIdx).typeTableIdx;
			if (returnByCopy)
			{
				IRValue returnValue = IRValueValue(proc.returnValueIdx, returnTypeTableIdx);
				*BucketArrayAdd(&x64Proc->instructions) = { X64_LEA, RAX, returnValue };
			}
			else
			{
				IRValue returnValue = IRValueValue(proc.returnValueIdx, returnTypeTableIdx);
				if (GetTypeInfo(context, returnTypeTableIdx).typeCategory == TYPECATEGORY_FLOATING)
				{
					IRValue typedXmm0 = XMM0;
					typedXmm0.typeTableIdx = returnTypeTableIdx;
					X64Mov(context, x64Proc, typedXmm0, returnValue);
				}
				else
					X64Mov(context, x64Proc, RAX, returnValue);
			}
		}
	}

#if DEBUG_BUILD
	BucketArrayInit(&context->outputBuffer);
	X64PrintInstructions(context, x64Procedures);
	WriteOutOutputBuffer(context, "output/x64_before_allocation.txt"_s);
#endif

	TimerSplit("X64 generation"_s);

	X64AllocateRegisters(context, x64Procedures);

	// Remove instructions that reference unused values
	for (int procedureIdx = 1; procedureIdx < x64Procedures.size; ++procedureIdx)
	{
		X64InstructionStream stream = X64InstructionStreamBegin(&x64Procedures[procedureIdx]);
		X64Instruction *inst = X64InstructionStreamAdvance(&stream);
		X64Instruction *nextInst  = X64InstructionStreamAdvance(&stream);
		X64Instruction *nextInst2 = X64InstructionStreamAdvance(&stream);
		while (inst)
		{
			// Replace LEAs with a register as a source with a MOV.
			if (inst->type == X64_LEA)
			{
				if (inst->src.value.offset == 0 && inst->src.value.elementSize == 0)
				{
					Value v = GetValueRead(context, inst->src.value.valueIdx);
					if ((v.flags & VALUEFLAGS_IS_ALLOCATED) && !(v.flags & VALUEFLAGS_IS_MEMORY))
					{
						inst->type = X64_MOV;
						inst->src.valueType = IRVALUETYPE_VALUE;
					}
				}
			}

			switch (inst->type)
			{
			// dst write, src read
			case X64_MOVUPS:
			{
				// If aligned change to MOVAPS
				ASSERT((inst->dst.valueType == IRVALUETYPE_VALUE ||
					 inst->dst.valueType == IRVALUETYPE_VALUE_DEREFERENCE) &&
					(inst->src.valueType == IRVALUETYPE_VALUE ||
					 inst->src.valueType == IRVALUETYPE_VALUE_DEREFERENCE));

				Value dst = GetValueRead(context, inst->dst.value.valueIdx);
				Value src = GetValueRead(context, inst->src.value.valueIdx);
				if (dst.flags & VALUEFLAGS_IS_ALLOCATED && src.flags & VALUEFLAGS_IS_ALLOCATED)
				{
					if (!(dst.flags & VALUEFLAGS_IS_MEMORY) ||
						(dst.stackOffset & 15))
						goto unalignedMovups;

					if (!(src.flags & VALUEFLAGS_IS_MEMORY) ||
						(src.stackOffset & 15))
						goto unalignedMovups;

					inst->type = X64_MOVAPS;
				}
unalignedMovups:;
			} // fall through
			case X64_MOV:
			case X64_MOVSS:
			case X64_MOVSD:
			{
				// Ignore mov thing into itself
				if (inst->dst.valueType == IRVALUETYPE_VALUE &&
					inst->src.valueType == IRVALUETYPE_VALUE)
				{
					Value dst = GetValueRead(context, inst->dst.value.valueIdx);
					Value src = GetValueRead(context, inst->src.value.valueIdx);
					if (dst.flags & VALUEFLAGS_IS_ALLOCATED && src.flags & VALUEFLAGS_IS_ALLOCATED)
					{
						// Value::stackOffset is alias of Value::allocatedRegister
						if (dst.allocatedRegister == src.allocatedRegister)
						{
							inst->type = X64_Ignore;
							break;
						}
					}
				}
			} // fall through
			case X64_MOVZX:
			case X64_MOVSX:
			case X64_MOVSXD:
			case X64_LEA:
			case X64_SETE:
			case X64_SETNE:
			case X64_SETL:
			case X64_SETLE:
			case X64_SETG:
			case X64_SETGE:
			case X64_SETA:
			case X64_SETAE:
			case X64_SETB:
			case X64_SETBE:
			case X64_CVTSI2SS:
			case X64_CVTSI2SD:
			case X64_CVTTSS2SI:
			case X64_CVTTSD2SI:
			case X64_CVTSS2SD:
			case X64_CVTSD2SS:
			{
				if (inst->dst.valueType == IRVALUETYPE_VALUE ||
					inst->dst.valueType == IRVALUETYPE_VALUE_DEREFERENCE)
				{
					Value v = GetValueRead(context, inst->dst.value.valueIdx);
					if (!(v.flags & (VALUEFLAGS_IS_USED | VALUEFLAGS_ON_STATIC_STORAGE)))
						inst->type = X64_Ignore;
				}
			} break;
			}

			// Zero idioms
			if (inst->type == X64_MOVSS || inst->type == X64_MOVSD)
			{
				if (inst->src.valueType == IRVALUETYPE_IMMEDIATE_INTEGER &&
					inst->src.immediate == 0)
				{
					if (IsValueInMemory(context, inst->dst))
						inst->type = X64_MOV;
					else
						*inst = { inst->type == X64_MOVSS ? X64_XORPS : X64_XORPD,
							inst->dst, inst->dst };
				}
			}
			else if (inst->type == X64_MOV)
			{
				if (inst->src.valueType == IRVALUETYPE_IMMEDIATE_INTEGER &&
					inst->src.immediate == 0)
				{
					if (!IsValueInMemory(context, inst->dst))
						*inst = { X64_XOR, inst->dst, inst->dst };
				}
			}

			// Unnecessary jumps
			if (nextInst && inst->type >= X64_Jump_Begin && inst->type <= X64_Jump_End &&
				nextInst->type == X64_Label)
			{
				if (inst->label == nextInst->label)
					inst->type = X64_Ignore;
			}

			// Replace CMP 0 with TEST
			if (nextInst2 && nextInst2->type == X64_CMP)
			{
				if (nextInst2->src.valueType == IRVALUETYPE_IMMEDIATE_INTEGER &&
					nextInst2->src.immediate == 0 && !IsValueInMemory(context, nextInst2->dst))
				{
					nextInst2->type = X64_TEST;
					nextInst2->src = nextInst2->dst;
				}
				else if (nextInst2->dst.valueType == IRVALUETYPE_IMMEDIATE_INTEGER &&
					nextInst2->dst.immediate == 0 && !IsValueInMemory(context, nextInst2->src))
				{
					nextInst2->type = X64_TEST;
					nextInst2->dst = nextInst2->src;
				}
			}

			// Avoid saving to bool then testing
			// @Todo: do this sort of thing with IR, should catch a lot more cases.
			if (nextInst2 && inst->type == X64_SETE && nextInst->type == X64_TEST &&
					nextInst2->type == X64_JE)
			{
				if (inst->dst.value.valueIdx == nextInst->dst.value.valueIdx &&
					inst->dst.value.valueIdx == nextInst->src.value.valueIdx)
				{
					inst->type = X64_Ignore;
					nextInst->type = X64_Ignore;
					nextInst2->type = X64_JE;
				}
			}

			inst = nextInst;
			nextInst = nextInst2;
			nextInst2 = X64InstructionStreamAdvance(&stream);
			while (nextInst2 && nextInst2->type >= X64_Count)
				nextInst2 = X64InstructionStreamAdvance(&stream);
		}
	}

	// TypeInfo immediate structs
	{
		u32 pointerToStructMemberInfoIdx =
			GetTypeInfoPointerOf(context, TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT);
		u32 pointerToStringIdx =
			GetTypeInfoPointerOf(context, TYPETABLEIDX_STRING_STRUCT);
		u32 pointerToS64Idx =
			GetTypeInfoPointerOf(context, TYPETABLEIDX_S64);
		u32 pointerToTypeInfoIdx =
			GetTypeInfoPointerOf(context, TYPETABLEIDX_TYPE_INFO_STRUCT);

		const auto &typeTable = context->typeTable.LockForRead();
		u64 tableSize = BucketArrayCount(&typeTable);
		for (u32 typeTableIdx = TYPETABLEIDX_Begin; typeTableIdx < tableSize; ++typeTableIdx)
		{
			TypeInfo typeInfo = GetTypeInfo(context, typeTableIdx);

			GetValueWrite(context, typeInfo.valueIdx)->typeTableIdx = TYPETABLEIDX_TYPE_INFO_STRUCT;

			IRStaticVariable newStaticVar = { typeInfo.valueIdx };
			newStaticVar.initialValue.valueType = IRVALUETYPE_IMMEDIATE_GROUP;
			newStaticVar.initialValue.typeTableIdx = TYPETABLEIDX_UNSET;

			switch (typeInfo.typeCategory)
			{
			case TYPECATEGORY_INTEGER:
			{
				ArrayInit(&newStaticVar.initialValue.immediateStructMembers, 3);
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
					{ IRValueImmediate(0, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
					{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
					{ IRValueImmediate(typeInfo.integerInfo.isSigned, TYPETABLEIDX_S32) };
			} break;
			case TYPECATEGORY_FLOATING:
			{
				newStaticVar.initialValue.typeTableIdx = TYPETABLEIDX_TYPE_INFO_STRUCT;
				ArrayInit(&newStaticVar.initialValue.immediateStructMembers, 2);
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
					{ IRValueImmediate(1, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
					{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
			} break;
			case TYPECATEGORY_STRUCT:
			case TYPECATEGORY_UNION:
			{
				String structName = typeInfo.structInfo.name;
				if (!structName.size)
					structName = "<anonymous struct>"_s;

				u32 membersValueIdx = NewValue(context, TPrintF("_members_%lld", typeTableIdx),
						TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT, VALUEFLAGS_ON_STATIC_STORAGE);
				IRStaticVariable membersStaticVar = { membersValueIdx };
				membersStaticVar.initialValue.valueType = IRVALUETYPE_IMMEDIATE_GROUP;
				membersStaticVar.initialValue.typeTableIdx = TYPETABLEIDX_UNSET;
				ArrayInit(&membersStaticVar.initialValue.immediateStructMembers,
						typeInfo.structInfo.members.size);
				for (s64 memberIdx = 0; memberIdx < (s64)typeInfo.structInfo.members.size; ++memberIdx)
				{
					StructMember member = typeInfo.structInfo.members[memberIdx];
					TypeInfo memberType = GetTypeInfo(context, member.typeTableIdx);

					IRValue memberImm;
					memberImm.valueType = IRVALUETYPE_IMMEDIATE_GROUP;
					memberImm.typeTableIdx = TYPETABLEIDX_UNSET;
					ArrayInit(&memberImm.immediateStructMembers, 4);
					*ArrayAdd(&memberImm.immediateStructMembers) =
						{ IRValueImmediateString(context, member.name) };
					*ArrayAdd(&memberImm.immediateStructMembers) =
						{ IRValueDereference(memberType.valueIdx, pointerToTypeInfoIdx) };
					*ArrayAdd(&memberImm.immediateStructMembers) =
						{ IRValueImmediate(member.offset, TYPETABLEIDX_S64) };

					*ArrayAdd(&membersStaticVar.initialValue.immediateStructMembers) = memberImm;
				}
				*DynamicArrayAdd(&context->irStaticVariables) = membersStaticVar;

				ArrayInit(&newStaticVar.initialValue.immediateStructMembers, 6);
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
					{ IRValueImmediate(2, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
					{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
					{ IRValueImmediateString(context, structName) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
					{ IRValueImmediate(typeInfo.typeCategory == TYPECATEGORY_UNION, TYPETABLEIDX_S32) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
					{ IRValueImmediate(typeInfo.structInfo.members.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
					{ IRValueDereference(membersValueIdx, pointerToStructMemberInfoIdx) };
			} break;
			case TYPECATEGORY_ENUM:
			{
				String enumName = typeInfo.enumInfo.name;
				if (!enumName.size)
					enumName = "<anonymous enum>"_s;

				TypeInfo enumType = GetTypeInfo(context, typeInfo.enumInfo.typeTableIdx);

				u32 namesValueIdx = NewValue(context, TPrintF("_names_%lld", typeTableIdx),
						TYPETABLEIDX_STRING_STRUCT, VALUEFLAGS_ON_STATIC_STORAGE);
				IRStaticVariable namesStaticVar = { namesValueIdx };
				namesStaticVar.initialValue.valueType = IRVALUETYPE_IMMEDIATE_GROUP;
				ArrayInit(&namesStaticVar.initialValue.immediateStructMembers,
						typeInfo.enumInfo.names.size);
				for (s64 nameIdx = 0; nameIdx < (s64)typeInfo.enumInfo.names.size; ++nameIdx)
				{
					IRValue nameImm = IRValueImmediateString(context,
							typeInfo.enumInfo.names[nameIdx]);
					*ArrayAdd(&namesStaticVar.initialValue.immediateStructMembers) = nameImm;
				}
				*DynamicArrayAdd(&context->irStaticVariables) = namesStaticVar;

				u32 valuesValueIdx = NewValue(context, TPrintF("_values_%lld", typeTableIdx),
						TYPETABLEIDX_S64, VALUEFLAGS_ON_STATIC_STORAGE);
				IRStaticVariable valuesStaticVar = { valuesValueIdx };
				valuesStaticVar.initialValue.valueType = IRVALUETYPE_IMMEDIATE_GROUP;
				ArrayInit(&valuesStaticVar.initialValue.immediateStructMembers,
						typeInfo.enumInfo.values.size);
				for (s64 valueIdx = 0; valueIdx < (s64)typeInfo.enumInfo.values.size; ++valueIdx)
				{
					IRValue valueImm = IRValueImmediate(typeInfo.enumInfo.values[valueIdx],
							TYPETABLEIDX_S64);
					*ArrayAdd(&valuesStaticVar.initialValue.immediateStructMembers) = valueImm;
				}
				*DynamicArrayAdd(&context->irStaticVariables) = valuesStaticVar;

				ArrayInit(&newStaticVar.initialValue.immediateStructMembers, 8);
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(3, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediateString(context, enumName) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueDereference(enumType.valueIdx, pointerToTypeInfoIdx) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(typeInfo.enumInfo.names.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueDereference(namesValueIdx, pointerToStringIdx) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(typeInfo.enumInfo.values.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueDereference(valuesValueIdx, pointerToS64Idx) };
			} break;
			case TYPECATEGORY_POINTER:
			{
				TypeInfo pointedType = GetTypeInfo(context, typeInfo.pointerInfo.pointedTypeTableIdx);

				ArrayInit(&newStaticVar.initialValue.immediateStructMembers, 3);
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(4, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueDereference(pointedType.valueIdx, pointerToTypeInfoIdx) };
			} break;
			case TYPECATEGORY_ARRAY:
			{
				TypeInfo elementType = GetTypeInfo(context, typeInfo.arrayInfo.elementTypeTableIdx);

				ArrayInit(&newStaticVar.initialValue.immediateStructMembers, 4);
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(5, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(typeInfo.arrayInfo.count, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueDereference(elementType.valueIdx, pointerToTypeInfoIdx) };
			} break;
			case TYPECATEGORY_PROCEDURE:
			{
				u32 parametersValueIdx = 0;
				if (typeInfo.procedureInfo.parameters.size > 0)
				{
					parametersValueIdx = NewValue(context, TPrintF("_params_%lld", typeTableIdx),
							pointerToTypeInfoIdx, VALUEFLAGS_ON_STATIC_STORAGE);
					IRStaticVariable paramsStaticVar = { parametersValueIdx };
					paramsStaticVar.initialValue.valueType = IRVALUETYPE_IMMEDIATE_GROUP;
					ArrayInit(&paramsStaticVar.initialValue.immediateStructMembers,
							typeInfo.procedureInfo.parameters.size);
					for (s64 paramIdx = 0; paramIdx < (s64)typeInfo.procedureInfo.parameters.size;
							++paramIdx)
					{
						TypeInfo paramType =
							GetTypeInfo(context, typeInfo.procedureInfo.parameters[paramIdx].typeTableIdx);
						IRValue paramImm = IRValueDereference(paramType.valueIdx, pointerToTypeInfoIdx);
						*ArrayAdd(&paramsStaticVar.initialValue.immediateStructMembers) = paramImm;
					}
					*DynamicArrayAdd(&context->irStaticVariables) = paramsStaticVar;
				}

				ArrayInit(&newStaticVar.initialValue.immediateStructMembers, 5);
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(6, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(typeInfo.procedureInfo.parameters.size, TYPETABLEIDX_S64) };
				if (parametersValueIdx)
					*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
					{ IRValueDereference(parametersValueIdx, pointerToTypeInfoIdx) };
				else
					*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
					{ IRValueImmediate(0, pointerToTypeInfoIdx) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(typeInfo.procedureInfo.isVarargs, TYPETABLEIDX_BOOL) };
			} break;
			case TYPECATEGORY_ALIAS:
			{
				TypeInfo aliasedType = GetTypeInfo(context, typeInfo.aliasInfo.aliasedTypeIdx);

				ArrayInit(&newStaticVar.initialValue.immediateStructMembers, 3);
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(7, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueDereference(aliasedType.valueIdx, pointerToTypeInfoIdx) };
			} break;
			case TYPECATEGORY_INVALID:
			{
				ArrayInit(&newStaticVar.initialValue.immediateStructMembers, 1);
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(8, TYPETABLEIDX_S8) };
			} break;
			default:
				ASSERT(false);
			}
			*DynamicArrayAdd(&context->irStaticVariables) = newStaticVar;
		}
		context->typeTable.UnlockForRead();
	}

	TimerSplit("X64 register/stack allocation"_s);

	BucketArrayInit(&context->outputBuffer);

#if _MSC_VER
	String memoryUtilFullpath = SYSExpandPathCompilerRelative("core\\memory_masm.asm"_s);
	PrintOut(context, "include <%S>\n\n", memoryUtilFullpath);
#else
	String memoryUtilFullpath = SYSExpandPathCompilerRelative("core/memory_nasm.asm"_s);
	PrintOut(context, "%cinclude \"%S\"\n\n", '%', memoryUtilFullpath);
#endif

#if _MSC_VER
	PrintOut(context, "_DATA SEGMENT\n");
#else
	PrintOut(context, "section .data\n");
#endif

	// String literals
	s64 strCount = (s64)BucketArrayCount(&context->stringLiterals);
	s64 bytesWritten = 0;
	for (s64 stringLiteralIdx = 1; stringLiteralIdx < strCount; ++stringLiteralIdx)
	{
		PrintOut(context, "_str_%lld DB ", stringLiteralIdx);
		String str = context->stringLiterals[stringLiteralIdx];
		s64 size = str.size;
		bool first = true;
		u8 *buffer = (u8 *)g_memory->framePtr;
		u8 *out = buffer;
		const u8 *in = (const u8 *)str.data;
		for (int i = 0; i < str.size; ++i)
		{
			if (*in == '\\')
			{
				if (!first) PrintOut(context, ", ");

				++in;
				switch (*in)
				{
				case 'n':
					PrintOut(context, "0AH");
					break;
				case '0':
					PrintOut(context, "00H");
					break;
				case '"':
					PrintOut(context, "22H");
					break;
				}
				++in;
				++i;
				--size; // Don't count backslash for string size.
				first = false;
			}
			else if (*in == '\'')
			{
				// MASM uses ' as string delimiters
				if (!first) PrintOut(context, ", ");
				PrintOut(context, "27H");
				++in;
				first = false;
			}
			else
			{
				*out++ = *in++;

				if (i == str.size - 1 || *in == '\\' || *in == '\'')
				{
					*out++ = 0;
					g_memory->framePtr = out;

					if (!first) PrintOut(context, ", ");
					PrintOut(context, "'%s'", buffer);
					out = buffer;

					first = false;
				}
			}
		}
		PrintOut(context, "\n");
		g_memory->framePtr = buffer;
		bytesWritten += size;
	}

	X64StaticDataAlignTo(context, 16, true);

	// Initialized
	const u64 staticVariableCount = context->irStaticVariables.size;
	for (int staticVariableIdx = 0; staticVariableIdx < staticVariableCount; ++staticVariableIdx)
	{
		IRStaticVariable staticVar = context->irStaticVariables[staticVariableIdx];
		if (staticVar.initialValue.valueType != IRVALUETYPE_INVALID &&
				staticVar.initialValue.immediate != 0)
		{
			Value value = GetValueRead(context, staticVar.valueIdx);

			String name = value.name;
			if (!(value.flags & VALUEFLAGS_IS_EXTERNAL))
				name = StringConcat("g_"_s, name);

			X64PrintStaticData(context, name, staticVar.initialValue, value.typeTableIdx, 16);
		}
	}

#if _MSC_VER
	PrintOut(context, "_DATA ENDS\n");
	PrintOut(context, "_BSS SEGMENT\n");
#else
	PrintOut(context, "section .bss\n");
#endif

	// Uninitialized
	// @Speed: don't iterate this twice...
	for (int staticVariableIdx = 0; staticVariableIdx < staticVariableCount; ++staticVariableIdx)
	{
		IRStaticVariable staticVar = context->irStaticVariables[staticVariableIdx];
		if (staticVar.initialValue.valueType == IRVALUETYPE_INVALID ||
				staticVar.initialValue.immediate == 0)
		{
			Value value = GetValueRead(context, staticVar.valueIdx);

			String name = value.name;
			if (!(value.flags & VALUEFLAGS_IS_EXTERNAL))
				name = StringConcat("g_"_s, name);

			X64PrintStaticDataUninitialized(context, name,
					staticVar.initialValue, value.typeTableIdx, 16);
		}
	}

#if _MSC_VER
	PrintOut(context, "_BSS ENDS\n");
#endif

#if !_MSC_VER
	for (int procedureIdx = 1; procedureIdx < procedureCount; ++procedureIdx)
	{
		Procedure proc = GetProcedureRead(context, procedureIdx);
		if (proc.isExported)
			PrintOut(context, "GLOBAL %S\n", proc->name);
	}
#endif

	for (int varIdx = 0; varIdx < context->irExternalVariables.size; ++varIdx)
	{
		Value v = GetValueRead(context, context->irExternalVariables[varIdx]);
		s64 size = GetTypeInfo(context, v.typeTableIdx).size;
		String type;
		switch (size)
		{
			case 1: type = "BYTE"_s; break;
			case 2: type = "WORD"_s; break;
			case 4: type = "DWORD"_s; break;
			case 8: type = "QWORD"_s; break;
			default: type = "QWORD"_s;
		}
#if _MSC_VER
		PrintOut(context, "EXTRN %S:%S\n", v.name, type);
#else
		PrintOut(context, "EXTERN %S:%S\n", v.name, type);
#endif
	}

	{
		auto externalProcedures = context->externalProcedures.GetForRead();
		u64 externalProcedureCount = BucketArrayCount(&externalProcedures);
		for (int procedureIdx = 1; procedureIdx < externalProcedureCount; ++procedureIdx)
		{
			// Don't declare hard-coded procedures that are already included in the asm file.
			if (procedureIdx == -copyMemoryProcIdx || procedureIdx == -zeroMemoryProcIdx)
				continue;

			String procName = (*externalProcedures)[procedureIdx].name;
#if _MSC_VER
			PrintOut(context, "EXTRN %S:proc\n", procName);
#else
			PrintOut(context, "EXTERN %S\n", procName);
#endif
		}
	}

#if _MSC_VER
	PrintOut(context, "_TEXT SEGMENT\n");
#else
	PrintOut(context, "section .text\n");
#endif

	// Code
	X64PrintInstructions(context, x64Procedures);

#if _MSC_VER
	PrintOut(context, "_TEXT ENDS\n");
	PrintOut(context, "END\n");
#endif

	String outputPath = SYSExpandPathWorkingDirectoryRelative("output"_s);
	SYSCreateDirectory(outputPath);

	WriteOutOutputBuffer(context, "output/out.asm"_s);

	TimerSplit("X64 output file write"_s);

	bool makeLibrary = false;
	{
		auto staticDefinitions = context->staticDefinitions.GetForRead();
		u64 staticDefinitionCount = BucketArrayCount(&staticDefinitions);
		for (u64 i = 0; i < staticDefinitionCount; ++i)
		{
			const StaticDefinition *currentDef = &(*staticDefinitions)[i];
			if (StringEquals("compiler_output_type"_s, currentDef->name))
			{
				ASSERT(currentDef->definitionType == STATICDEFINITIONTYPE_CONSTANT);
				ASSERT(currentDef->constant.type == CONSTANTTYPE_INTEGER);
				makeLibrary = currentDef->constant.valueAsInt == 1;
			}
		}
	}

	String extraLinkerArguments = {};
	for (int i = 0; i < context->libsToLink.size; ++i)
		extraLinkerArguments = TPrintF("%S %S", extraLinkerArguments,
				context->libsToLink[i]);

#if _MSC_VER
	bool useWindowsSubsystem = false;
	{
		auto staticDefinitions = context->staticDefinitions.GetForRead();
		u64 staticDefinitionCount = BucketArrayCount(&staticDefinitions);
		for (u64 i = 0; i < staticDefinitionCount; ++i)
		{
			const StaticDefinition *currentDef = &(*staticDefinitions)[i];
			if (StringEquals("compiler_subsystem"_s, currentDef->name))
			{
				ASSERT(currentDef->definitionType == STATICDEFINITIONTYPE_CONSTANT);
				ASSERT(currentDef->constant.type == CONSTANTTYPE_INTEGER);
				useWindowsSubsystem = currentDef->constant.valueAsInt == 1;
			}
		}
	}

	String subsystemArgument;
	if (useWindowsSubsystem)
		subsystemArgument = "/subsystem:WINDOWS "_s;
	else
		subsystemArgument = "/subsystem:CONSOLE "_s;

	extraLinkerArguments = TPrintF("%S %S", extraLinkerArguments, subsystemArgument);
#endif

	if (!context->config.dontCallAssembler)
	{
		SYSRunAssembler(outputPath, ""_s);
		TimerSplit("Calling assembler"_s);

		SYSRunLinker(outputPath, makeLibrary, extraLinkerArguments);
		TimerSplit("Calling linker"_s);
	}
}
