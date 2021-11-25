enum AcceptedOperandFlags
{
	ACCEPTEDOPERANDS_REGISTER  = 1,
	ACCEPTEDOPERANDS_MEMORY    = 2,
	ACCEPTEDOPERANDS_REGMEM    = 3,
	ACCEPTEDOPERANDS_IMMEDIATE = 4,
	ACCEPTEDOPERANDS_ALL       = 7,
};

enum InstructionFlags
{
	INSTRUCTIONFLAGS_DONT_CALCULATE_ADDRESSES = 1,
};

struct X64InstructionInfo
{
	String mnemonic;
	u8 acceptedOperandsLeft;
	u8 acceptedOperandsRight;
	u8 flags;
};

enum X64InstructionType
{
	X64_MOV,
	X64_MOVZX,
	X64_MOVSX,
	X64_MOVSXD,
	X64_CQO,
	X64_PUSH,
	X64_POP,
	X64_JMP,
	X64_JE,
	X64_JNE,
	X64_CALL,
	X64_LEAVE,
	X64_RET,
	X64_LEA,
	X64_CMP,
	X64_SETG,
	X64_SETL,
	X64_SETGE,
	X64_SETLE,
	X64_SETE,
	X64_ADD,
	X64_SUB,
	X64_MUL,
	X64_IMUL,
	X64_DIV,
	X64_IDIV,
	X64_SAR,
	X64_SAL,
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
	X64_COMISS,
	X64_COMISD,
	X64_CVTSI2SS,
	X64_CVTSI2SD,
	X64_CVTSS2SI,
	X64_CVTSD2SI,
	X64_CVTSS2SD,
	X64_CVTSD2SS,
	X64_Label,
	X64_Push_Scope,
	X64_Pop_Scope,
	X64_Push_Variable,
	X64_Patch,
	X64_Patch_Many
};

struct X64Instruction
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
		String procLabel;
		Variable *variable;
		struct
		{
			X64Instruction *patch1;
			X64Instruction *patch2;
		};
		Array<X64Instruction> patchInstructions;
	};
};

struct X64Procedure
{
	String name;
	BucketArray<X64Instruction, 1024, malloc, realloc> instructions;
	u64 stackSize;
	u64 virtualRegisterCount;
	s64 allocatedParameterCount;
	DynamicArray<Variable *, malloc, realloc> spillVariables;
	u64 noSpillRegisters[8];
};

enum X64FloatingType
{
	X64FLOATINGTYPE_NONE,
	X64FLOATINGTYPE_F32,
	X64FLOATINGTYPE_F64
};

const X64InstructionInfo MOV      = { "mov"_s,       ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0 };
const X64InstructionInfo MOVZX    = { "movzx"_s,     ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0 };
const X64InstructionInfo MOVSX    = { "movsx"_s,     ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM, 0};
const X64InstructionInfo MOVSXD   = { "movsxd"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM, 0};
const X64InstructionInfo LEA      = { "lea"_s,       ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_MEMORY, INSTRUCTIONFLAGS_DONT_CALCULATE_ADDRESSES };
const X64InstructionInfo CMP      = { "cmp"_s,       ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0};
const X64InstructionInfo ADD      = { "add"_s,       ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0};
const X64InstructionInfo SUB      = { "sub"_s,       ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0};
const X64InstructionInfo IMUL     = { "imul"_s,      ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0};
const X64InstructionInfo SAR      = { "sar"_s,       ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0};
const X64InstructionInfo SAL      = { "sal"_s,       ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0};
const X64InstructionInfo AND      = { "and"_s,       ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0};
const X64InstructionInfo OR       = { "or"_s,        ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0};
const X64InstructionInfo XOR      = { "xor"_s,       ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0};
const X64InstructionInfo MOVSS    = { "movss"_s,     ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM, 0 };
const X64InstructionInfo ADDSS    = { "addss"_s,     ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM, 0};
const X64InstructionInfo SUBSS    = { "subss"_s,     ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM, 0};
const X64InstructionInfo MULSS    = { "mulss"_s,     ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM, 0};
const X64InstructionInfo COMISS   = { "comiss"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM, 0};
const X64InstructionInfo COMISD   = { "comisd"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM, 0};
const X64InstructionInfo CVTSI2SS = { "cvtsi2ss"_s,  ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_REGMEM, 0};
const X64InstructionInfo CVTSI2SD = { "cvtsi2sd"_s,  ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_REGMEM, 0};
const X64InstructionInfo CVTSS2SI = { "cvtss2si"_s,  ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_REGMEM, 0};
const X64InstructionInfo CVTSD2SI = { "cvtsd2si"_s,  ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_REGMEM, 0};
const X64InstructionInfo CVTSS2SD = { "cvtss2sd"_s,  ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_REGMEM, 0};
const X64InstructionInfo CVTSD2SS = { "cvtsd2ss"_s,  ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_REGMEM, 0};

const s64 registerIndexBegin = 0x4000000000000000;
const s64 RAX_idx = registerIndexBegin + 0;
const s64 RCX_idx = registerIndexBegin + 1;
const s64 RDX_idx = registerIndexBegin + 2;
const s64 RBX_idx = registerIndexBegin + 3;
const s64 RSI_idx = registerIndexBegin + 4;
const s64 RDI_idx = registerIndexBegin + 5;
const s64 RSP_idx = registerIndexBegin + 6;
const s64 RBP_idx = registerIndexBegin + 7;
const s64 R8_idx  = registerIndexBegin + 8;
const s64 R9_idx  = registerIndexBegin + 9;
const s64 R10_idx = registerIndexBegin + 10;
const s64 R11_idx = registerIndexBegin + 11;
const s64 R12_idx = registerIndexBegin + 12;
const s64 R13_idx = registerIndexBegin + 13;
const s64 R14_idx = registerIndexBegin + 14;
const s64 R15_idx = registerIndexBegin + 15;
const s64 XMM0_idx = registerIndexBegin + 16;
const s64 XMM1_idx = registerIndexBegin + 17;
const s64 XMM2_idx = registerIndexBegin + 18;
const s64 XMM3_idx = registerIndexBegin + 19;
const s64 XMM4_idx = registerIndexBegin + 20;
const s64 XMM5_idx = registerIndexBegin + 21;
const s64 XMM6_idx = registerIndexBegin + 22;
const s64 XMM7_idx = registerIndexBegin + 23;

const IRValue RAX = { IRVALUETYPE_REGISTER, RAX_idx, TYPETABLEIDX_S64 };
const IRValue RCX = { IRVALUETYPE_REGISTER, RCX_idx, TYPETABLEIDX_S64 };
const IRValue RDX = { IRVALUETYPE_REGISTER, RDX_idx, TYPETABLEIDX_S64 };
const IRValue RBX = { IRVALUETYPE_REGISTER, RBX_idx, TYPETABLEIDX_S64 };
const IRValue RSI = { IRVALUETYPE_REGISTER, RSI_idx, TYPETABLEIDX_S64 };
const IRValue RDI = { IRVALUETYPE_REGISTER, RDI_idx, TYPETABLEIDX_S64 };
const IRValue RSP = { IRVALUETYPE_REGISTER, RSP_idx, TYPETABLEIDX_S64 };
const IRValue RBP = { IRVALUETYPE_REGISTER, RBP_idx, TYPETABLEIDX_S64 };
const IRValue R8  = { IRVALUETYPE_REGISTER, R8_idx,  TYPETABLEIDX_S64 };
const IRValue R9  = { IRVALUETYPE_REGISTER, R9_idx,  TYPETABLEIDX_S64 };
const IRValue R10 = { IRVALUETYPE_REGISTER, R10_idx, TYPETABLEIDX_S64 };
const IRValue R11 = { IRVALUETYPE_REGISTER, R11_idx, TYPETABLEIDX_S64 };
const IRValue R12 = { IRVALUETYPE_REGISTER, R12_idx, TYPETABLEIDX_S64 };
const IRValue R13 = { IRVALUETYPE_REGISTER, R13_idx, TYPETABLEIDX_S64 };
const IRValue R14 = { IRVALUETYPE_REGISTER, R14_idx, TYPETABLEIDX_S64 };
const IRValue R15 = { IRVALUETYPE_REGISTER, R15_idx, TYPETABLEIDX_S64 };

const IRValue EAX  = { IRVALUETYPE_REGISTER, RAX_idx, TYPETABLEIDX_S32 };
const IRValue ECX  = { IRVALUETYPE_REGISTER, RCX_idx, TYPETABLEIDX_S32 };
const IRValue EDX  = { IRVALUETYPE_REGISTER, RDX_idx, TYPETABLEIDX_S32 };
const IRValue EBX  = { IRVALUETYPE_REGISTER, RBX_idx, TYPETABLEIDX_S32 };
const IRValue ESI  = { IRVALUETYPE_REGISTER, RSI_idx, TYPETABLEIDX_S32 };
const IRValue EDI  = { IRVALUETYPE_REGISTER, RDI_idx, TYPETABLEIDX_S32 };
const IRValue ESP  = { IRVALUETYPE_REGISTER, RSP_idx, TYPETABLEIDX_S32 };
const IRValue EBP  = { IRVALUETYPE_REGISTER, RBP_idx, TYPETABLEIDX_S32 };
const IRValue R8D  = { IRVALUETYPE_REGISTER, R8_idx,  TYPETABLEIDX_S32 };
const IRValue R9D  = { IRVALUETYPE_REGISTER, R9_idx,  TYPETABLEIDX_S32 };
const IRValue R10D = { IRVALUETYPE_REGISTER, R10_idx, TYPETABLEIDX_S32 };
const IRValue R11D = { IRVALUETYPE_REGISTER, R11_idx, TYPETABLEIDX_S32 };
const IRValue R12D = { IRVALUETYPE_REGISTER, R12_idx, TYPETABLEIDX_S32 };
const IRValue R13D = { IRVALUETYPE_REGISTER, R13_idx, TYPETABLEIDX_S32 };
const IRValue R14D = { IRVALUETYPE_REGISTER, R14_idx, TYPETABLEIDX_S32 };
const IRValue R15D = { IRVALUETYPE_REGISTER, R15_idx, TYPETABLEIDX_S32 };

const IRValue AX   = { IRVALUETYPE_REGISTER, RAX_idx, TYPETABLEIDX_S16 };
const IRValue CX   = { IRVALUETYPE_REGISTER, RCX_idx, TYPETABLEIDX_S16 };
const IRValue DX   = { IRVALUETYPE_REGISTER, RDX_idx, TYPETABLEIDX_S16 };
const IRValue BX   = { IRVALUETYPE_REGISTER, RBX_idx, TYPETABLEIDX_S16 };
const IRValue SI   = { IRVALUETYPE_REGISTER, RSI_idx, TYPETABLEIDX_S16 };
const IRValue DI   = { IRVALUETYPE_REGISTER, RDI_idx, TYPETABLEIDX_S16 };
const IRValue SP   = { IRVALUETYPE_REGISTER, RSP_idx, TYPETABLEIDX_S16 };
const IRValue BP   = { IRVALUETYPE_REGISTER, RBP_idx, TYPETABLEIDX_S16 };
const IRValue R8W  = { IRVALUETYPE_REGISTER, R8_idx,  TYPETABLEIDX_S16 };
const IRValue R9W  = { IRVALUETYPE_REGISTER, R9_idx,  TYPETABLEIDX_S16 };
const IRValue R10W = { IRVALUETYPE_REGISTER, R10_idx, TYPETABLEIDX_S16 };
const IRValue R11W = { IRVALUETYPE_REGISTER, R11_idx, TYPETABLEIDX_S16 };
const IRValue R12W = { IRVALUETYPE_REGISTER, R12_idx, TYPETABLEIDX_S16 };
const IRValue R13W = { IRVALUETYPE_REGISTER, R13_idx, TYPETABLEIDX_S16 };
const IRValue R14W = { IRVALUETYPE_REGISTER, R14_idx, TYPETABLEIDX_S16 };
const IRValue R15W = { IRVALUETYPE_REGISTER, R15_idx, TYPETABLEIDX_S16 };

const IRValue AL   = { IRVALUETYPE_REGISTER, RAX_idx, TYPETABLEIDX_S8 };
const IRValue CL   = { IRVALUETYPE_REGISTER, RCX_idx, TYPETABLEIDX_S8 };
const IRValue DL   = { IRVALUETYPE_REGISTER, RDX_idx, TYPETABLEIDX_S8 };
const IRValue BL   = { IRVALUETYPE_REGISTER, RBX_idx, TYPETABLEIDX_S8 };
const IRValue SIL  = { IRVALUETYPE_REGISTER, RSI_idx, TYPETABLEIDX_S8 };
const IRValue DIL  = { IRVALUETYPE_REGISTER, RDI_idx, TYPETABLEIDX_S8 };
const IRValue SPL  = { IRVALUETYPE_REGISTER, RSP_idx, TYPETABLEIDX_S8 };
const IRValue BPL  = { IRVALUETYPE_REGISTER, RBP_idx, TYPETABLEIDX_S8 };
const IRValue R8B  = { IRVALUETYPE_REGISTER, R8_idx,  TYPETABLEIDX_S8 };
const IRValue R9B  = { IRVALUETYPE_REGISTER, R9_idx,  TYPETABLEIDX_S8 };
const IRValue R10B = { IRVALUETYPE_REGISTER, R10_idx, TYPETABLEIDX_S8 };
const IRValue R11B = { IRVALUETYPE_REGISTER, R11_idx, TYPETABLEIDX_S8 };
const IRValue R12B = { IRVALUETYPE_REGISTER, R12_idx, TYPETABLEIDX_S8 };
const IRValue R13B = { IRVALUETYPE_REGISTER, R13_idx, TYPETABLEIDX_S8 };
const IRValue R14B = { IRVALUETYPE_REGISTER, R14_idx, TYPETABLEIDX_S8 };
const IRValue R15B = { IRVALUETYPE_REGISTER, R15_idx, TYPETABLEIDX_S8 };

void PrintOut(Context *context, HANDLE outputFile, const char *format, ...)
{
	char *buffer = (char *)g_memory->framePtr;

	va_list args;
	va_start(args, format);

	stbsp_vsprintf(buffer, format, args);

	DWORD bytesWritten;
#if PRINT_ASM_OUTPUT
	if (!context->config.silent)
	{
		OutputDebugStringA(buffer);
		WriteFile(g_hStdout, buffer, (DWORD)strlen(buffer), &bytesWritten, nullptr); // Stdout
	}
#endif
	WriteFile(outputFile, buffer, (DWORD)strlen(buffer), &bytesWritten, nullptr);

	va_end(args);
}

inline u8 IRValueTypeToFlags(IRValueType valueType)
{
	switch (valueType)
	{
	case IRVALUETYPE_INVALID:
		return 0;
	case IRVALUETYPE_REGISTER:
		return ACCEPTEDOPERANDS_REGISTER;
	case IRVALUETYPE_MEMORY_REGISTER:
	case IRVALUETYPE_MEMORY_VARIABLE:
		return ACCEPTEDOPERANDS_MEMORY;
	default:
		return ACCEPTEDOPERANDS_IMMEDIATE;
	}
}

String X64RegisterToStr(s64 registerIdx, s64 size, bool floating)
{
	String result = "???REG"_s;
	if (!floating)
	{
#if 0
		// Map virtual registers to logical registers
		switch (registerIdx)
		{
		case 0: registerIdx = RBX_idx; break;
		case 1: registerIdx = R10_idx; break;
		case 2: registerIdx = R11_idx; break;
		case 3: registerIdx = R12_idx; break;
		case 4: registerIdx = R13_idx; break;
		case 5: registerIdx = R14_idx; break;
		case 6: registerIdx = R15_idx; break;
		case 7: registerIdx = RSI_idx; break;
		case 8: registerIdx = RDI_idx; break;
		default:
			ASSERT(registerIdx >= RAX_idx || !"Out of temporal registers");
		}
#endif

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
			case IRSPECIALREGISTER_SHOULD_RETURN:
				result = "r12"_s;
				break;
			case IRSPECIALREGISTER_RETURN:
				result = "rax"_s;
				break;
			default:
				result = TPrintF("vr%lld", registerIdx);
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
			case IRSPECIALREGISTER_SHOULD_RETURN:
				result = "r12d"_s;
				break;
			case IRSPECIALREGISTER_RETURN:
				result = "eax"_s;
				break;
			default:
				result = TPrintF("vr%lldd", registerIdx);
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
			case IRSPECIALREGISTER_SHOULD_RETURN:
				result = "r12w"_s;
				break;
			case IRSPECIALREGISTER_RETURN:
				result = "ax"_s;
				break;
			default:
				result = TPrintF("vr%lldw", registerIdx);
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
			case IRSPECIALREGISTER_SHOULD_RETURN:
				result = "r12b"_s;
				break;
			case IRSPECIALREGISTER_RETURN:
				result = "al"_s;
				break;
			default:
				result = TPrintF("vr%lldb", registerIdx);
			}
			break;
		default:
			ASSERT(!"Invalid size for a register!");
		}
	}
	else
	{
#if 0
		// Map virtual registers to logical registers
		switch (registerIdx)
		{
		case 0: registerIdx = XMM0_idx; break;
		case 1: registerIdx = XMM1_idx; break;
		case 2: registerIdx = XMM2_idx; break;
		case 3: registerIdx = XMM3_idx; break;
		case 4: registerIdx = XMM4_idx; break;
		case 5: registerIdx = XMM5_idx; break;
		case 6: registerIdx = XMM6_idx; break;
		case 7: registerIdx = XMM7_idx; break;
		default:
			ASSERT(registerIdx >= RAX_idx || !"Out of temporal registers");
		}
#endif

		switch (registerIdx)
		{
		case XMM0_idx: result = "xmm0"_s; break;
		case XMM1_idx: result = "xmm1"_s; break;
		case XMM2_idx: result = "xmm2"_s; break;
		case XMM3_idx: result = "xmm3"_s; break;
		case XMM4_idx: result = "xmm4"_s; break;
		case XMM5_idx: result = "xmm5"_s; break;
		case XMM6_idx: result = "xmm6"_s; break;
		case XMM7_idx: result = "xmm7"_s; break;
		default:
			result = TPrintF("vr%lldv", registerIdx);
		}
	}
	return result;
}

String X64IRValueToStr(Context *context, IRValue value)
{
	String result = "???VALUE"_s;

	TypeInfo *typeInfo = &context->typeTable[value.typeTableIdx];
	u64 size = typeInfo->size;
	bool floating = typeInfo->typeCategory == TYPECATEGORY_FLOATING;

	if (value.valueType == IRVALUETYPE_REGISTER)
	{
		result = X64RegisterToStr(value.registerIdx, size, floating);
	}
	else if (value.valueType == IRVALUETYPE_MEMORY_REGISTER)
	{
		result = X64RegisterToStr(value.memory.baseRegister, 8, false);
		if (value.memory.offset)
			if (value.memory.offset > 0)
				result = TPrintF("%S+%llu", result, value.memory.offset);
			else
				result = TPrintF("%S-%llu", result, -value.memory.offset);
	}
	else if (value.valueType == IRVALUETYPE_MEMORY_VARIABLE)
	{
		s64 offset = value.memory.offset;

		if (value.memory.baseVariable->isStatic ||
				!value.memory.baseVariable->isAllocated)
			result = TPrintF("%S", value.memory.baseVariable->name);
		else
		{
			result = X64RegisterToStr(RSP_idx, 8, false);
			//ASSERT(value.memory.baseVariable->isAllocated);
			offset += value.memory.baseVariable->stackOffset;
		}

		if (offset)
			if (offset > 0)
				result = TPrintF("%S+%llu", result, offset);
			else
				result = TPrintF("%S-%llu", result, -offset);
	}
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
	{
		result = TPrintF("%lld", value.immediate);
	}
	else
		ASSERT(!"Invalid value type!");

	if (value.valueType == IRVALUETYPE_MEMORY_REGISTER || value.valueType == IRVALUETYPE_MEMORY_VARIABLE)
	{
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
		default:
			result = TPrintF("QWORD PTR [%S]", result);
			break;
		//default:
			//ASSERT(!"Invalid size!");
		}
	}

	return result;
}

String X64ProcedureToLabel(Context *context, Procedure *proc)
{
	StaticDefinition *staticDef = FindStaticDefinitionByProcedure(context, proc);
	if (staticDef)
		return staticDef->name;
	else
		return TPrintF("proc.%X", proc);
}

void X64OutputInstruction(Context *context, HANDLE outputFile, X64InstructionInfo instInfo,
		IRValue first, IRValue second)
{
	ASSERT(instInfo.acceptedOperandsLeft & IRValueTypeToFlags(first.valueType));
	String firstStr = X64IRValueToStr(context, first);

	ASSERT(instInfo.acceptedOperandsRight & IRValueTypeToFlags(second.valueType));
	String secondStr = X64IRValueToStr(context, second);

	PrintOut(context, outputFile, "%S %S, %S\n", instInfo.mnemonic, firstStr, secondStr);
}

void X64OutputInstruction(Context *context, HANDLE outputFile, X64InstructionInfo instInfo,
		IRValue first)
{
	String firstStr;
	if (instInfo.acceptedOperandsLeft & IRValueTypeToFlags(first.valueType))
		// Type of operator allowed
		firstStr = X64IRValueToStr(context, first);
	else
	{
		// Type of operator not allowed, copy to register
		ASSERT(instInfo.acceptedOperandsLeft & ACCEPTEDOPERANDS_REGISTER);
		IRValue sizedRAX = IRValueRegister(RAX_idx, first.typeTableIdx);
		X64OutputInstruction(context, outputFile, MOV, sizedRAX, first);

		TypeInfo *typeInfo = &context->typeTable[first.typeTableIdx];
		firstStr = X64RegisterToStr(RAX_idx, typeInfo->size,
				typeInfo->typeCategory == TYPECATEGORY_FLOATING);
	}

	PrintOut(context, outputFile, "%S %S\n", instInfo.mnemonic, firstStr);
}

void X64OutputInstruction3(Context *context, HANDLE outputFile, X64InstructionInfo instInfo,
		IRValue out, IRValue left, IRValue right)
{
	bool floating = context->typeTable[out.typeTableIdx].typeCategory == TYPECATEGORY_FLOATING;

	IRValue copyToOut, operand;
	if (AreIRValuesDependent(context, out, right))
	{
		// if out operand is same as right, copy right to out, not left!
		copyToOut = right;
		operand   = left;
	}
	else
	{
		copyToOut = left;
		operand   = right;
	}

	if (!floating)
		X64OutputInstruction(context, outputFile, MOV, out, copyToOut);
	else
		X64OutputInstruction(context, outputFile, MOVSS, out, copyToOut);

	X64OutputInstruction(context, outputFile, instInfo, out, operand);
}

void X64ProcessInstruction(Context *context, HANDLE outputFile, IRInstruction inst)
{
	String conditionCode;

	if (inst.type >= IRINSTRUCTIONTYPE_BINARY_BEGIN && inst.type <= IRINSTRUCTIONTYPE_BINARY_END)
	{
		bool floating =
			context->typeTable[inst.binaryOperation.out.typeTableIdx].typeCategory ==
			TYPECATEGORY_FLOATING;

		switch (inst.type)
		{
		case IRINSTRUCTIONTYPE_ADD:
		{
			if (!floating)
				X64OutputInstruction3(context, outputFile, ADD, inst.binaryOperation.out,
						inst.binaryOperation.left, inst.binaryOperation.right);
			else
				X64OutputInstruction3(context, outputFile, ADDSS, inst.binaryOperation.out,
						inst.binaryOperation.left, inst.binaryOperation.right);
		} break;
		case IRINSTRUCTIONTYPE_SUBTRACT:
		{
			if (!floating)
				X64OutputInstruction3(context, outputFile, SUB, inst.binaryOperation.out,
						inst.binaryOperation.left, inst.binaryOperation.right);
			else
				X64OutputInstruction3(context, outputFile, SUBSS, inst.binaryOperation.out,
						inst.binaryOperation.left, inst.binaryOperation.right);
		} break;
		case IRINSTRUCTIONTYPE_MULTIPLY:
		{
			if (!floating)
				X64OutputInstruction3(context, outputFile, IMUL, inst.binaryOperation.out,
						inst.binaryOperation.left, inst.binaryOperation.right);
			else
				X64OutputInstruction3(context, outputFile, MULSS, inst.binaryOperation.out,
						inst.binaryOperation.left, inst.binaryOperation.right);
		} break;
		case IRINSTRUCTIONTYPE_DIVIDE:
		{
			String left = X64IRValueToStr(context, inst.binaryOperation.left);
			String right = X64IRValueToStr(context, inst.binaryOperation.right);
			String out = X64IRValueToStr(context, inst.binaryOperation.out);
			X64OutputInstruction(context, outputFile, MOV, inst.binaryOperation.out, inst.binaryOperation.left);
			X64OutputInstruction(context, outputFile, MOV, RAX, inst.binaryOperation.left);
			PrintOut(context, outputFile, "cqo\n");
			if (inst.binaryOperation.right.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
			{
				X64OutputInstruction(context, outputFile, MOV, RCX, inst.binaryOperation.right);
				PrintOut(context, outputFile, "idiv rcx\n");
			}
			else
				PrintOut(context, outputFile, "idiv %S\n", right);
			X64OutputInstruction(context, outputFile, MOV, inst.binaryOperation.out, RAX);
		} break;
		case IRINSTRUCTIONTYPE_MODULO:
		{
			String left = X64IRValueToStr(context, inst.binaryOperation.left);
			String right = X64IRValueToStr(context, inst.binaryOperation.right);
			String out = X64IRValueToStr(context, inst.binaryOperation.out);
			X64OutputInstruction(context, outputFile, MOV, RAX, inst.binaryOperation.left);
			PrintOut(context, outputFile, "cqo\n");
			if (inst.binaryOperation.right.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
			{
				X64OutputInstruction(context, outputFile, MOV, RCX, inst.binaryOperation.right);
				PrintOut(context, outputFile, "idiv rcx\n");
			}
			else
				PrintOut(context, outputFile, "idiv %S\n", right);
			X64OutputInstruction(context, outputFile, MOV, inst.binaryOperation.out, RDX);
		} break;
		case IRINSTRUCTIONTYPE_SHIFT_LEFT:
		{
			ASSERT(!floating);
			X64OutputInstruction3(context, outputFile, SAL, inst.binaryOperation.out,
					inst.binaryOperation.left, inst.binaryOperation.right);
		} break;
		case IRINSTRUCTIONTYPE_SHIFT_RIGHT:
		{
			ASSERT(!floating);
			X64OutputInstruction3(context, outputFile, SAR, inst.binaryOperation.out,
					inst.binaryOperation.left, inst.binaryOperation.right);
		} break;
		case IRINSTRUCTIONTYPE_BITWISE_AND:
		{
			ASSERT(!floating);
			X64OutputInstruction3(context, outputFile, AND, inst.binaryOperation.out,
					inst.binaryOperation.left, inst.binaryOperation.right);
		} break;
		case IRINSTRUCTIONTYPE_BITWISE_OR:
		{
			ASSERT(!floating);
			X64OutputInstruction3(context, outputFile, OR, inst.binaryOperation.out,
					inst.binaryOperation.left, inst.binaryOperation.right);
		} break;
		case IRINSTRUCTIONTYPE_BITWISE_XOR:
		{
			ASSERT(!floating);
			X64OutputInstruction3(context, outputFile, XOR, inst.binaryOperation.out,
					inst.binaryOperation.left, inst.binaryOperation.right);
		} break;
		case IRINSTRUCTIONTYPE_GREATER_THAN:
		{
			conditionCode = "g"_s;
			goto doConditionalSet;
		} break;
		case IRINSTRUCTIONTYPE_LESS_THAN:
		{
			conditionCode = "l"_s;
			goto doConditionalSet;
		} break;
		case IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS:
		{
			conditionCode = "ge"_s;
			goto doConditionalSet;
		} break;
		case IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS:
		{
			conditionCode = "le"_s;
			goto doConditionalSet;
		} break;
		case IRINSTRUCTIONTYPE_EQUALS:
		{
			conditionCode = "e"_s;
			goto doConditionalSet;
		} break;
		default:
		{
			ASSERT(!"Didn't recognize instruction type");
			PrintOut(context, outputFile, "???INST\n");
		} break;
		}
	}
	else switch (inst.type)
	{
	case IRINSTRUCTIONTYPE_NOP:
	case IRINSTRUCTIONTYPE_PUSH_VARIABLE:
	case IRINSTRUCTIONTYPE_PUSH_SCOPE:
	case IRINSTRUCTIONTYPE_POP_SCOPE:
	{
	} break;
	case IRINSTRUCTIONTYPE_COMMENT:
	{
		PrintOut(context, outputFile, "; %S\n", inst.comment);
	} break;
	case IRINSTRUCTIONTYPE_ASSIGNMENT:
	{
		if (context->typeTable[inst.binaryOperation.out.typeTableIdx].typeCategory !=
			TYPECATEGORY_FLOATING)
			X64OutputInstruction(context, outputFile, MOV, inst.assignment.dst, inst.assignment.src);
		else
			X64OutputInstruction(context, outputFile, MOVSS, inst.assignment.dst, inst.assignment.src);
	} break;
	case IRINSTRUCTIONTYPE_ASSIGNMENT_ZERO_EXTEND:
	{
		X64OutputInstruction(context, outputFile, MOVZX, inst.assignment.dst, inst.assignment.src);
	} break;
	case IRINSTRUCTIONTYPE_GET_PARAMETER:
	{
		IRValue value;
		switch (inst.getParameter.parameterIdx)
		{
			case 0: value = RCX; break;
			case 1: value = RDX; break;
			case 2: value = R8;  break;
			case 3: value = R9;  break;
							// Add 16, 8 for return address, and 8 because we push RBP
			default: value = IRValueMemory(RBP_idx, 16 + inst.getParameter.parameterIdx * 8, TYPETABLEIDX_S64);
		}
		X64OutputInstruction(context, outputFile, MOV, inst.getParameter.dst, value);
	} break;
	case IRINSTRUCTIONTYPE_GET_TYPE_INFO:
	{
		Variable fakeVar = {};
		fakeVar.parameterIndex = -1;
		fakeVar.name = TPrintF("_typeInfo%lld", inst.getTypeInfo.typeTableIdx);
		fakeVar.isStatic = true;
		IRValue typeInfoVar = IRValueFromVariable(context, &fakeVar);
		X64OutputInstruction(context, outputFile, LEA, inst.getTypeInfo.out, typeInfoVar);
	} break;
	case IRINSTRUCTIONTYPE_NOT:
	{
		String in = X64IRValueToStr(context, inst.unaryOperation.in);
		String out = X64IRValueToStr(context, inst.unaryOperation.out);
		X64OutputInstruction(context, outputFile, CMP, inst.unaryOperation.in, IRValueImmediate(0));
		PrintOut(context, outputFile, "sete al\n");
		PrintOut(context, outputFile, "movzx rax, al\n");
		IRValue sizedRAX = IRValueRegister(RAX_idx, inst.unaryOperation.out.typeTableIdx);
		X64OutputInstruction(context, outputFile, MOV, inst.unaryOperation.out, sizedRAX);
	} break;
	case IRINSTRUCTIONTYPE_BITWISE_NOT:
	{
		X64OutputInstruction(context, outputFile, MOV, inst.unaryOperation.out, inst.unaryOperation.in);
		String out = X64IRValueToStr(context, inst.unaryOperation.out);
		PrintOut(context, outputFile, "not %S\n", out);
	} break;
	case IRINSTRUCTIONTYPE_SUBTRACT_UNARY:
	{
		X64OutputInstruction(context, outputFile, MOV, inst.unaryOperation.out, inst.unaryOperation.in);
		String out = X64IRValueToStr(context, inst.unaryOperation.out);
		PrintOut(context, outputFile, "neg %S\n", out);
	} break;
	case IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS:
	{
		X64OutputInstruction(context, outputFile, LEA, inst.unaryOperation.out, inst.unaryOperation.in);
	} break;
	case IRINSTRUCTIONTYPE_CONVERT_INT_TO_F32:
	{
		X64OutputInstruction(context, outputFile, CVTSI2SS, inst.unaryOperation.out, inst.unaryOperation.in);
	} break;
	case IRINSTRUCTIONTYPE_CONVERT_INT_TO_F64:
	{
		X64OutputInstruction(context, outputFile, CVTSI2SD, inst.unaryOperation.out, inst.unaryOperation.in);
	} break;
	case IRINSTRUCTIONTYPE_CONVERT_F32_TO_INT:
	{
		X64OutputInstruction(context, outputFile, CVTSS2SI, inst.unaryOperation.out, inst.unaryOperation.in);
	} break;
	case IRINSTRUCTIONTYPE_CONVERT_F64_TO_INT:
	{
		X64OutputInstruction(context, outputFile, CVTSD2SI, inst.unaryOperation.out, inst.unaryOperation.in);
	} break;
	case IRINSTRUCTIONTYPE_CONVERT_F32_TO_F64:
	{
		X64OutputInstruction(context, outputFile, CVTSS2SD, inst.unaryOperation.out, inst.unaryOperation.in);
	} break;
	case IRINSTRUCTIONTYPE_CONVERT_F64_TO_F32:
	{
		X64OutputInstruction(context, outputFile, CVTSD2SS, inst.unaryOperation.out, inst.unaryOperation.in);
	} break;
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL:
	{
		// At this point, we have the actual values that go into registers/stack slots. If something
		// is passed by copy, we already have the pointer to the copy as argument value, so we don't
		// care.

		// @Incomplete: implement calling conventions other than MS ABI
		for (int i = 0; i < inst.procedureCall.parameters.size; ++i)
		{
			IRValue param = inst.procedureCall.parameters[i];
			switch(i)
			{
			case 0:
				X64OutputInstruction(context, outputFile, MOV, RCX, param);
				break;
			case 1:
				X64OutputInstruction(context, outputFile, MOV, RDX, param);
				break;
			case 2:
				X64OutputInstruction(context, outputFile, MOV, R8, param);
				break;
			case 3:
				X64OutputInstruction(context, outputFile, MOV, R9, param);
				break;
			default:
				X64OutputInstruction(context, outputFile, MOV, IRValueMemory(RSP_idx, i * 8, TYPETABLEIDX_S64), param);
				break;
			}
		}

		String callProcLabel = X64ProcedureToLabel(context, inst.procedureCall.procedure);
		PrintOut(context, outputFile, "call %S\n", callProcLabel);

		if (inst.procedureCall.out.valueType != IRVALUETYPE_INVALID)
		{
			// @Improve: this is weird, just expect result to be at RAX instead of Out value
			X64OutputInstruction(context, outputFile, MOV, inst.procedureCall.out, RAX);
		}
	} break;
	case IRINSTRUCTIONTYPE_RETURN:
	{
	} break;
	case IRINSTRUCTIONTYPE_LABEL:
	{
		PrintOut(context, outputFile, "%S:\n", inst.label);
	} break;
	case IRINSTRUCTIONTYPE_JUMP:
	{
		String label = inst.conditionalJump.label->name;
		PrintOut(context, outputFile, "jmp %S\n", label);
	} break;
	case IRINSTRUCTIONTYPE_JUMP_IF_ZERO:
	{
		String label = inst.conditionalJump.label->name;
		X64OutputInstruction(context, outputFile, CMP, inst.conditionalJump.condition,
				IRValueImmediate(0));
		PrintOut(context, outputFile, "je %S\n", label);
	} break;
	case IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO:
	{
		String label = inst.conditionalJump.label->name;
		String condition = X64IRValueToStr(context, inst.conditionalJump.condition);
		X64OutputInstruction(context, outputFile, CMP, inst.conditionalJump.condition,
				IRValueImmediate(0));
		PrintOut(context, outputFile, "jne %S\n", label);
	} break;
	case IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY:
	{
		String src = X64IRValueToStr(context, inst.memcpy.src);
		String dst = X64IRValueToStr(context, inst.memcpy.dst);
		String size = X64IRValueToStr(context, inst.memcpy.size);
		X64OutputInstruction(context, outputFile, MOV, RCX, inst.memcpy.dst);
		X64OutputInstruction(context, outputFile, MOV, RDX, inst.memcpy.src);
		X64OutputInstruction(context, outputFile, MOV, R8, inst.memcpy.size);
		PrintOut(context, outputFile, "call CopyMemory\n");
	} break;
	case IRINSTRUCTIONTYPE_PATCH:
	{
		X64ProcessInstruction(context, outputFile, *inst.patch.first);
		X64ProcessInstruction(context, outputFile, *inst.patch.second);
	} break;
	case IRINSTRUCTIONTYPE_PATCH_MANY:
	{
		for (int i = 0; i < inst.patchMany.instructions.size; ++i)
			X64ProcessInstruction(context, outputFile, inst.patchMany.instructions[i]);
	} break;
	default:
	{
		ASSERT(!"Didn't recognize instruction type");
		PrintOut(context, outputFile, "???INST\n");
	} break;
	}

	return;
doConditionalSet:
	{
		String left = X64IRValueToStr(context, inst.binaryOperation.left);
		String right = X64IRValueToStr(context, inst.binaryOperation.right);
		String out = X64IRValueToStr(context, inst.binaryOperation.out);

		TypeInfo *leftTypeInfo = &context->typeTable[inst.binaryOperation.left.typeTableIdx];
		if (leftTypeInfo-> typeCategory != TYPECATEGORY_FLOATING)
			X64OutputInstruction(context, outputFile, CMP, inst.binaryOperation.left,
					inst.binaryOperation.right);
		else if (leftTypeInfo->size == 4)
			X64OutputInstruction(context, outputFile, COMISS, inst.binaryOperation.left,
					inst.binaryOperation.right);
		else
			X64OutputInstruction(context, outputFile, COMISD, inst.binaryOperation.left,
					inst.binaryOperation.right);
		PrintOut(context, outputFile, "set%S al\n", conditionCode);
		IRValue sizedRAX = IRValueRegister(RAX_idx, inst.binaryOperation.out.typeTableIdx);
		X64OutputInstruction(context, outputFile, MOV, inst.binaryOperation.out, sizedRAX);
	}
}

void X64ConvertInstruction(Context *context, IRInstruction inst, X64Procedure *x64Proc)
{
	X64Instruction result = {};

	X64FloatingType floatingType = X64FLOATINGTYPE_NONE;
	if (inst.type >= IRINSTRUCTIONTYPE_BINARY_BEGIN &&
		inst.type <  IRINSTRUCTIONTYPE_BINARY_END)
	{
		TypeInfo *typeInfo = &context->typeTable[inst.binaryOperation.out.typeTableIdx];
		if (typeInfo->typeCategory == TYPECATEGORY_FLOATING)
			floatingType = (X64FloatingType)(X64FLOATINGTYPE_F32 + (typeInfo->size == 8));
	}
	else if (inst.type >= IRINSTRUCTIONTYPE_UNARY_BEGIN &&
			 inst.type <  IRINSTRUCTIONTYPE_UNARY_END)
	{
		TypeInfo *typeInfo = &context->typeTable[inst.unaryOperation.out.typeTableIdx];
		if (typeInfo->typeCategory == TYPECATEGORY_FLOATING)
			floatingType = (X64FloatingType)(X64FLOATINGTYPE_F32 + (typeInfo->size == 8));
	}
	else if (inst.type == IRINSTRUCTIONTYPE_ASSIGNMENT)
	{
		TypeInfo *typeInfo = &context->typeTable[inst.assignment.dst.typeTableIdx];
		if (typeInfo->typeCategory == TYPECATEGORY_FLOATING)
			floatingType = (X64FloatingType)(X64FLOATINGTYPE_F32 + (typeInfo->size == 8));
	}

	switch (inst.type)
	{
	case IRINSTRUCTIONTYPE_PATCH:
	{
		X64ConvertInstruction(context, *inst.patch.first,  x64Proc);
		X64ConvertInstruction(context, *inst.patch.second, x64Proc);
	} break;
	case IRINSTRUCTIONTYPE_PATCH_MANY:
	{
		const s64 instructionCount = inst.patchMany.instructions.size;
		for (s64 i = 0; i < instructionCount; ++i)
			X64ConvertInstruction(context, inst.patchMany.instructions[i], x64Proc);
	} break;
	case IRINSTRUCTIONTYPE_ASSIGNMENT:
	{
		switch (floatingType)
		{
		case X64FLOATINGTYPE_NONE:
			result.type = X64_MOV;
			goto doMov;
		case X64FLOATINGTYPE_F32:
			result.type = X64_MOVSS;
			goto doMov;
		case X64FLOATINGTYPE_F64:
			result.type = X64_MOVSD;
			goto doMov;
		}
	}
	case IRINSTRUCTIONTYPE_ASSIGNMENT_ZERO_EXTEND:
	{
		result.type = X64_MOVZX;
		goto doMov;
	}
	case IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS:
		result.type = X64_LEA;
		goto doMov;
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
		switch (floatingType)
		{
		case X64FLOATINGTYPE_NONE:
			result.type = X64_NEG;
			goto doRM;
		case X64FLOATINGTYPE_F32:
			result.type = X64_XORPS;
			result.src = IRValueImmediate(0x80000000);
			break;
		case X64FLOATINGTYPE_F64:
			result.type = X64_XORPD;
			result.src = IRValueImmediate(0x8000000000000000);
			break;
		}
		result.dst = inst.unaryOperation.out;
		*BucketArrayAdd(&x64Proc->instructions) = result;
	case IRINSTRUCTIONTYPE_MULTIPLY:
		switch (floatingType)
		{
		case X64FLOATINGTYPE_NONE:
			result.type = X64_IMUL;
			goto doRM_RMI;
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
			*BucketArrayAdd(&x64Proc->instructions) = { X64_MOV, RAX, inst.binaryOperation.left };
			*BucketArrayAdd(&x64Proc->instructions) = { X64_CQO };
			result.type = X64_IDIV;
			result.dst = inst.binaryOperation.right;
			*BucketArrayAdd(&x64Proc->instructions) = result;
			*BucketArrayAdd(&x64Proc->instructions) = { X64_MOV, inst.binaryOperation.out, RAX };
			return;
		case X64FLOATINGTYPE_F32:
			result.type = X64_DIVSS;
			goto doX_XM;
		case X64FLOATINGTYPE_F64:
			result.type = X64_DIVSD;
			goto doX_XM;
		}
	case IRINSTRUCTIONTYPE_MODULO:
		*BucketArrayAdd(&x64Proc->instructions) = { X64_MOV, RAX, inst.binaryOperation.left };
		*BucketArrayAdd(&x64Proc->instructions) = { X64_CQO };
		result.type = X64_IDIV;
		result.dst = inst.binaryOperation.right;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		*BucketArrayAdd(&x64Proc->instructions) = { X64_MOV, inst.binaryOperation.out, RDX };
		return;
	case IRINSTRUCTIONTYPE_SHIFT_LEFT:
		result.type = X64_SAL;
		goto doRM_RMI;
	case IRINSTRUCTIONTYPE_SHIFT_RIGHT:
		result.type = X64_SAR;
		goto doRM_RMI;
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
	{
		X64Instruction cmpInst = { X64_CMP };
		cmpInst.dst = inst.conditionalJump.condition;
		cmpInst.src = IRValueImmediate(0);
		result.type = X64_JNE;
		result.label = inst.conditionalJump.label;

		*BucketArrayAdd(&x64Proc->instructions) = cmpInst;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	}
	case IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO:
	{
		X64Instruction cmpInst = { X64_CMP };
		cmpInst.dst = inst.conditionalJump.condition;
		cmpInst.src = IRValueImmediate(0);
		result.type = X64_JNE;
		result.label = inst.conditionalJump.label;

		*BucketArrayAdd(&x64Proc->instructions) = cmpInst;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	}
	case IRINSTRUCTIONTYPE_GREATER_THAN:
	{
		result.type = X64_SETG;
		goto doConditionalSet;
	} break;
	case IRINSTRUCTIONTYPE_LESS_THAN:
	{
		result.type = X64_SETL;
		goto doConditionalSet;
	} break;
	case IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS:
	{
		result.type = X64_SETGE;
		goto doConditionalSet;
	} break;
	case IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS:
	{
		result.type = X64_SETLE;
		goto doConditionalSet;
	} break;
	case IRINSTRUCTIONTYPE_EQUALS:
	{
		result.type = X64_SETE;
		goto doConditionalSet;
	} break;
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL:
	{
		// At this point, we have the actual values that go into registers/stack slots. If something
		// is passed by copy, we already have the pointer to the copy as argument value, so we don't
		// care.

		// @Incomplete: implement calling conventions other than MS ABI
		for (int i = 0; i < inst.procedureCall.parameters.size; ++i)
		{
			X64Instruction paramInst = { X64_MOV };
			paramInst.src= inst.procedureCall.parameters[i];
			switch(i)
			{
			case 0:
				paramInst.dst = RCX;
				break;
			case 1:
				paramInst.dst = RDX;
				break;
			case 2:
				paramInst.dst = R8;
				break;
			case 3:
				paramInst.dst = R9;
				break;
			default:
				paramInst.dst = IRValueMemory(RSP_idx, i * 8, TYPETABLEIDX_S64);
			}
			*BucketArrayAdd(&x64Proc->instructions) = paramInst;
		}

		result.type = X64_CALL;
		String callProcLabel = X64ProcedureToLabel(context, inst.procedureCall.procedure);
		result.procLabel = callProcLabel;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	}
	case IRINSTRUCTIONTYPE_GET_PARAMETER:
	{
		IRValue value;
		switch (inst.getParameter.parameterIdx)
		{
			case 0: value = RCX; break;
			case 1: value = RDX; break;
			case 2: value = R8;  break;
			case 3: value = R9;  break;
							// Add 16, 8 for return address, and 8 because we push RBP
			default: value = IRValueMemory(RBP_idx, 16 + inst.getParameter.parameterIdx * 8, TYPETABLEIDX_S64);
		}
		*BucketArrayAdd(&x64Proc->instructions) = { X64_MOV, inst.getParameter.dst, value };
		return;
	}
	case IRINSTRUCTIONTYPE_RETURN:
		*BucketArrayAdd(&x64Proc->instructions) = { X64_LEAVE };
		*BucketArrayAdd(&x64Proc->instructions) = { X64_RET };
		return;
	case IRINSTRUCTIONTYPE_GET_TYPE_INFO:
	{
		Variable *fakeVar = NewVariable(context,
				TPrintF("_typeInfo%lld", inst.getTypeInfo.typeTableIdx));
		fakeVar->isStatic = true;
		IRValue typeInfoVar = IRValueFromVariable(context, fakeVar);
		*BucketArrayAdd(&x64Proc->instructions) = { X64_LEA, inst.getTypeInfo.out, typeInfoVar };
		return;
	}
	case IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY:
	{
		*BucketArrayAdd(&x64Proc->instructions) = { X64_MOV, RCX, inst.memcpy.dst };
		*BucketArrayAdd(&x64Proc->instructions) = { X64_MOV, RDX, inst.memcpy.src };
		*BucketArrayAdd(&x64Proc->instructions) = { X64_MOV, R8, inst.memcpy.size };
		result.type = X64_CALL;
		result.procLabel = "CopyMemory"_s;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	}
	case IRINSTRUCTIONTYPE_CONVERT_INT_TO_F32:
		result.type = X64_CVTSI2SS;
		goto doCvt;
	case IRINSTRUCTIONTYPE_CONVERT_INT_TO_F64:
		result.type = X64_CVTSI2SD;
		goto doCvt;
	case IRINSTRUCTIONTYPE_CONVERT_F32_TO_INT:
		result.type = X64_CVTSS2SI;
		goto doCvt;
	case IRINSTRUCTIONTYPE_CONVERT_F64_TO_INT:
		result.type = X64_CVTSD2SI;
		goto doCvt;
	case IRINSTRUCTIONTYPE_CONVERT_F32_TO_F64:
		result.type = X64_CVTSS2SD;
		goto doCvt;
	case IRINSTRUCTIONTYPE_CONVERT_F64_TO_F32:
		result.type = X64_CVTSD2SS;
		goto doCvt;
	case IRINSTRUCTIONTYPE_PUSH_SCOPE:
		result.type = X64_Push_Scope;
		return;
	case IRINSTRUCTIONTYPE_POP_SCOPE:
		result.type = X64_Pop_Scope;
		return;
	case IRINSTRUCTIONTYPE_PUSH_VARIABLE:
		result.type = X64_Push_Variable;
		return;
	case IRINSTRUCTIONTYPE_COMMENT:
		return;
	default:
		ASSERT(!"Unrecognized IR instruction type");
		return;
	}

doMov:
	{
		result.dst = inst.assignment.dst;
		result.src = inst.assignment.src;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	}
doRM:
	{
		X64Instruction movInst = { X64_MOV };
		movInst.dst = inst.unaryOperation.out;
		movInst.src = inst.unaryOperation.in;

		result.dst = movInst.dst;

		*BucketArrayAdd(&x64Proc->instructions) = movInst;
		*BucketArrayAdd(&x64Proc->instructions) = result;

		return;
	}
doRM_RMI:
	{
		IRValue left  = inst.binaryOperation.left;
		IRValue right = inst.binaryOperation.right;
		IRValue out   = inst.binaryOperation.out;

		IRValue tmp = IRValueRegister(x64Proc->virtualRegisterCount++, left.typeTableIdx);

		X64Instruction movInInst = { X64_MOV, tmp, left };

		result.dst = tmp;
		result.src = right;

		X64Instruction movOutInst = { X64_MOV, out, tmp };

		*BucketArrayAdd(&x64Proc->instructions) = movInInst;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		*BucketArrayAdd(&x64Proc->instructions) = movOutInst;

		return;
	}
doX_XM:
	{
		IRValue left  = inst.binaryOperation.left;
		IRValue right = inst.binaryOperation.right;
		IRValue out = inst.binaryOperation.out;

		IRValue tmp = IRValueRegister(x64Proc->virtualRegisterCount++, left.typeTableIdx);

		X64Instruction movInInst;
		if (floatingType == X64FLOATINGTYPE_F32)
			movInInst = { X64_MOVSS };
		else
			movInInst = { X64_MOVSD };
		movInInst.dst = tmp;
		movInInst.src = left;

		result.dst = tmp;
		result.src = right;

		X64Instruction movOutInst;
		if (floatingType == X64FLOATINGTYPE_F32)
			movOutInst = { X64_MOVSS };
		else
			movOutInst = { X64_MOVSD };
		movOutInst.dst = out;
		movOutInst.src = tmp;

		*BucketArrayAdd(&x64Proc->instructions) = movInInst;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		*BucketArrayAdd(&x64Proc->instructions) = movOutInst;

		return;
	}
doCvt:
	{
		result.dst = inst.unaryOperation.out;
		result.src = inst.unaryOperation.in;
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
		}
		cmpInst.dst = inst.binaryOperation.left;
		cmpInst.src = inst.binaryOperation.right;
		IRValue sizedRAX = IRValueRegister(RAX_idx, inst.binaryOperation.out.typeTableIdx);

		X64Instruction movInst = { X64_MOV, inst.binaryOperation.out, sizedRAX };

		*BucketArrayAdd(&x64Proc->instructions) = cmpInst;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		*BucketArrayAdd(&x64Proc->instructions) = movInst;
	}
}

s64 X64PrintInstruction(Context *context, X64Instruction inst)
{
	s64 result = 0;
	switch (inst.type)
	{
	case X64_MOV:
		result = Print("mov ");
		goto printDstSrc;
	case X64_MOVZX:
		result = Print("movzx ");
		goto printDstSrc;
	case X64_CQO:
		return Print("cqo");
	case X64_LEA:
		result = Print("lea ");
		goto printDstSrc;
	case X64_PUSH:
		result = Print("push ");
		goto printDst;
	case X64_POP:
		result = Print("pop ");
		goto printDst;
	case X64_ADD:
		result = Print("add ");
		goto printDstSrc;
	case X64_SUB:
		result = Print("sub ");
		goto printDstSrc;
	case X64_NEG:
		result = Print("neg ");
		goto printDst;
	case X64_IMUL:
		result = Print("imul ");
		goto printDstSrc;
	case X64_IDIV:
		result = Print("idiv ");
		goto printDst;
	case X64_SAL:
		result = Print("sal ");
		goto printDstSrc;
	case X64_SAR:
		result = Print("sar ");
		goto printDstSrc;
	case X64_CMP:
		result = Print("cmp ");
		goto printDstSrc;
	case X64_JMP:
		result = Print("jmp ");
		goto printLabel;
	case X64_JE:
		result = Print("je ");
		goto printLabel;
	case X64_JNE:
		result = Print("jne ");
		goto printLabel;
	case X64_CALL:
		return Print("call %S", inst.procLabel);
	case X64_LEAVE:
		return Print("leave");
	case X64_RET:
		return Print("ret");
	case X64_SETG:
		result = Print("setg ");
		goto printDst;
	case X64_SETL:
		result = Print("setl ");
		goto printDst;
	case X64_SETGE:
		result = Print("setge ");
		goto printDst;
	case X64_SETLE:
		result = Print("setle ");
		goto printDst;
	case X64_SETE:
		result = Print("sete ");
		goto printDst;
	case X64_MOVSS:
		result = Print("movss ");
		goto printDstSrc;
	case X64_MOVSD:
		result = Print("movsd ");
		goto printDstSrc;
	case X64_ADDSS:
		result = Print("addss ");
		goto printDstSrc;
	case X64_ADDSD:
		result = Print("addsd ");
		goto printDstSrc;
	case X64_SUBSS:
		result = Print("subss ");
		goto printDstSrc;
	case X64_SUBSD:
		result = Print("subsd ");
		goto printDstSrc;
	case X64_MULSS:
		result = Print("mulss ");
		goto printDstSrc;
	case X64_MULSD:
		result = Print("mulsd ");
		goto printDstSrc;
	case X64_DIVSS:
		result = Print("divss ");
		goto printDstSrc;
	case X64_DIVSD:
		result = Print("divsd ");
		goto printDstSrc;
	case X64_CVTSI2SS:
		result = Print("cvtsi2ss ");
		goto printDstSrc;
	case X64_CVTSI2SD:
		result = Print("cvtsi2sd ");
		goto printDstSrc;
	case X64_CVTSS2SI:
		result = Print("cvtss2si ");
		goto printDstSrc;
	case X64_CVTSD2SI:
		result = Print("cvtsd2si ");
		goto printDstSrc;
	case X64_CVTSS2SD:
		result = Print("cvtss2sd ");
		goto printDstSrc;
	case X64_CVTSD2SS:
		result = Print("cvtsd2ss ");
		goto printDstSrc;
	case X64_Label:
		return Print("%S:", inst.label->name);
	case X64_Push_Scope:
	case X64_Pop_Scope:
	case X64_Push_Variable:
		return 0;
	case X64_Patch:
		result = X64PrintInstruction(context, *inst.patch1);
		result += Print("\n");
		result += X64PrintInstruction(context, *inst.patch2);
		return result;
	case X64_Patch_Many:
		result = 0;
		for (int i = 0; i < inst.patchInstructions.size; ++i)
		{
			if (i) result = Print("\n");
			result += X64PrintInstruction(context, inst.patchInstructions[i]);
		}
		return result;
	default:
		ASSERT(!"Unrecognized x64 instruction type");
	}

printDst:
	{
		String dst = X64IRValueToStr(context, inst.dst);
		result += Print("%S", dst);
		return result;
	}
printDstSrc:
	{
		String dst = X64IRValueToStr(context, inst.dst);
		String src = X64IRValueToStr(context, inst.src);
		result += Print("%S, %S", dst, src);
		return result;
	}
printLabel:
	{
		result += Print("%S", inst.label->name);
		return result;
	}
}

#include "X64RegisterAllocation.cpp"

void X64PrintInstructions(Context *context, Array<X64Procedure> x64Procedures)
{
	for (int procedureIdx = 0; procedureIdx < x64Procedures.size; ++procedureIdx)
	{
		// @Cleanup
		if (context->procedures[procedureIdx].isExternal)
			continue;

		X64Procedure x64Proc = x64Procedures[procedureIdx];

		Print("\n%S PROC\n", x64Proc.name);

		u64 instructionCount = BucketArrayCount(&x64Proc.instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			X64PrintInstruction(context, x64Proc.instructions[instructionIdx]);
			Print("\n");
		}

		Print("%S ENDP\n", x64Proc.name);
	}
}

void BackendConvert(Context *context)
{
	BucketArrayInit(&context->patchedInstructions);

	Array<X64Procedure> x64Procedures;
	u64 procedureCount = BucketArrayCount(&context->procedures);
	ArrayInit(&x64Procedures, procedureCount, malloc);
	x64Procedures.size = procedureCount;

	for (int procedureIdx = 0; procedureIdx < procedureCount; ++procedureIdx)
	{
		Procedure *proc = &context->procedures[procedureIdx];
		X64Procedure *x64Proc = &x64Procedures[procedureIdx];

		x64Proc->name = X64ProcedureToLabel(context, proc);
		x64Proc->virtualRegisterCount = proc->registerCount;
		x64Proc->allocatedParameterCount = proc->allocatedParameterCount;
		memset(x64Proc->noSpillRegisters, 0, sizeof(x64Proc->noSpillRegisters));
		DynamicArrayInit(&x64Proc->spillVariables, 8);

		// @Speed: separate array of external procedures to avoid branching
		if (proc->isExternal)
			continue;

		BucketArrayInit(&x64Proc->instructions);
		*BucketArrayAdd(&x64Proc->instructions) = { X64_PUSH, RBP };
		*BucketArrayAdd(&x64Proc->instructions) = { X64_MOV, RBP, RSP };

		u64 instructionCount = BucketArrayCount(&proc->instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			IRInstruction inst = proc->instructions[instructionIdx];
			X64ConvertInstruction(context, inst, x64Proc);
		}
	}

	X64AllocateRegisters(context, x64Procedures);

	X64PrintInstructions(context, x64Procedures);
}

void BackendMain(Context *context)
{
	HANDLE outputFile = CreateFileA(
			"out.asm",
			GENERIC_WRITE,
			0,
			nullptr,
			CREATE_ALWAYS,
			FILE_ATTRIBUTE_NORMAL,
			nullptr
			);
	context->outputFile = outputFile;

	PrintOut(context, outputFile, "include basic.asm\n\n");

	PrintOut(context, outputFile, "_DATA SEGMENT\n");

	// TypeInfo data
	{
		u64 tableSize = BucketArrayCount(&context->typeTable);
		for (u64 typeTableIdx = 0; typeTableIdx < tableSize; ++typeTableIdx)
		{
			TypeInfo *typeInfo = &context->typeTable[typeTableIdx];
			switch (typeInfo->typeCategory)
			{
			case TYPECATEGORY_INTEGER:
			{
				PrintOut(context, outputFile, "_typeInfo%lld DB 0\n\tDQ %lld\n\tDW %d\n",
						typeTableIdx, typeInfo->size, typeInfo->integerInfo.isSigned);
			} break;
			case TYPECATEGORY_FLOATING:
			{
				PrintOut(context, outputFile, "_typeInfo%lld DB 1\n\tDQ %lld\n",
						typeTableIdx, typeInfo->size);
			} break;
			case TYPECATEGORY_STRUCT:
			{
				String structName = "<anonymous struct>"_s;
				StaticDefinition *staticDefStruct = FindStaticDefinitionByTypeTableIdx(context,
						typeTableIdx);
				if (staticDefStruct)
					structName = staticDefStruct->name;

				// Member name strings
				for (s64 memberIdx = 0; memberIdx < (s64)typeInfo->structInfo.members.size; ++memberIdx)
				{
					StructMember member = typeInfo->structInfo.members[memberIdx];
					PrintOut(context, outputFile, "_memberName%lld_%lld DB '%S'\n",
							typeTableIdx, memberIdx, member.name);
				}

				PrintOut(context, outputFile, "_memberInfos%lld DQ ", typeTableIdx);
				for (s64 memberIdx = 0; memberIdx < (s64)typeInfo->structInfo.members.size; ++memberIdx)
				{
					StructMember member = typeInfo->structInfo.members[memberIdx];
					if (memberIdx) PrintOut(context, outputFile, ", ");
					PrintOut(context, outputFile, "%lld, _memberName%lld_%lld, _typeInfo%lld, %llxH",
							member.name.size, typeTableIdx, memberIdx, member.typeTableIdx, member.offset);
				}
				PrintOut(context, outputFile, "\n");

				PrintOut(context, outputFile, "_structName%lld DB '%S'\n", typeTableIdx, structName);
				PrintOut(context, outputFile, "_typeInfo%lld DB 2\n\tDQ %lld, %lld, _structName%lld\n"
						"\tDW %d\n\tDQ %lld, _memberInfos%lld\n",
						typeTableIdx, typeInfo->size, structName.size, typeTableIdx,
						(s32)typeInfo->structInfo.isUnion, typeInfo->structInfo.members.size,
						typeTableIdx);
			} break;
			case TYPECATEGORY_ENUM:
			{
				String enumName = "<anonymous enum>"_s;
				StaticDefinition *staticDefStruct = FindStaticDefinitionByTypeTableIdx(context,
						typeTableIdx);
				if (staticDefStruct)
					enumName = staticDefStruct->name;

				PrintOut(context, outputFile, "_enumName%lld DB '%S'\n", typeTableIdx, enumName);
				PrintOut(context, outputFile, "_typeInfo%lld DB 3\n"
						"\tDQ %lld, %lld, _enumName%lld, _typeInfo%lld\n",
						typeTableIdx, typeInfo->size, enumName.size, typeTableIdx,
						typeInfo->enumInfo.typeTableIdx);
			} break;
			case TYPECATEGORY_POINTER:
			{
				PrintOut(context, outputFile, "_typeInfo%lld DB 4\n\tDQ %lld, _typeInfo%lld\n",
						typeTableIdx, typeInfo->size, typeInfo->pointerInfo.pointedTypeTableIdx);
			} break;
			case TYPECATEGORY_ARRAY:
			{
				PrintOut(context, outputFile, "_typeInfo%lld DB 5\n\tDQ %llu, %llu, _typeInfo%lld\n",
						typeTableIdx, typeInfo->size, typeInfo->arrayInfo.count,
						typeInfo->arrayInfo.elementTypeTableIdx);
			} break;
			case TYPECATEGORY_INVALID:
			{
				PrintOut(context, outputFile, "_typeInfo%lld DB 6\n", typeTableIdx);
			} break;
			}
		}
	}

	const u64 staticVariableCount = context->irStaticVariables.size;
	for (int staticVariableIdx = 0; staticVariableIdx < staticVariableCount; ++staticVariableIdx)
	{
		IRStaticVariable staticVar = context->irStaticVariables[staticVariableIdx];

		TypeInfo *typeInfo  = &context->typeTable[staticVar.variable->typeTableIdx];

		if (staticVar.initialValue.valueType == IRVALUETYPE_IMMEDIATE_STRING)
		{
			PrintOut(context, outputFile, "_str_%S DB ", staticVar.variable->name);

			u64 size = staticVar.initialValue.immediateString.size;
			bool first = true;
			u8 *buffer = (u8 *)g_memory->framePtr;
			u8 *out = buffer;
			const u8 *in = (const u8 *)staticVar.initialValue.immediateString.data;
			for (int i = 0; i < staticVar.initialValue.immediateString.size; ++i)
			{
				if (*in == '\\')
				{
					if (!first) PrintOut(context, outputFile, ", ");

					++in;
					switch (*in)
					{
					case 'n':
						PrintOut(context, outputFile, "0AH");
						break;
					case '0':
						PrintOut(context, outputFile, "00H");
						break;
					}
					++in;
					++i;
					--size; // Don't count backslash for string size.
					first = false;
				}
				else
				{
					*out++ = *in++;

					if (i == staticVar.initialValue.immediateString.size - 1 || *in == '\\')
					{
						if (!first) PrintOut(context, outputFile, ", ");

						*out++ = 0;
						g_memory->framePtr = out;
						PrintOut(context, outputFile, "'%s'", buffer);
						out = buffer;

						first = false;
					}
				}
			}
			PrintOut(context, outputFile, "\n");
			g_memory->framePtr = buffer;

			PrintOut(context, outputFile, "%S DQ %.16llxH, _str_%S\n", staticVar.variable->name,
					size, staticVar.variable->name);
		}
		else if (staticVar.initialValue.valueType == IRVALUETYPE_IMMEDIATE_FLOAT)
		{
			switch (typeInfo->size)
			{
			case 4:
			{
				union { u32 asU32; f32 asF32; };
				asF32 = (f32)staticVar.initialValue.immediateFloat;
				PrintOut(context, outputFile, "%S DD %.8xH\n", staticVar.variable->name, asU32);
			} break;
			case 8:
				PrintOut(context, outputFile, "%S DQ %.16llxH\n", staticVar.variable->name,
						staticVar.initialValue.immediate);
				break;
			default:
				ASSERT(!"Invalid immediate size");
			}
		}
		else if (staticVar.initialValue.valueType != IRVALUETYPE_INVALID)
		{
			switch (typeInfo->size)
			{
			case 1:
				PrintOut(context, outputFile, "%S DB %.2llxH\n", staticVar.variable->name,
						staticVar.initialValue.immediate);
				break;
			case 2:
				PrintOut(context, outputFile, "%S DW %.4llxH\n", staticVar.variable->name,
						staticVar.initialValue.immediate);
				break;
			case 4:
				PrintOut(context, outputFile, "%S DD %.8llxH\n", staticVar.variable->name,
						staticVar.initialValue.immediate);
				break;
			case 8:
				PrintOut(context, outputFile, "%S DQ %.16llxH\n", staticVar.variable->name,
						staticVar.initialValue.immediate);
				break;
			default:
				ASSERT(!"Invalid immediate size");
			}
		}
		else
		{
			PrintOut(context, outputFile, "COMM %S:BYTE:%llxH\n", staticVar.variable->name,
					typeInfo->size);
		}
	}

	PrintOut(context, outputFile, "_DATA ENDS\n");

	u64 procedureCount = BucketArrayCount(&context->procedures);
	for (int procedureIdx = 0; procedureIdx < procedureCount; ++procedureIdx)
	{
		Procedure *proc = &context->procedures[procedureIdx];

		// @Speed: separate array of external procedures to avoid branching
		if (!proc->isExternal)
		{
			StaticDefinition *staticDef = FindStaticDefinitionByProcedure(context, proc);
			if (staticDef)
				PrintOut(context, outputFile, "PUBLIC %S\n", staticDef->name);
		}
		else
		{
			StaticDefinition *staticDef = FindStaticDefinitionByProcedure(context, proc);
			if (staticDef)
				PrintOut(context, outputFile, "EXTRN %S:proc\n", staticDef->name);
		}
	}

	PrintOut(context, outputFile, "_TEXT SEGMENT\n");

	for (int procedureIdx = 0; procedureIdx < procedureCount; ++procedureIdx)
	{
		Procedure *proc = &context->procedures[procedureIdx];

		// @Speed: separate array of external procedures to avoid branching
		if (proc->isExternal)
			continue;

		String procLabel = X64ProcedureToLabel(context, proc);
		PrintOut(context, outputFile, "\n%S PROC\n", procLabel);

		PrintOut(context, outputFile, "push rbp\n");
		PrintOut(context, outputFile, "mov rbp, rsp\n");

		if (proc->stackSize)
			PrintOut(context, outputFile, "sub rsp, %llu\n", proc->stackSize);

		u64 instructionCount = BucketArrayCount(&proc->instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			IRInstruction inst = proc->instructions[instructionIdx];
			X64ProcessInstruction(context, outputFile, inst);
		}

		PrintOut(context, outputFile, "leave\n");
		PrintOut(context, outputFile, "ret\n");

		PrintOut(context, outputFile, "%S ENDP\n", procLabel);
	}

	PrintOut(context, outputFile, "_TEXT ENDS\n");
	PrintOut(context, outputFile, "END\n");

	CloseHandle(outputFile);
}
