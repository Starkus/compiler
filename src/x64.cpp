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

s32 x64ScratchRegisters[] = {
	RAX_idx, RBX_idx, RCX_idx, RDX_idx,
	RSI_idx, RDI_idx, R8_idx,  R9_idx,
	R10_idx, R11_idx, R12_idx, R13_idx,
	R14_idx, R15_idx };

u32 x64ParameterValuesWrite[32];
u32 x64ParameterValuesRead[32];

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

#if PRINT_ASM_OUTPUT
	if (!context->config.silent)
	{
		OutputDebugStringA(buffer);
		DWORD bytesWritten;
		WriteFile(g_hStdout, buffer, (DWORD)size, &bytesWritten, nullptr); // Stdout
	}
#endif

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

	if (value.valueType == IRVALUETYPE_PROCEDURE)
	{
		result = TPrintF("%S", context->procedures[value.procedureIdx].name);
		return result;
	}

	u64 size = 0;
	TypeInfo typeInfo = context->typeTable[value.typeTableIdx];
	size = typeInfo.size;
	Value v = context->values[value.value.valueIdx];

	s64 offset = 0;
	if (value.valueType == IRVALUETYPE_VALUE || value.valueType == IRVALUETYPE_VALUE_DEREFERENCE)
		offset = value.value.offset;

	if (v.flags & (VALUEFLAGS_ON_STATIC_STORAGE | VALUEFLAGS_IS_EXTERNAL))
	{
		result = v.name;
		if (offset > 0)
			result = TPrintF("%S+0%xh", result, offset);
		else if (offset < 0)
			result = TPrintF("%S-0%xh", result, -offset);
		goto decoratePtr;
	}

	bool isXMM = typeInfo.size > 8 || typeInfo.typeCategory == TYPECATEGORY_FLOATING;

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
					ASSERT(!"Value assigned invalid register!");
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
					ASSERT(!"Value assigned invalid register!");
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
					ASSERT(!"Value assigned invalid register!");
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
					ASSERT(!"Value assigned invalid register!");
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
					ASSERT(!"Value assigned invalid register!");
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
			ASSERT(!"Value assigned invalid register!");
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
	}
	return result;
}

bool IsValueInMemory(Context *context, IRValue irValue)
{
	if (irValue.valueType == IRVALUETYPE_VALUE_DEREFERENCE)
		return true;
	if (irValue.valueType != IRVALUETYPE_VALUE)
		return false;
	Value value = context->values[irValue.value.valueIdx];
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
	bool isImmediate = value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER ||
					   value.valueType == IRVALUETYPE_IMMEDIATE_FLOAT;
	if (isImmediate)
		return false;
	if (context->values[value.value.valueIdx].flags & VALUEFLAGS_FORCE_REGISTER)
		return false;
	if ((context->values[value.value.valueIdx].flags & (VALUEFLAGS_IS_ALLOCATED |
			VALUEFLAGS_IS_MEMORY)) == VALUEFLAGS_IS_ALLOCATED)
		return false;
	return true;
}

void X64Mov(Context *context, X64Procedure *x64Proc, IRValue dst, IRValue src);
void X64MovNoTmp(Context *context, X64Procedure *x64Proc, IRValue dst, IRValue src)
{
	X64Instruction result;
	TypeInfo dstType = context->typeTable[dst.typeTableIdx];
	TypeInfo srcType = context->typeTable[src.typeTableIdx];

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
				if (isSigned && dstType.size > 4)
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
		else if (srcType.size == 4)
			result.type = X64_CVTTSS2SI;
		else
		{
			ASSERT(srcType.size == 8);
			result.type = X64_CVTTSD2SI;
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
		u32 srcUsedFlag = context->values[src.value.valueIdx].flags & VALUEFLAGS_IS_USED;
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

	if (dst.valueType == IRVALUETYPE_VALUE_DEREFERENCE &&
			context->typeTable[dst.typeTableIdx].typeCategory == TYPECATEGORY_POINTER)
	{
		X64Instruction ins = { X64_Comment };
		ins.comment = "asdasd"_s;
		*BucketArrayAdd(&x64Proc->instructions) = ins;
	}

	X64MovNoTmp(context, x64Proc, dst, src);
}

void X64Test(Context *context, X64Procedure *x64Proc, IRValue value)
{
	X64FloatingType floatingType = X64FLOATINGTYPE_NONE;
	TypeInfo typeInfo = context->typeTable[value.typeTableIdx];
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

void X64ConvertInstruction(Context *context, IRInstruction inst, X64Procedure *x64Proc)
{
	X64Instruction result = {};

	X64FloatingType floatingType = X64FLOATINGTYPE_NONE;
	bool isSigned = false;
	{
		TypeInfo typeInfo = context->typeTable[TYPETABLEIDX_S64];
		if      (inst.type >= IRINSTRUCTIONTYPE_COMPARE_BEGIN &&
				 inst.type <  IRINSTRUCTIONTYPE_COMPARE_END)
			typeInfo = context->typeTable[inst.binaryOperation.left.typeTableIdx];
		else if (inst.type >= IRINSTRUCTIONTYPE_BINARY_BEGIN &&
				 inst.type <  IRINSTRUCTIONTYPE_BINARY_END)
			typeInfo = context->typeTable[inst.binaryOperation.left.typeTableIdx];
		else if (inst.type >= IRINSTRUCTIONTYPE_UNARY_BEGIN &&
				 inst.type <  IRINSTRUCTIONTYPE_UNARY_END)
			typeInfo = context->typeTable[inst.unaryOperation.in.typeTableIdx];
		else if (inst.type >= IRINSTRUCTIONTYPE_COMPARE_JUMP_BEGIN &&
				 inst.type <  IRINSTRUCTIONTYPE_COMPARE_JUMP_END)
			typeInfo = context->typeTable[inst.conditionalJump2.left.typeTableIdx];
		else if (inst.type == IRINSTRUCTIONTYPE_ASSIGNMENT)
			typeInfo = context->typeTable[inst.assignment.dst.typeTableIdx];

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
		static s64 voidPtrTypeIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_VOID);

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
			break;
		case X64FLOATINGTYPE_F64:
			result.type = X64_XORPD;
			result.src = IRValueImmediateFloat(context, -0.0);
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
					IsPowerOf2(left.immediate))
			{
				IRValue tmp = left;
				left = right;
				right = tmp;
			}

			if (right.valueType == IRVALUETYPE_IMMEDIATE_INTEGER && right.immediate > 0 &&
					IsPowerOf2(right.immediate))
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

			if (right.valueType == IRVALUETYPE_IMMEDIATE_INTEGER && IsPowerOf2(right.immediate))
			{
				u32 immitateFlag = left.valueType == IRVALUETYPE_VALUE ? VALUEFLAGS_TRY_IMMITATE : 0;
				IRValue tmp = IRValueNewValue(context, "_mulshfttmp"_s, left.typeTableIdx,
						immitateFlag, left.value.valueIdx);

				TypeInfo leftType = context->typeTable[left.typeTableIdx];
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

		if (right.valueType == IRVALUETYPE_IMMEDIATE_INTEGER && IsPowerOf2(right.immediate))
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
		TypeInfo leftType = context->typeTable[inst.binaryOperation.left.typeTableIdx];
		result.type = isSigned ? X64_SAL : X64_SHL;
		goto doShift;
	}
	case IRINSTRUCTIONTYPE_SHIFT_RIGHT:
	{
		TypeInfo leftType = context->typeTable[inst.binaryOperation.left.typeTableIdx];
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
		if (context->typeTable[result.dst.typeTableIdx].size != 1)
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
		// At this point, we have the actual values that go into registers/stack slots. If something
		// is passed by copy, we already have the pointer to the copy as argument value, so we don't
		// care.
		if (inst.type == IRINSTRUCTIONTYPE_PROCEDURE_CALL)
		{
			result.type = X64_CALL;
			result.procedureIdx = inst.procedureCall.procedureIdx;
		}
		else
		{
			result.type = X64_CALL_Indirect;
			result.procedureIRValue = inst.procedureCall.procIRValue;
		}

		// @Incomplete: implement calling conventions other than MS ABI
		for (int i = 0; i < inst.procedureCall.parameters.size; ++i)
		{
			IRValue param = inst.procedureCall.parameters[i];
			s64 paramType = param.typeTableIdx;
			bool isXMM = context->typeTable[paramType].typeCategory == TYPECATEGORY_FLOATING;

			IRValue dst;
			switch(i)
			{
			case 0:
				dst = isXMM ? XMM0 : RCX;
				break;
			case 1:
				dst = isXMM ? XMM1 : RDX;
				break;
			case 2:
				dst = isXMM ? XMM2 : R8;
				break;
			case 3:
				dst = isXMM ? XMM3 : R9;
				break;
			default:
				dst = IRValueValue(x64ParameterValuesWrite[i], TYPETABLEIDX_S64);
			}
			dst.typeTableIdx = paramType;
			X64Mov(context, x64Proc, dst, param);
		}

		*BucketArrayAdd(&x64Proc->instructions) = result;

		if (inst.procedureCall.out.valueType != IRVALUETYPE_INVALID)
		{
			s64 returnTypeIdx = inst.procedureCall.out.typeTableIdx;
			if (context->typeTable[returnTypeIdx].typeCategory == TYPECATEGORY_FLOATING)
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
		ASSERT(inst.copyMemory.dst.valueType == IRVALUETYPE_VALUE ||
			   inst.copyMemory.dst.valueType == IRVALUETYPE_VALUE_DEREFERENCE);
		ASSERT(inst.copyMemory.src.valueType == IRVALUETYPE_VALUE ||
			   inst.copyMemory.src.valueType == IRVALUETYPE_VALUE_DEREFERENCE);
		u32 dstIdx = inst.copyMemory.dst.value.valueIdx;
		u32 srcIdx = inst.copyMemory.src.value.valueIdx;

		// First attempt to copy manually
		if (inst.copyMemory.size.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		{
			TypeInfo dstTypeInfo = context->typeTable[context->values[dstIdx].typeTableIdx];
			TypeInfo srcTypeInfo = context->typeTable[context->values[srcIdx].typeTableIdx];
			s64 size = inst.copyMemory.size.immediate;

			s64 copiedBytes = 0;
			while (size - copiedBytes >= 16)
			{
				X64Mov(context, x64Proc,
						IRValueDereference(dstIdx, TYPETABLEIDX_128, copiedBytes),
						IRValueDereference(srcIdx, TYPETABLEIDX_128, copiedBytes));
				copiedBytes += 16;
			}
			while (size - copiedBytes >= 8)
			{
				X64Mov(context, x64Proc,
						IRValueDereference(dstIdx, TYPETABLEIDX_S64, copiedBytes),
						IRValueDereference(srcIdx, TYPETABLEIDX_S64, copiedBytes));
				copiedBytes += 8;
			}
			while (size - copiedBytes >= 4)
			{
				X64Mov(context, x64Proc,
						IRValueDereference(dstIdx, TYPETABLEIDX_S32, copiedBytes),
						IRValueDereference(srcIdx, TYPETABLEIDX_S32, copiedBytes));
				copiedBytes += 4;
			}
			while (size - copiedBytes >= 1)
			{
				X64Mov(context, x64Proc,
						IRValueDereference(dstIdx, TYPETABLEIDX_S8, copiedBytes),
						IRValueDereference(srcIdx, TYPETABLEIDX_S8, copiedBytes));
				++copiedBytes;
			}
			return;
		}

		X64Mov(context, x64Proc, RCX, inst.copyMemory.dst);
		X64Mov(context, x64Proc, RDX, inst.copyMemory.src);
		X64Mov(context, x64Proc, R8,  inst.copyMemory.size);
		result.type = X64_CALL;
		result.procedureIdx = copyMemoryProcIdx;
		*BucketArrayAdd(&x64Proc->instructions) = result;
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
			TypeInfo dstTypeInfo = context->typeTable[context->values[dstIdx].typeTableIdx];
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
							IRValueValue(dstIdx, TYPETABLEIDX_128, copiedBytes), zeroXmmReg);
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
							IRValueValue(dstIdx, TYPETABLEIDX_S64, copiedBytes), zeroReg);
					copiedBytes += 8;
				}
				while (size - copiedBytes >= 4)
				{
					X64Mov(context, x64Proc,
							IRValueValue(dstIdx, TYPETABLEIDX_S32, copiedBytes), zeroReg);
					copiedBytes += 4;
				}
				while (size - copiedBytes >= 1)
				{
					X64Mov(context, x64Proc,
							IRValueValue(dstIdx, TYPETABLEIDX_S8, copiedBytes), zeroReg);
					++copiedBytes;
				}
			}
			return;
		}

		X64Mov(context, x64Proc, RCX, inst.zeroMemory.dst);
		X64Mov(context, x64Proc, RDX,  inst.zeroMemory.size);
		result.type = X64_CALL;
		result.procedureIdx = zeroMemoryProcIdx;
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
		IRValue tmp = IRValueNewValue(context, "_rmrmitmp"_s, left.typeTableIdx, immitateFlag,
				left.value.valueIdx);

		X64Mov(context, x64Proc, tmp, left);

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
		IRValue tmp = IRValueNewValue(context, left.typeTableIdx, immitateFlagLeft, out.value.valueIdx);

		X64Mov(context, x64Proc, tmp, left);

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
		if (context->typeTable[result.dst.typeTableIdx].size != 1)
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
		return TPrintF("call %S", GetProcedure(context, inst.procedureIdx)->name);
	case X64_CALL_Indirect:
		String proc = X64IRValueToStr(context, inst.procedureIRValue);
		return TPrintF("call %S", proc);
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

		PrintOut(context, "\n%S PROC\n", x64Proc.name);
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
		PrintOut(context, "%S ENDP\n", x64Proc.name);
	}
}

s64 X64StaticDataAlignTo(Context *context, s64 bytesSoFar, s64 alignment)
{
	s64 padding = (-bytesSoFar & (alignment - 1));
	if (padding)
	{
		PrintOut(context, "    DB 00");
		for (int i = 1; i < padding; ++i)
		{
			PrintOut(context, ", 00");
		}
		PrintOut(context, "\n");
		bytesSoFar += padding;
	}
	return bytesSoFar;
}

void X64PrintStaticData(Context *context, String name, IRValue value, s64 typeTableIdx,
		int alignmentOverride = -1)
{
	static s64 bytesSoFar = 0;

	switch (value.valueType)
	{
	case IRVALUETYPE_IMMEDIATE_STRING:
	{
		int alignment = alignmentOverride < 0 ? 8 : alignmentOverride;
		bytesSoFar = X64StaticDataAlignTo(context, bytesSoFar, alignment);

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

		bytesSoFar += 16;
	} break;
	case IRVALUETYPE_IMMEDIATE_FLOAT:
	{
		TypeInfo typeInfo = context->typeTable[typeTableIdx];
		int alignment = alignmentOverride < 0 ? (int)typeInfo.size : alignmentOverride;
		bytesSoFar = X64StaticDataAlignTo(context, bytesSoFar, alignment);
		switch (typeInfo.size)
		{
		case 4:
		{
			union { u32 asU32; f32 asF32; };
			asF32 = (f32)value.immediateFloat;
			PrintOut(context, "%S DD 0%.8xH\n", name, asU32);
		} break;
		case 8:
			PrintOut(context, "%S DQ 0%.16llxH\n", name,
					value.immediate);
			break;
		default:
			ASSERT(!"Invalid immediate size");
		}
		bytesSoFar += typeInfo.size;
	} break;
	case IRVALUETYPE_IMMEDIATE_GROUP:
	{
		int alignment = alignmentOverride < 0 ? 8 : alignmentOverride;
		bytesSoFar = X64StaticDataAlignTo(context, bytesSoFar, alignment);

		Array<IRValue, FrameAllocator> members = value.immediateStructMembers;
		for (int memberIdx = 0; memberIdx < members.size; ++memberIdx)
		{
			String memberName = memberIdx ? "   "_s : name;
			X64PrintStaticData(context, memberName, members[memberIdx],
					members[memberIdx].typeTableIdx);
		}
	} break;
	case IRVALUETYPE_VALUE_DEREFERENCE:
	{
		// @Improve: We are kinda using this to mean 'this is a value in data section, just put the
		// name in' which has nothing to do with 'VALUE_DEREFERENCE'...
		int alignment = alignmentOverride < 0 ? 8 : alignmentOverride;
		bytesSoFar = X64StaticDataAlignTo(context, bytesSoFar, alignment);

		Value v = context->values[value.value.valueIdx];
		ASSERT(v.flags & VALUEFLAGS_ON_STATIC_STORAGE);
		ASSERT(v.name.size);
		PrintOut(context, "%S DQ %S\n", name, v.name);

		bytesSoFar += 8;
	} break;
	case IRVALUETYPE_INVALID:
	{
		TypeInfo typeInfo = context->typeTable[typeTableIdx];
		PrintOut(context, "COMM %S:BYTE:0%llxH\n", name,
				typeInfo.size);
	} break;
	default:
	{
		TypeInfo typeInfo = context->typeTable[typeTableIdx];
		int alignment = alignmentOverride < 0 ? (int)typeInfo.size : alignmentOverride;
		bytesSoFar = X64StaticDataAlignTo(context, bytesSoFar, alignment);
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
		bytesSoFar += typeInfo.size;
	}
	}
}

void WriteOutOutputBuffer(Context *context, String filename)
{
	HANDLE outputFile = CreateFileA(
			StringToCStr(filename, PhaseAllocator::Alloc),
			GENERIC_WRITE,
			0,
			nullptr,
			CREATE_ALWAYS,
			FILE_ATTRIBUTE_NORMAL,
			nullptr
			);
	for (int i = 0; i < context->outputBuffer.buckets.size; ++i)
	{
		DWORD bytesWritten;
		WriteFile(outputFile,
				context->outputBuffer.buckets[i].data,
				(DWORD)context->outputBuffer.buckets[i].size,
				&bytesWritten,
				nullptr);
	}
	CloseHandle(outputFile);
}

void BackendMain(Context *context)
{
	BucketArrayInit(&context->bePatchedInstructions);

	// Hard coded CopyMemory and ZeroMemory external procedures
	{
		s64 voidPtrIdx = GetTypeInfoPointerOf(context, TYPETABLEIDX_VOID);

		TypeInfo t = { TYPECATEGORY_PROCEDURE };
		t.procedureInfo.returnTypeTableIdx = -1;
		ArrayInit(&t.procedureInfo.parameters, 3);
		t.procedureInfo.parameters.size = 3;
		t.procedureInfo.parameters[0] = { voidPtrIdx, {} };
		t.procedureInfo.parameters[1] = { voidPtrIdx, {} };
		t.procedureInfo.parameters[2] = { TYPETABLEIDX_S64, {} };
		s64 typeTableIdx = FindOrAddTypeTableIdx(context, t);

		copyMemoryProcIdx = -(s32)BucketArrayCount(&context->externalProcedures);
		Procedure *copyMemory = BucketArrayAdd(&context->externalProcedures);
		*copyMemory = {};
		copyMemory->name = "CopyMemory"_s;
		copyMemory->typeTableIdx = typeTableIdx;
		copyMemory->returnValueIdx = U32_MAX;

		zeroMemoryProcIdx = -(s32)BucketArrayCount(&context->externalProcedures);
		Procedure *zeroMemory = BucketArrayAdd(&context->externalProcedures);
		*zeroMemory = {};
		zeroMemory->name = "ZeroMemory"_s;
		zeroMemory->typeTableIdx = typeTableIdx;
		zeroMemory->returnValueIdx = U32_MAX;
	}

	x64InstructionInfos[X64_MOV] =       { "mov"_s,       OPERANDTYPE_REGMEM,   OPERANDACCESS_WRITE, OPERANDTYPE_ALL,    OPERANDACCESS_READ };
	x64InstructionInfos[X64_MOVZX] =     { "movzx"_s,     OPERANDTYPE_REGMEM,   OPERANDACCESS_WRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MOVSX] =     { "movsx"_s,     OPERANDTYPE_REGMEM,   OPERANDACCESS_WRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MOVSXD] =    { "movsxd"_s,    OPERANDTYPE_REGMEM,   OPERANDACCESS_WRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CQO] =       { "cqo"_s };
	x64InstructionInfos[X64_PUSH] =      { "push"_s,      OPERANDTYPE_ALL,      OPERANDACCESS_READ };
	x64InstructionInfos[X64_POP] =       { "pop"_s,       OPERANDTYPE_REGMEM,   OPERANDACCESS_WRITE };
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
	x64InstructionInfos[X64_CALL_Indirect] = { "call"_s,  OPERANDTYPE_REGMEM,   OPERANDACCESS_READ };
	x64InstructionInfos[X64_LEAVE] =     { "leave"_s };
	x64InstructionInfos[X64_RET] =       { "ret"_s };
	x64InstructionInfos[X64_LEA] =       { "lea"_s,       OPERANDTYPE_REGISTER, OPERANDACCESS_WRITE,     OPERANDTYPE_MEMORY, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CMP] =       { "cmp"_s,       OPERANDTYPE_REGMEM,   OPERANDACCESS_READ,      OPERANDTYPE_ALL,    OPERANDACCESS_READ };
	x64InstructionInfos[X64_TEST] =      { "test"_s,      OPERANDTYPE_REGMEM,   OPERANDACCESS_READ,      OPERANDTYPE_ALL,    OPERANDACCESS_READ };
	x64InstructionInfos[X64_SETG] =      { "setg"_s,      OPERANDTYPE_REGMEM,   OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETL] =      { "setl"_s,      OPERANDTYPE_REGMEM,   OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETGE] =     { "setge"_s,     OPERANDTYPE_REGMEM,   OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETLE] =     { "setle"_s,     OPERANDTYPE_REGMEM,   OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETA] =      { "seta"_s,      OPERANDTYPE_REGMEM,   OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETB] =      { "setb"_s,      OPERANDTYPE_REGMEM,   OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETAE] =     { "setae"_s,     OPERANDTYPE_REGMEM,   OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETBE] =     { "setbe"_s,     OPERANDTYPE_REGMEM,   OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETE] =      { "sete"_s,      OPERANDTYPE_REGMEM,   OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_SETNE] =     { "setne"_s,     OPERANDTYPE_REGMEM,   OPERANDACCESS_WRITE };
	x64InstructionInfos[X64_ADD] =       { "add"_s,       OPERANDTYPE_REGMEM,   OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SUB] =       { "sub"_s,       OPERANDTYPE_REGMEM,   OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MUL] =       { "mul"_s,       OPERANDTYPE_REGMEM,   OPERANDACCESS_READ };
	x64InstructionInfos[X64_IMUL] =      { "imul"_s,      OPERANDTYPE_REGMEM,   OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_DIV] =       { "div"_s,       OPERANDTYPE_REGMEM,   OPERANDACCESS_READ };
	x64InstructionInfos[X64_IDIV] =      { "idiv"_s,      OPERANDTYPE_REGMEM,   OPERANDACCESS_READ };
	x64InstructionInfos[X64_SAR] =       { "sar"_s,       OPERANDTYPE_REGMEM,   OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SAL] =       { "sal"_s,       OPERANDTYPE_REGMEM,   OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SHR] =       { "shr"_s,       OPERANDTYPE_REGMEM,   OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SHL] =       { "shl"_s,       OPERANDTYPE_REGMEM,   OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_AND] =       { "and"_s,       OPERANDTYPE_REGMEM,   OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_OR] =        { "or"_s,        OPERANDTYPE_REGMEM,   OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_XOR] =       { "xor"_s,       OPERANDTYPE_REGMEM,   OPERANDACCESS_READWRITE, OPERANDTYPE_ALL, OPERANDACCESS_READ };
	x64InstructionInfos[X64_NOT] =       { "not"_s,       OPERANDTYPE_REGMEM,   OPERANDACCESS_READWRITE };
	x64InstructionInfos[X64_NEG] =       { "neg"_s,       OPERANDTYPE_REGMEM,   OPERANDACCESS_READWRITE };
	x64InstructionInfos[X64_MOVSS] =     { "movss"_s,     OPERANDTYPE_REGMEM,   OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MOVSD] =     { "movsd"_s,     OPERANDTYPE_REGMEM,   OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_ADDSS] =     { "addss"_s,     OPERANDTYPE_REGMEM,   OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_ADDSD] =     { "addsd"_s,     OPERANDTYPE_REGMEM,   OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SUBSS] =     { "subss"_s,     OPERANDTYPE_REGMEM,   OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SUBSD] =     { "subsd"_s,     OPERANDTYPE_REGMEM,   OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MULSS] =     { "mulss"_s,     OPERANDTYPE_REGISTER, OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MULSD] =     { "mulsd"_s,     OPERANDTYPE_REGISTER, OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_DIVSS] =     { "divss"_s,     OPERANDTYPE_REGISTER, OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_DIVSD] =     { "divsd"_s,     OPERANDTYPE_REGISTER, OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_XORPS] =     { "xorps"_s,     OPERANDTYPE_REGISTER, OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_XORPD] =     { "xorpd"_s,     OPERANDTYPE_REGISTER, OPERANDACCESS_READWRITE, OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SQRTSS] =    { "sqrtss"_s,    OPERANDTYPE_REGISTER, OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_SQRTSD] =    { "sqrtsd"_s,    OPERANDTYPE_REGISTER, OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_COMISS] =    { "comiss"_s,    OPERANDTYPE_REGISTER, OPERANDACCESS_READ,      OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_COMISD] =    { "comisd"_s,    OPERANDTYPE_REGISTER, OPERANDACCESS_READ,      OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CVTSI2SS] =  { "cvtsi2ss"_s,  OPERANDTYPE_REGISTER, OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CVTSI2SD] =  { "cvtsi2sd"_s,  OPERANDTYPE_REGISTER, OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CVTTSS2SI] = { "cvttss2si"_s, OPERANDTYPE_REGISTER, OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CVTTSD2SI] = { "cvttsd2si"_s, OPERANDTYPE_REGISTER, OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CVTSS2SD] =  { "cvtss2sd"_s,  OPERANDTYPE_REGISTER, OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_CVTSD2SS] =  { "cvtsd2ss"_s,  OPERANDTYPE_REGISTER, OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MOVUPS] =    { "movups"_s,    OPERANDTYPE_REGMEM,   OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };
	x64InstructionInfos[X64_MOVAPS] =    { "movaps"_s,    OPERANDTYPE_REGMEM,   OPERANDACCESS_WRITE,     OPERANDTYPE_REGMEM, OPERANDACCESS_READ };

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
		context->values[RAX_valueIdx + i].allocatedRegister = i;

	RAX  = { IRVALUETYPE_VALUE, RAX_valueIdx, TYPETABLEIDX_S64 };
	RCX  = { IRVALUETYPE_VALUE, RCX_valueIdx, TYPETABLEIDX_S64 };
	RDX  = { IRVALUETYPE_VALUE, RDX_valueIdx, TYPETABLEIDX_S64 };
	RBX  = { IRVALUETYPE_VALUE, RBX_valueIdx, TYPETABLEIDX_S64 };
	RSI  = { IRVALUETYPE_VALUE, RSI_valueIdx, TYPETABLEIDX_S64 };
	RDI  = { IRVALUETYPE_VALUE, RDI_valueIdx, TYPETABLEIDX_S64 };
	RSP  = { IRVALUETYPE_VALUE, RSP_valueIdx, TYPETABLEIDX_S64 };
	RBP  = { IRVALUETYPE_VALUE, RBP_valueIdx, TYPETABLEIDX_S64 };
	R8   = { IRVALUETYPE_VALUE, R8_valueIdx,  TYPETABLEIDX_S64 };
	R9   = { IRVALUETYPE_VALUE, R9_valueIdx,  TYPETABLEIDX_S64 };
	R10  = { IRVALUETYPE_VALUE, R10_valueIdx, TYPETABLEIDX_S64 };
	R11  = { IRVALUETYPE_VALUE, R11_valueIdx, TYPETABLEIDX_S64 };
	R12  = { IRVALUETYPE_VALUE, R12_valueIdx, TYPETABLEIDX_S64 };
	R13  = { IRVALUETYPE_VALUE, R13_valueIdx, TYPETABLEIDX_S64 };
	R14  = { IRVALUETYPE_VALUE, R14_valueIdx, TYPETABLEIDX_S64 };
	R15  = { IRVALUETYPE_VALUE, R15_valueIdx, TYPETABLEIDX_S64 };

	EAX  = { IRVALUETYPE_VALUE, RAX_valueIdx, TYPETABLEIDX_S32 };
	ECX  = { IRVALUETYPE_VALUE, RCX_valueIdx, TYPETABLEIDX_S32 };
	EDX  = { IRVALUETYPE_VALUE, RDX_valueIdx, TYPETABLEIDX_S32 };
	EBX  = { IRVALUETYPE_VALUE, RBX_valueIdx, TYPETABLEIDX_S32 };
	ESI  = { IRVALUETYPE_VALUE, RSI_valueIdx, TYPETABLEIDX_S32 };
	EDI  = { IRVALUETYPE_VALUE, RDI_valueIdx, TYPETABLEIDX_S32 };
	ESP  = { IRVALUETYPE_VALUE, RSP_valueIdx, TYPETABLEIDX_S32 };
	EBP  = { IRVALUETYPE_VALUE, RBP_valueIdx, TYPETABLEIDX_S32 };
	R8D  = { IRVALUETYPE_VALUE, R8_valueIdx,  TYPETABLEIDX_S32 };
	R9D  = { IRVALUETYPE_VALUE, R9_valueIdx,  TYPETABLEIDX_S32 };
	R10D = { IRVALUETYPE_VALUE, R10_valueIdx, TYPETABLEIDX_S32 };
	R11D = { IRVALUETYPE_VALUE, R11_valueIdx, TYPETABLEIDX_S32 };
	R12D = { IRVALUETYPE_VALUE, R12_valueIdx, TYPETABLEIDX_S32 };
	R13D = { IRVALUETYPE_VALUE, R13_valueIdx, TYPETABLEIDX_S32 };
	R14D = { IRVALUETYPE_VALUE, R14_valueIdx, TYPETABLEIDX_S32 };
	R15D = { IRVALUETYPE_VALUE, R15_valueIdx, TYPETABLEIDX_S32 };

	AX   = { IRVALUETYPE_VALUE, RAX_valueIdx, TYPETABLEIDX_S16 };
	CX   = { IRVALUETYPE_VALUE, RCX_valueIdx, TYPETABLEIDX_S16 };
	DX   = { IRVALUETYPE_VALUE, RDX_valueIdx, TYPETABLEIDX_S16 };
	BX   = { IRVALUETYPE_VALUE, RBX_valueIdx, TYPETABLEIDX_S16 };
	SI   = { IRVALUETYPE_VALUE, RSI_valueIdx, TYPETABLEIDX_S16 };
	DI   = { IRVALUETYPE_VALUE, RDI_valueIdx, TYPETABLEIDX_S16 };
	SP   = { IRVALUETYPE_VALUE, RSP_valueIdx, TYPETABLEIDX_S16 };
	BP   = { IRVALUETYPE_VALUE, RBP_valueIdx, TYPETABLEIDX_S16 };
	R8W  = { IRVALUETYPE_VALUE, R8_valueIdx,  TYPETABLEIDX_S16 };
	R9W  = { IRVALUETYPE_VALUE, R9_valueIdx,  TYPETABLEIDX_S16 };
	R10W = { IRVALUETYPE_VALUE, R10_valueIdx, TYPETABLEIDX_S16 };
	R11W = { IRVALUETYPE_VALUE, R11_valueIdx, TYPETABLEIDX_S16 };
	R12W = { IRVALUETYPE_VALUE, R12_valueIdx, TYPETABLEIDX_S16 };
	R13W = { IRVALUETYPE_VALUE, R13_valueIdx, TYPETABLEIDX_S16 };
	R14W = { IRVALUETYPE_VALUE, R14_valueIdx, TYPETABLEIDX_S16 };
	R15W = { IRVALUETYPE_VALUE, R15_valueIdx, TYPETABLEIDX_S16 };

	AL   = { IRVALUETYPE_VALUE, RAX_valueIdx, TYPETABLEIDX_S8 };
	CL   = { IRVALUETYPE_VALUE, RCX_valueIdx, TYPETABLEIDX_S8 };
	DL   = { IRVALUETYPE_VALUE, RDX_valueIdx, TYPETABLEIDX_S8 };
	BL   = { IRVALUETYPE_VALUE, RBX_valueIdx, TYPETABLEIDX_S8 };
	SIL  = { IRVALUETYPE_VALUE, RSI_valueIdx, TYPETABLEIDX_S8 };
	DIL  = { IRVALUETYPE_VALUE, RDI_valueIdx, TYPETABLEIDX_S8 };
	SPL  = { IRVALUETYPE_VALUE, RSP_valueIdx, TYPETABLEIDX_S8 };
	BPL  = { IRVALUETYPE_VALUE, RBP_valueIdx, TYPETABLEIDX_S8 };
	R8B  = { IRVALUETYPE_VALUE, R8_valueIdx,  TYPETABLEIDX_S8 };
	R9B  = { IRVALUETYPE_VALUE, R9_valueIdx,  TYPETABLEIDX_S8 };
	R10B = { IRVALUETYPE_VALUE, R10_valueIdx, TYPETABLEIDX_S8 };
	R11B = { IRVALUETYPE_VALUE, R11_valueIdx, TYPETABLEIDX_S8 };
	R12B = { IRVALUETYPE_VALUE, R12_valueIdx, TYPETABLEIDX_S8 };
	R13B = { IRVALUETYPE_VALUE, R13_valueIdx, TYPETABLEIDX_S8 };
	R14B = { IRVALUETYPE_VALUE, R14_valueIdx, TYPETABLEIDX_S8 };
	R15B = { IRVALUETYPE_VALUE, R15_valueIdx, TYPETABLEIDX_S8 };

	XMM0 =  { IRVALUETYPE_VALUE, XMM0_valueIdx,  TYPETABLEIDX_F64 };
	XMM1 =  { IRVALUETYPE_VALUE, XMM1_valueIdx,  TYPETABLEIDX_F64 };
	XMM2 =  { IRVALUETYPE_VALUE, XMM2_valueIdx,  TYPETABLEIDX_F64 };
	XMM3 =  { IRVALUETYPE_VALUE, XMM3_valueIdx,  TYPETABLEIDX_F64 };
	XMM4 =  { IRVALUETYPE_VALUE, XMM4_valueIdx,  TYPETABLEIDX_F64 };
	XMM5 =  { IRVALUETYPE_VALUE, XMM5_valueIdx,  TYPETABLEIDX_F64 };
	XMM6 =  { IRVALUETYPE_VALUE, XMM6_valueIdx,  TYPETABLEIDX_F64 };
	XMM7 =  { IRVALUETYPE_VALUE, XMM7_valueIdx,  TYPETABLEIDX_F64 };
	XMM8 =  { IRVALUETYPE_VALUE, XMM8_valueIdx,  TYPETABLEIDX_F64 };
	XMM9 =  { IRVALUETYPE_VALUE, XMM9_valueIdx,  TYPETABLEIDX_F64 };
	XMM10 = { IRVALUETYPE_VALUE, XMM10_valueIdx, TYPETABLEIDX_F64 };
	XMM11 = { IRVALUETYPE_VALUE, XMM11_valueIdx, TYPETABLEIDX_F64 };
	XMM12 = { IRVALUETYPE_VALUE, XMM12_valueIdx, TYPETABLEIDX_F64 };
	XMM13 = { IRVALUETYPE_VALUE, XMM13_valueIdx, TYPETABLEIDX_F64 };
	XMM14 = { IRVALUETYPE_VALUE, XMM14_valueIdx, TYPETABLEIDX_F64 };
	XMM15 = { IRVALUETYPE_VALUE, XMM15_valueIdx, TYPETABLEIDX_F64 };

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

	x64ParameterValuesRead[0] = RCX.value.valueIdx;
	x64ParameterValuesRead[1] = RDX.value.valueIdx;
	x64ParameterValuesRead[2] = R8.value.valueIdx;
	x64ParameterValuesRead[3] = R9.value.valueIdx;
	for (int paramIdx = 4; paramIdx < 32; ++paramIdx)
	{
		u32 newValueIdx = NewValue(context, TPrintF("_param%d", paramIdx), TYPETABLEIDX_S64,
				VALUEFLAGS_IS_ALLOCATED | VALUEFLAGS_IS_MEMORY | VALUEFLAGS_BASE_RELATIVE);
		// Add 16, 8 for return address, and 8 because we push RBP
		// (remember by now we are at parameter index 4+).
		context->values[newValueIdx].stackOffset = 16 + paramIdx * 8;
		x64ParameterValuesRead[paramIdx] = newValueIdx;
	}

	x64ParameterValuesWrite[0] = RCX.value.valueIdx;
	x64ParameterValuesWrite[1] = RDX.value.valueIdx;
	x64ParameterValuesWrite[2] = R8.value.valueIdx;
	x64ParameterValuesWrite[3] = R9.value.valueIdx;
	for (int paramIdx = 4; paramIdx < 32; ++paramIdx)
	{
		u32 newValueIdx = NewValue(context, TPrintF("_param%d", paramIdx), TYPETABLEIDX_S64,
				VALUEFLAGS_IS_ALLOCATED | VALUEFLAGS_IS_MEMORY);
		// Add 16, 8 for return address, and 8 because we push RBP
		// (remember by now we are at parameter index 4+).
		context->values[newValueIdx].stackOffset = paramIdx * 8;
		x64ParameterValuesWrite[paramIdx] = newValueIdx;
	}

	Array<X64Procedure, PhaseAllocator> x64Procedures;
	u64 procedureCount = BucketArrayCount(&context->procedures);
	ArrayInit(&x64Procedures, procedureCount);
	x64Procedures.size = procedureCount;

	// Create X64Procedures
	for (int procedureIdx = 1; procedureIdx < procedureCount; ++procedureIdx)
	{
		Procedure *proc = GetProcedure(context, procedureIdx);
		X64Procedure *x64Proc = &x64Procedures[procedureIdx];
		ASSERT(context->typeTable[proc->typeTableIdx].typeCategory == TYPECATEGORY_PROCEDURE);
		TypeInfoProcedure procTypeInfo = context->typeTable[proc->typeTableIdx].procedureInfo;

		x64Proc->name = GetProcedure(context, procedureIdx)->name;
		x64Proc->allocatedParameterCount = proc->allocatedParameterCount;
		x64Proc->returnValueIdx = proc->returnValueIdx;
		x64Proc->stackSize = 0;
		DynamicArrayInit(&x64Proc->spilledValues, 8);

		BucketArrayInit(&x64Proc->instructions);

		// Allocate parameters
		bool returnByCopy = IRShouldPassByCopy(context, procTypeInfo.returnTypeTableIdx);
		int paramIdx = 0;
		int paramCount = (int)proc->parameterValues.size;
		if (procTypeInfo.returnTypeTableIdx != TYPETABLEIDX_VOID && returnByCopy)
		{
			++paramIdx;
			X64Mov(context, x64Proc, IRValueValue(proc->returnValueIdx, TYPETABLEIDX_S64), RCX);
		}
		for (int i = 0; i < paramCount; ++i, ++paramIdx)
		{
			u32 paramValueIdx = proc->parameterValues[i];
			Value *v = &context->values[paramValueIdx];
			s64 paramTypeIdx = v->typeTableIdx;
			if (IRShouldPassByCopy(context, paramTypeIdx))
				paramTypeIdx = GetTypeInfoPointerOf(context, paramTypeIdx);
			bool floating = context->typeTable[paramTypeIdx].typeCategory == TYPECATEGORY_FLOATING;

			IRValue src;
			switch (paramIdx)
			{
			case 0:
				src = floating ? XMM0 : RCX;
				break;
			case 1:
				src = floating ? XMM1 : RDX;
				break;
			case 2:
				src = floating ? XMM2 : R8;
				break;
			case 3:
				src = floating ? XMM3 : R9;
				break;
			default:
				src = IRValueValue(x64ParameterValuesRead[paramIdx], TYPETABLEIDX_S64);
			}
			if (floating)
				src.typeTableIdx = paramTypeIdx;

			v->flags |= VALUEFLAGS_TRY_IMMITATE;
			v->tryImmitateValueIdx = src.value.valueIdx;

			X64Mov(context, x64Proc, IRValueValue(paramValueIdx, paramTypeIdx), src);
		}

		u64 instructionCount = BucketArrayCount(&proc->instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			IRInstruction inst = proc->instructions[instructionIdx];
			X64ConvertInstruction(context, inst, x64Proc);
		}

		if (procTypeInfo.returnTypeTableIdx != TYPETABLEIDX_VOID)
		{
			s64 returnTypeTableIdx = context->values[proc->returnValueIdx].typeTableIdx;
			if (returnByCopy)
			{
				IRValue returnValue = IRValueValue(proc->returnValueIdx, returnTypeTableIdx);
				*BucketArrayAdd(&x64Proc->instructions) = { X64_LEA, RAX, returnValue };
			}
			else
			{
				IRValue returnValue = IRValueValue(proc->returnValueIdx, returnTypeTableIdx);
				if (context->typeTable[returnTypeTableIdx].typeCategory == TYPECATEGORY_FLOATING)
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

	BucketArrayInit(&context->outputBuffer);
	X64PrintInstructions(context, x64Procedures);
	WriteOutOutputBuffer(context, "output/x64_before_allocation.txt"_s);

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
					Value v = context->values[inst->src.value.valueIdx];
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

				Value dst = context->values[inst->dst.value.valueIdx];
				Value src = context->values[inst->src.value.valueIdx];
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
					Value dst = context->values[inst->dst.value.valueIdx];
					Value src = context->values[inst->src.value.valueIdx];
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
					Value v = context->values[inst->dst.value.valueIdx];
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
		static s64 pointerToStructMemberInfoIdx =
			GetTypeInfoPointerOf(context, TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT);
		static s64 pointerToStringIdx =
			GetTypeInfoPointerOf(context, TYPETABLEIDX_STRING_STRUCT);
		static s64 pointerToS64Idx =
			GetTypeInfoPointerOf(context, TYPETABLEIDX_S64);
		static s64 pointerToTypeInfoIdx =
			GetTypeInfoPointerOf(context, TYPETABLEIDX_TYPE_INFO_STRUCT);

		u64 tableSize = BucketArrayCount(&context->typeTable);
		for (u64 typeTableIdx = 1; typeTableIdx < tableSize; ++typeTableIdx)
		{
			TypeInfo typeInfo = context->typeTable[typeTableIdx];

			context->values[typeInfo.valueIdx].typeTableIdx = TYPETABLEIDX_TYPE_INFO_STRUCT;

			IRStaticVariable newStaticVar = { typeInfo.valueIdx };
			newStaticVar.initialValue.valueType = IRVALUETYPE_IMMEDIATE_GROUP;
			newStaticVar.initialValue.typeTableIdx = -1;

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
				String structName = "<anonymous struct>"_s;
				StaticDefinition *staticDefStruct = FindStaticDefinitionByTypeTableIdx(context,
						typeTableIdx);
				if (staticDefStruct)
					structName = staticDefStruct->name;

				u32 membersValueIdx = NewValue(context, TPrintF("_members_%lld", typeTableIdx),
						TYPETABLEIDX_TYPE_INFO_STRUCT_MEMBER_STRUCT, VALUEFLAGS_ON_STATIC_STORAGE);
				IRStaticVariable membersStaticVar = { membersValueIdx };
				membersStaticVar.initialValue.valueType = IRVALUETYPE_IMMEDIATE_GROUP;
				ArrayInit(&membersStaticVar.initialValue.immediateStructMembers,
						typeInfo.structInfo.members.size);
				for (s64 memberIdx = 0; memberIdx < (s64)typeInfo.structInfo.members.size; ++memberIdx)
				{
					StructMember member = typeInfo.structInfo.members[memberIdx];
					TypeInfo memberType = context->typeTable[member.typeTableIdx];

					IRValue memberImm;
					memberImm.valueType = IRVALUETYPE_IMMEDIATE_GROUP;
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
				String enumName = "<anonymous enum>"_s;
				StaticDefinition *staticDefStruct = FindStaticDefinitionByTypeTableIdx(context,
						typeTableIdx);
				if (staticDefStruct)
					enumName = staticDefStruct->name;

				TypeInfo enumType = context->typeTable[typeInfo.enumInfo.typeTableIdx];

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
				TypeInfo pointedType = context->typeTable[typeInfo.pointerInfo.pointedTypeTableIdx];

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
				TypeInfo elementType = context->typeTable[typeInfo.arrayInfo.elementTypeTableIdx];

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
							context->typeTable[typeInfo.procedureInfo.parameters[paramIdx].typeTableIdx];
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
			case TYPECATEGORY_INVALID:
			{
				ArrayInit(&newStaticVar.initialValue.immediateStructMembers, 1);
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(7, TYPETABLEIDX_S8) };
			} break;
			default:
				ASSERT(false);
			}
			*DynamicArrayAdd(&context->irStaticVariables) = newStaticVar;
		}
	}

	TimerSplit("X64 register/stack allocation"_s);

	BucketArrayInit(&context->outputBuffer);

	PrintOut(context, "include ..\\core\\memory.asm\n\n");

	PrintOut(context, "_DATA SEGMENT\n");

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

	//int padding = 16 - (bytesWritten & 15);
	X64StaticDataAlignTo(context, bytesWritten, 16);

	const u64 staticVariableCount = context->irStaticVariables.size;
	for (int staticVariableIdx = 0; staticVariableIdx < staticVariableCount; ++staticVariableIdx)
	{
		IRStaticVariable staticVar = context->irStaticVariables[staticVariableIdx];
		Value value = context->values[staticVar.valueIdx];
		String name = value.name;
		X64PrintStaticData(context, name, staticVar.initialValue, value.typeTableIdx, 16);
	}

	PrintOut(context, "_DATA ENDS\n");

	for (int procedureIdx = 1; procedureIdx < procedureCount; ++procedureIdx)
	{
		StaticDefinition *staticDef = FindStaticDefinitionByProcedure(context, procedureIdx);
		if (staticDef)
			PrintOut(context, "PUBLIC %S\n", staticDef->name);
	}

	for (int varIdx = 0; varIdx < context->irExternalVariables.size; ++varIdx)
	{
		Value v = context->values[context->irExternalVariables[varIdx]];
		s64 size = context->typeTable[v.typeTableIdx].size;
		String type;
		switch (size)
		{
			case 1: type = "BYTE"_s; break;
			case 2: type = "WORD"_s; break;
			case 4: type = "DWORD"_s; break;
			case 8: type = "QWORD"_s; break;
			default: type = "QWORD"_s;
		}
		PrintOut(context, "EXTRN %S:%S\n", v.name, type);
	}

	u64 externalProcedureCount = BucketArrayCount(&context->externalProcedures);
	for (int procedureIdx = 1; procedureIdx < externalProcedureCount; ++procedureIdx)
	{
		StaticDefinition *staticDef = FindStaticDefinitionByProcedure(context, -procedureIdx);
		if (staticDef)
			PrintOut(context, "EXTRN %S:proc\n", staticDef->name);
	}

	PrintOut(context, "_TEXT SEGMENT\n");

	// Code
	X64PrintInstructions(context, x64Procedures);

	PrintOut(context, "_TEXT ENDS\n");
	PrintOut(context, "END\n");

	String outputPath;
	String assemblyOutputFilename;
	{
		char *buffer = (char *)PhaseAllocator::Alloc(MAX_PATH);
		outputPath.size = GetFullPathNameA("output", MAX_PATH, buffer, nullptr);
		outputPath.data = buffer;

		CreateDirectoryA(outputPath.data, nullptr);

		buffer = (char *)PhaseAllocator::Alloc(MAX_PATH);
		assemblyOutputFilename.size = GetFullPathNameA("output\\out.asm", MAX_PATH, buffer, nullptr);
		assemblyOutputFilename.data = buffer;
	}

	WriteOutOutputBuffer(context, assemblyOutputFilename);

	TimerSplit("X64 output file write"_s);

	// Run MASM
	PWSTR programFilesPathWstr;
	SHGetKnownFolderPath(FOLDERID_ProgramFilesX86, 0, NULL, &programFilesPathWstr);
	String programFilesPath = StupidStrToString(programFilesPathWstr, PhaseAllocator::Alloc);

	String visualStudioPath = TPrintF("%S\\Microsoft Visual Studio", programFilesPath);
	{
		// Get anything starting with 20...
		String wildcard = TPrintF("%S\\20*", visualStudioPath);
		WIN32_FIND_DATAA foundData = {};
		HANDLE findHandle = FindFirstFileA(StringToCStr(wildcard, PhaseAllocator::Alloc), &foundData);
		const char *newestVersionStr = nullptr;
		int newestVersion = 0;
		if (findHandle != INVALID_HANDLE_VALUE) while (true)
		{
			int foundVersion = atoi(foundData.cFileName);
			if (foundVersion > newestVersion)
			{
				newestVersion = foundVersion;
				newestVersionStr = foundData.cFileName;
			}
			if (!FindNextFileA(findHandle, &foundData)) break;
		}
		visualStudioPath = TPrintF("%S\\%s", visualStudioPath, newestVersionStr);
	}

	String msvcPath = {};
	{
		String buildToolsPath = TPrintF("%S\\BuildTools", visualStudioPath);
		String enterprisePath = TPrintF("%S\\Enterprise", visualStudioPath);
		String professionalPath = TPrintF("%S\\Professional", visualStudioPath);
		String communityPath = TPrintF("%S\\Community", visualStudioPath);
		if (GetFileAttributes(StringToCStr(buildToolsPath, PhaseAllocator::Alloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = buildToolsPath;
		else if (GetFileAttributes(StringToCStr(enterprisePath, PhaseAllocator::Alloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = enterprisePath;
		else if (GetFileAttributes(StringToCStr(professionalPath, PhaseAllocator::Alloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = professionalPath;
		else if (GetFileAttributes(StringToCStr(communityPath, PhaseAllocator::Alloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = communityPath;
		msvcPath = TPrintF("%S\\VC\\Tools\\MSVC", msvcPath);

		String wildcard = StringConcat(msvcPath, "\\*"_s);
		WIN32_FIND_DATAA foundData = {};
		HANDLE findHandle = FindFirstFileA(StringToCStr(wildcard, PhaseAllocator::Alloc), &foundData);
		if (findHandle != INVALID_HANDLE_VALUE)
		{
			while (foundData.cFileName[0] == '.')
				FindNextFileA(findHandle, &foundData);
			msvcPath = TPrintF("%S\\%s", msvcPath, foundData.cFileName);
		}
	}

	String windowsSDKPath = TPrintF("%S\\Windows Kits\\10", programFilesPath);
	String windowsSDKVersion = {};
	{
		String wildcard = TPrintF("%S\\include\\10.*", windowsSDKPath);
		WIN32_FIND_DATAA foundData = {};
		HANDLE findHandle = FindFirstFileA(StringToCStr(wildcard, PhaseAllocator::Alloc), &foundData);
		s64 highestTuple[4] = {};
		const char *latestVersionName = nullptr;
		if (findHandle != INVALID_HANDLE_VALUE) while (true)
		{
			s64 tuple[4];
			int numDigits = 0;
			int foundNumbers = 0;
			// Parse tuple
			for (const char *scan = foundData.cFileName; ; ++scan)
			{
				if (*scan == '.' || *scan == 0)
				{
					tuple[foundNumbers++] = IntFromString({ numDigits, scan - numDigits });
					numDigits = 0;
				}
				else
					++numDigits;
				if (!*scan) break;
			}
			// Replace if greater
			for (int i = 0; i < 4; ++i)
			{
				if (tuple[i] < highestTuple[i])
					goto nextTuple;
				if (tuple[i] > highestTuple[i])
					break;
				// If same keep going
			}
			memcpy(highestTuple, tuple, sizeof(highestTuple));
			latestVersionName = foundData.cFileName;
nextTuple:
			if (!FindNextFileA(findHandle, &foundData)) break;
		}
		windowsSDKVersion = CStrToString(latestVersionName);
	}

	bool useWindowsSubsystem = false;
	StaticDefinition *subsystemStaticDef = FindStaticDefinitionByName(context,
			"compiler_subsystem"_s);
	if (subsystemStaticDef)
	{
		ASSERT(subsystemStaticDef->definitionType == STATICDEFINITIONTYPE_CONSTANT);
		ASSERT(subsystemStaticDef->constant.type == CONSTANTTYPE_INTEGER);
		useWindowsSubsystem = subsystemStaticDef->constant.valueAsInt == 1;
	}

	String subsystemArgument;
	if (useWindowsSubsystem)
		subsystemArgument = "/subsystem:WINDOWS "_s;
	else
		subsystemArgument = "/subsystem:CONSOLE "_s;

	String libsToLinkStr = {};
	for (int i = 0; i < context->libsToLink.size; ++i)
		libsToLinkStr = TPrintF("%S %S", libsToLinkStr, context->libsToLink[i]);

	String commandLine = TPrintF(
			"%S\\bin\\Hostx64\\x64\\ml64.exe " // msvcPath
			"out.asm "
			"/nologo /c "
			"/Zd "
			"/Zi "
			"/Fm "
			"/I \"%S\\include\" " // msvcPath
			"/I \"%S\\include\\%S\\ucrt\" " // windowsSDKPath, windowsSDKVersion
			"/I \"%S\\include\\%S\\shared\" " // windowsSDKPath, windowsSDKVersion
			"/I \"%S\\include\\%S\\um\" " // windowsSDKPath, windowsSDKVersion
			"/I \"%S\\include\\%S\\winrt\" " // windowsSDKPath, windowsSDKVersion
			"/I \"%S\\include\\%S\\cppwinrt\" " // windowsSDKPath, windowsSDKVersion
			"%c",
			msvcPath,
			msvcPath,
			windowsSDKPath, windowsSDKVersion,
			windowsSDKPath, windowsSDKVersion,
			windowsSDKPath, windowsSDKVersion,
			windowsSDKPath, windowsSDKVersion,
			windowsSDKPath, windowsSDKVersion,
			0
			);

	STARTUPINFO startupInfo = {};
	PROCESS_INFORMATION processInformation = {};
	startupInfo.cb = sizeof(STARTUPINFO);
	if (!CreateProcessA(
			NULL,
			(LPSTR)commandLine.data,
			NULL,
			NULL,
			false,
			0,
			NULL,
			outputPath.data,
			&startupInfo,
			&processInformation
			))
	{
		Print("Failed to call ml64.exe (%d)\n", GetLastError());
		CRASH;
	}
	WaitForSingleObject(processInformation.hProcess, INFINITE);

	TimerSplit("Calling ML64"_s);

	DWORD exitCode;
	GetExitCodeProcess(processInformation.hProcess, &exitCode);
	if (exitCode != 0)
	{
		Print("ml64.exe returned an error (%d)\n", exitCode);
		CRASH;
	}
	CloseHandle(processInformation.hProcess);
	CloseHandle(processInformation.hThread);

	commandLine = TPrintF(
			"%S\\bin\\Hostx64\\x64\\link.exe " // msvcPath
			"out.obj "
			"/nologo "
			"%S " // subsystemArgument
			"kernel32.lib "
			"user32.lib "
			"gdi32.lib "
			"winmm.lib "
			"%S " // libsToLinkStr
			"/nologo "
			"/debug:full "
			"/entry:__WindowsMain "
			"/opt:ref "
			"/incremental:no "
			"/dynamicbase:no "
			"/libpath:\"%S\\lib\\x64\" " // msvcPath
			"/libpath:\"%S\\lib\\%S\\ucrt\\x64\" " // windowsSDKPath, windowsSDKVersion
			"/libpath:\"%S\\lib\\%S\\um\\x64\" " // windowsSDKPath, windowsSDKVersion
			"/out:out.exe%c",
			msvcPath,
			subsystemArgument,
			libsToLinkStr,
			msvcPath,
			windowsSDKPath, windowsSDKVersion,
			windowsSDKPath, windowsSDKVersion,
			0
			);

	startupInfo = {};
	processInformation = {};
	startupInfo.cb = sizeof(STARTUPINFO);
	if (!CreateProcessA(
			NULL,
			(LPSTR)commandLine.data,
			NULL,
			NULL,
			false,
			0,
			NULL,
			outputPath.data,
			&startupInfo,
			&processInformation
			))
	{
		Print("Failed to call link.exe (%d)\n", GetLastError());
		CRASH;
	}
	WaitForSingleObject(processInformation.hProcess, INFINITE);

	TimerSplit("Calling LINK"_s);

	CloseHandle(processInformation.hProcess);
	CloseHandle(processInformation.hThread);
}
