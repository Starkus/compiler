enum AcceptedOperandFlags
{
	ACCEPTEDOPERANDS_NONE      = 0,
	ACCEPTEDOPERANDS_REGISTER  = 1,
	ACCEPTEDOPERANDS_MEMORY    = 2,
	ACCEPTEDOPERANDS_REGMEM    = 3,
	ACCEPTEDOPERANDS_IMMEDIATE = 4,
	ACCEPTEDOPERANDS_ALL       = 7,
};

struct X64InstructionInfo
{
	String mnemonic;
	u8 acceptedOperandsLeft;
	u8 acceptedOperandsRight;
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
	X64_SETNE,
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
	X64_Count,
	X64_Ignore = X64_Count,
	X64_Label,
	X64_Push_Scope,
	X64_Pop_Scope,
	X64_Push_Value,
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
		struct
		{
			// Proc call info
			String procLabel;
			s32 procParameterCount; // @Improve: remove this.
			Array<u32> liveValues;
		};
		u32 valueIdx;
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
	s64 allocatedParameterCount;
	u32 returnValueIdx;
	DynamicArray<u32, malloc, realloc> spilledValues;
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

X64InstructionInfo x64InstructionInfos[X64_Count];

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

IRValue x64Registers[X64REGISTER_Count] = {
	RAX,	RCX,	RDX,	RBX,
	RSI,	RDI,	RSP,	RBP,
	R8,		R9,		R10,	R11,
	R12,	R13,	R14,	R15,
	XMM0,	XMM1,	XMM2,	XMM3,
	XMM4,	XMM5,	XMM6,	XMM7
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

	DWORD bytesWritten;
#if PRINT_ASM_OUTPUT
	if (!context->config.silent)
	{
		OutputDebugStringA(buffer);
		WriteFile(g_hStdout, buffer, (DWORD)size, &bytesWritten, nullptr); // Stdout
	}
#endif
	WriteFile(context->outputFile, buffer, (DWORD)size, &bytesWritten, nullptr);

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
	TypeInfo typeInfo = context->typeTable[value.typeTableIdx];
	size = typeInfo.size;
	Value v = context->values[value.valueIdx];
	if (v.flags & VALUEFLAGS_ON_STATIC_STORAGE)
	{
		result = v.name;
		goto decoratePtr;
	}

	bool floating = typeInfo.typeCategory == TYPECATEGORY_FLOATING;

	s64 offset = 0;
	if (value.valueType == IRVALUETYPE_MEMORY)
	{
		offset = value.memory.offset;
	}

	if (v.flags & VALUEFLAGS_IS_ALLOCATED)
	{
		if (v.flags & VALUEFLAGS_IS_MEMORY)
		{
			offset += v.stackOffset;
			if (v.flags & VALUEFLAGS_BASE_RELATIVE)
				result = "rbp"_s;
			else
				result = "rsp"_s;

			if (offset > 0)
				result = TPrintF("%S+0%xh", result, offset);
			else if (offset < 0)
				result = TPrintF("%S-0%xh", result, -offset);
		}
		else if (value.valueType == IRVALUETYPE_MEMORY)
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
		}
		else if (!floating)
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
		}
		else switch (v.allocatedRegister)
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
			ASSERT(!"Value assigned invalid register!");
		}
	}
	// Not allocated
	else
	{
		if (v.name)
			result = TPrintF("$vr%d\"%S\"", value.valueIdx, v.name);
		else
			result = TPrintF("$vr%d", value.valueIdx);

		if (offset > 0)
			result = TPrintF("%S+0%xh", result, offset);
		else if (offset < 0)
			result = TPrintF("%S-0%xh", result, -offset);
	}

	if (value.valueType != IRVALUETYPE_MEMORY && !(v.flags & VALUEFLAGS_IS_MEMORY))
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
		default:
			result = TPrintF("QWORD PTR [%S]", result);
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

bool IsValueInMemory(Context *context, IRValue irValue)
{
	if (irValue.valueType == IRVALUETYPE_MEMORY)
		return true;
	if (irValue.valueType != IRVALUETYPE_VALUE)
		return false;
	Value value = context->values[irValue.valueIdx];
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
		return acceptableOperands & ACCEPTEDOPERANDS_IMMEDIATE;
	if (!IsValueInMemory(context, value))
		return acceptableOperands & ACCEPTEDOPERANDS_REGISTER;
	return acceptableOperands & ACCEPTEDOPERANDS_MEMORY;
}

void X64Mov(Context *context, X64Procedure *x64Proc, IRValue dst, IRValue src)
{
	X64Instruction result;
	TypeInfo dstType = context->typeTable[dst.typeTableIdx];
	TypeInfo srcType = context->typeTable[src.typeTableIdx];

	if (IsValueInMemory(context, dst) && IsValueInMemory(context, src))
	{
		IRValue tmp = IRValueNewValue(context, "_mov_mm_tmp"_s, dst.typeTableIdx, VALUEFLAGS_FORCE_REGISTER);
		X64Mov(context, x64Proc, tmp, src);
		src = tmp;
		srcType = context->typeTable[src.typeTableIdx];
	}

	if (dstType.typeCategory != TYPECATEGORY_FLOATING)
	{
		if (srcType.typeCategory != TYPECATEGORY_FLOATING)
		{
			if (srcType.size == 4)
			{
				result.type = X64_MOV;
				ASSERT(dstType.size >= 4);
				dst.typeTableIdx = src.typeTableIdx;
			}
			else if (srcType.size < dstType.size && src.valueType != IRVALUETYPE_IMMEDIATE_INTEGER)
			{
				result.type = X64_MOV;

				// MOVZX is R-RM
				IRValue newValue = IRValueNewValue(context, "_movzx_tmp"_s, dst.typeTableIdx,
						VALUEFLAGS_FORCE_REGISTER);
				*BucketArrayAdd(&x64Proc->instructions) = { X64_MOVZX, newValue, src };
				src = newValue;
			}
			else if (srcType.size > dstType.size)
			{
				result.type = X64_MOV;
				src.typeTableIdx = dst.typeTableIdx;
			}
			else
				result.type = X64_MOV;
		}
		else if (srcType.size == 4)
			result.type = X64_CVTSS2SI;
		else
		{
			ASSERT(srcType.size == 8);
			result.type = X64_CVTSD2SI;
		}
	}
	else if (dstType.size == 4)
	{
		if (srcType.typeCategory != TYPECATEGORY_FLOATING)
			result.type = X64_CVTSI2SS;
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
			result.type = X64_CVTSI2SD;
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

void X64ConvertInstruction(Context *context, IRInstruction inst, X64Procedure *x64Proc)
{
	X64Instruction result = {};

	X64FloatingType floatingType = X64FLOATINGTYPE_NONE;
	if (inst.type >= IRINSTRUCTIONTYPE_BINARY_BEGIN &&
		inst.type <  IRINSTRUCTIONTYPE_BINARY_END)
	{
		TypeInfo typeInfo = context->typeTable[inst.binaryOperation.out.typeTableIdx];
		if (typeInfo.typeCategory == TYPECATEGORY_FLOATING)
			floatingType = (X64FloatingType)(X64FLOATINGTYPE_F32 + (typeInfo.size == 8));
	}
	else if (inst.type >= IRINSTRUCTIONTYPE_UNARY_BEGIN &&
			 inst.type <  IRINSTRUCTIONTYPE_UNARY_END)
	{
		TypeInfo typeInfo = context->typeTable[inst.unaryOperation.out.typeTableIdx];
		if (typeInfo.typeCategory == TYPECATEGORY_FLOATING)
			floatingType = (X64FloatingType)(X64FLOATINGTYPE_F32 + (typeInfo.size == 8));
	}
	else if (inst.type == IRINSTRUCTIONTYPE_ASSIGNMENT)
	{
		TypeInfo typeInfo = context->typeTable[inst.assignment.dst.typeTableIdx];
		if (typeInfo.typeCategory == TYPECATEGORY_FLOATING)
			floatingType = (X64FloatingType)(X64FLOATINGTYPE_F32 + (typeInfo.size == 8));
	}

	switch (inst.type)
	{
	case IRINSTRUCTIONTYPE_PATCH:
	{
		X64ConvertInstruction(context, *inst.patch.first,  x64Proc);
		X64ConvertInstruction(context, *inst.patch.second, x64Proc);
		return;
	}
	case IRINSTRUCTIONTYPE_PATCH_MANY:
	{
		const s64 instructionCount = inst.patchMany.instructions.size;
		for (s64 i = 0; i < instructionCount; ++i)
			X64ConvertInstruction(context, inst.patchMany.instructions[i], x64Proc);
		return;
	}
	case IRINSTRUCTIONTYPE_ASSIGNMENT:
	{
		X64Mov(context, x64Proc, inst.assignment.dst, inst.assignment.src);
		return;
	}
	case IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS:
	{
		IRValue dst = inst.assignment.dst;
		IRValue src = inst.assignment.src;
		if (IsValueInMemory(context, dst))
		{
			IRValue tmp = IRValueNewValue(context, "_lea_mm_tmp"_s, dst.typeTableIdx, VALUEFLAGS_FORCE_REGISTER);
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
		{
			X64Mov(context, x64Proc, RAX, inst.binaryOperation.left);

			*BucketArrayAdd(&x64Proc->instructions) = { X64_CQO };

			IRValue divisor = inst.binaryOperation.right;
			u8 accepted = x64InstructionInfos[X64_DIV].acceptedOperandsLeft;
			if (!FitsInOperand(context, accepted, divisor))
			{
				ASSERT(accepted & ACCEPTEDOPERANDS_REGISTER);
				IRValue newValue = IRValueNewValue(context, divisor.typeTableIdx,
						VALUEFLAGS_FORCE_REGISTER);
				X64Mov(context, x64Proc, newValue, divisor);
				divisor = newValue;
			}
			result.type = X64_IDIV;
			result.dst = divisor;
			*BucketArrayAdd(&x64Proc->instructions) = result;

			X64Mov(context, x64Proc, inst.binaryOperation.out, RAX);
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
		X64Mov(context, x64Proc, RAX, inst.binaryOperation.left);
		*BucketArrayAdd(&x64Proc->instructions) = { X64_CQO };

		IRValue divisor = inst.binaryOperation.right;
		u8 accepted = x64InstructionInfos[X64_DIV].acceptedOperandsLeft;
		if (!FitsInOperand(context, accepted, divisor))
		{
			ASSERT(accepted & ACCEPTEDOPERANDS_REGISTER);
			IRValue newValue = IRValueNewValue(context, divisor.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER);
			X64Mov(context, x64Proc, newValue, divisor);
			divisor = newValue;
		}
		result.type = X64_IDIV;
		result.dst = divisor;

		*BucketArrayAdd(&x64Proc->instructions) = result;
		X64Mov(context, x64Proc, inst.binaryOperation.out, RDX);
		return;
	}
	case IRINSTRUCTIONTYPE_SHIFT_LEFT:
		result.type = X64_SAL;
		goto doShift;
	case IRINSTRUCTIONTYPE_SHIFT_RIGHT:
		result.type = X64_SAR;
		goto doShift;
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
		IRValue operand = inst.conditionalJump.condition;
		u8 accepted = x64InstructionInfos[X64_CMP].acceptedOperandsLeft;
		if (!FitsInOperand(context, accepted, operand))
		{
			ASSERT(accepted & ACCEPTEDOPERANDS_REGISTER);
			IRValue newValue = IRValueNewValue(context, "_jump_hlp"_s, operand.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER);
			X64Mov(context, x64Proc, newValue, operand);
			operand = newValue;
		}

		X64Instruction cmpInst = { X64_CMP };
		cmpInst.dst = operand;
		cmpInst.src = IRValueImmediate(0);
		result.type = X64_JE;
		result.label = inst.conditionalJump.label;

		*BucketArrayAdd(&x64Proc->instructions) = cmpInst;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	}
	case IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO:
	{
		IRValue operand = inst.conditionalJump.condition;
		u8 accepted = x64InstructionInfos[X64_CMP].acceptedOperandsLeft;
		if (!FitsInOperand(context, accepted, operand))
		{
			ASSERT(accepted & ACCEPTEDOPERANDS_REGISTER);
			IRValue newValue = IRValueNewValue(context, "_jump_hlp"_s, operand.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER);
			X64Mov(context, x64Proc, newValue, operand);
			operand = newValue;
		}

		X64Instruction cmpInst = { X64_CMP };
		cmpInst.dst = operand;
		cmpInst.src = IRValueImmediate(0);
		result.type = X64_JNE;
		result.label = inst.conditionalJump.label;

		*BucketArrayAdd(&x64Proc->instructions) = cmpInst;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	}
	case IRINSTRUCTIONTYPE_GREATER_THAN:
		result.type = X64_SETG;
		goto doConditionalSet;
	case IRINSTRUCTIONTYPE_LESS_THAN:
		result.type = X64_SETL;
		goto doConditionalSet;
	case IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS:
		result.type = X64_SETGE;
		goto doConditionalSet;
	case IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS:
		result.type = X64_SETLE;
		goto doConditionalSet;
	case IRINSTRUCTIONTYPE_EQUALS:
		result.type = X64_SETE;
		goto doConditionalSet;
	case IRINSTRUCTIONTYPE_NOT:
	{
		X64Instruction cmpInst;
		cmpInst.dst = inst.unaryOperation.in;
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
		}

		u8 accepted = x64InstructionInfos[X64_CMP].acceptedOperandsLeft;
		if (!FitsInOperand(context, accepted, cmpInst.dst))
		{
			ASSERT(accepted & ACCEPTEDOPERANDS_REGISTER);
			IRValue newValue = IRValueNewValue(context, "_not_hlp"_s, cmpInst.dst.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER);
			X64Mov(context, x64Proc, newValue, cmpInst.dst);
			cmpInst.dst = newValue;
		}

		result.type = X64_SETE;
		result.dst = inst.unaryOperation.out;
		if (context->typeTable[result.dst.typeTableIdx].size != 1)
		{
			*BucketArrayAdd(&x64Proc->instructions) = { X64_XOR, result.dst, result.dst };
			result.dst.typeTableIdx = TYPETABLEIDX_S8;
		}

		*BucketArrayAdd(&x64Proc->instructions) = cmpInst;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	}
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL:
	{
		// At this point, we have the actual values that go into registers/stack slots. If something
		// is passed by copy, we already have the pointer to the copy as argument value, so we don't
		// care.

		// @Incomplete: implement calling conventions other than MS ABI
		for (int i = 0; i < inst.procedureCall.parameters.size; ++i)
		{
			IRValue dst;
			switch(i)
			{
			case 0:
				dst = RCX;
				break;
			case 1:
				dst = RDX;
				break;
			case 2:
				dst = R8;
				break;
			case 3:
				dst = R9;
				break;
			default:
				dst = IRValueMemory(x64ParameterValuesWrite[i], 0, TYPETABLEIDX_S64);
			}
			X64Mov(context, x64Proc, dst, inst.procedureCall.parameters[i]);
		}

		result.type = X64_CALL;
		String callProcLabel = X64ProcedureToLabel(context, inst.procedureCall.procedure);
		result.procLabel = callProcLabel;
		result.procParameterCount = (s32)inst.procedureCall.procedure->parameters.size;
		*BucketArrayAdd(&x64Proc->instructions) = result;

		if (inst.procedureCall.out.valueType != IRVALUETYPE_INVALID)
			X64Mov(context, x64Proc, inst.procedureCall.out, RAX);
		return;
	}
	case IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY:
	{
		X64Mov(context, x64Proc, RCX, inst.memcpy.dst);
		X64Mov(context, x64Proc, RDX, inst.memcpy.src);
		X64Mov(context, x64Proc, R8,  inst.memcpy.size);
		result.type = X64_CALL;
		result.procParameterCount = 3;
		result.procLabel = "CopyMemory"_s;
		*BucketArrayAdd(&x64Proc->instructions) = result;
		return;
	}
	case IRINSTRUCTIONTYPE_PUSH_SCOPE:
		result.type = X64_Push_Scope;
		return;
	case IRINSTRUCTIONTYPE_POP_SCOPE:
		result.type = X64_Pop_Scope;
		return;
	case IRINSTRUCTIONTYPE_PUSH_VALUE:
		result.type = X64_Push_Value;
		return;
	case IRINSTRUCTIONTYPE_COMMENT:
	case IRINSTRUCTIONTYPE_RETURN:
		return;
	default:
		ASSERT(!"Unrecognized IR instruction type");
		return;
	}

doRM:
	{
		IRValue operand = inst.unaryOperation.out;

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

		IRValue tmp = IRValueNewValue(context, left.typeTableIdx, 0);

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

		IRValue tmp = IRValueNewValue(context, left.typeTableIdx, 0);

		X64Mov(context, x64Proc, tmp, left);

		u8 accepted = x64InstructionInfos[result.type].acceptedOperandsRight;
		if (!FitsInOperand(context, accepted, right))
		{
			ASSERT(accepted & ACCEPTEDOPERANDS_REGISTER);
			IRValue newValue = IRValueNewValue(context, out.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER);
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
		u8 accepted = x64InstructionInfos[X64_CMP].acceptedOperandsLeft;
		if (!FitsInOperand(context, accepted, cmpInst.dst))
		{
			ASSERT(accepted & ACCEPTEDOPERANDS_REGISTER);
			IRValue newValue = IRValueNewValue(context, "_setcc_hlp"_s, cmpInst.dst.typeTableIdx,
					VALUEFLAGS_FORCE_REGISTER);
			X64Mov(context, x64Proc, newValue, cmpInst.dst);
			cmpInst.dst = newValue;
		}

		result.dst = inst.binaryOperation.out;
		if (context->typeTable[result.dst.typeTableIdx].size != 1)
		{
			*BucketArrayAdd(&x64Proc->instructions) = { X64_XOR, result.dst, result.dst };
			result.dst.typeTableIdx = TYPETABLEIDX_S8;
		}

		*BucketArrayAdd(&x64Proc->instructions) = cmpInst;
		*BucketArrayAdd(&x64Proc->instructions) = result;
	}
}

String X64InstructionToStr(Context *context, X64Instruction inst)
{
	String mnemonic = x64InstructionInfos[inst.type].mnemonic;
	switch (inst.type)
	{
	// Two args
	case X64_MOV:
	case X64_MOVZX:
	case X64_LEA:
	case X64_ADD:
	case X64_SUB:
	case X64_SAL:
	case X64_SAR:
	case X64_CMP:
	case X64_IMUL:
	case X64_AND:
	case X64_OR:
	case X64_XOR:
	case X64_MOVSS:
	case X64_MOVSD:
	case X64_ADDSS:
	case X64_ADDSD:
	case X64_SUBSS:
	case X64_SUBSD:
	case X64_MULSS:
	case X64_MULSD:
	case X64_DIVSS:
	case X64_DIVSD:
	case X64_CVTSI2SS:
	case X64_CVTSI2SD:
	case X64_CVTSS2SI:
	case X64_CVTSD2SI:
	case X64_CVTSS2SD:
	case X64_CVTSD2SS:
		goto printDstSrc;
	// One arg
	case X64_PUSH:
	case X64_POP:
	case X64_NEG:
	case X64_NOT:
	case X64_IDIV:
	case X64_SETG:
	case X64_SETL:
	case X64_SETGE:
	case X64_SETLE:
	case X64_SETE:
	case X64_SETNE:
		goto printDst;
	// No args
	case X64_CQO:
	case X64_LEAVE:
	case X64_RET:
		return mnemonic;
	case X64_JMP:
	case X64_JE:
	case X64_JNE:
		goto printLabel;
	case X64_CALL:
		return TPrintF("call %S", inst.procLabel);
	case X64_Label:
		return TPrintF("%S:", inst.label->name);
	case X64_Ignore:
	case X64_Push_Scope:
	case X64_Pop_Scope:
	case X64_Push_Value:
		return {};
	default:
		ASSERT(!"Unrecognized x64 instruction type");
	}
	return "???INST"_s;

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

void X64PrintInstructions(Context *context, Array<X64Procedure> x64Procedures)
{
	for (int procedureIdx = 0; procedureIdx < x64Procedures.size; ++procedureIdx)
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

void X64PrintStaticData(Context *context, String name, IRValue value, s64 typeTableIdx)
{
	if (value.valueType == IRVALUETYPE_IMMEDIATE_STRING)
	{
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
	}
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_FLOAT)
	{
		TypeInfo typeInfo = context->typeTable[typeTableIdx];
		switch (typeInfo.size)
		{
		case 4:
		{
			union { u32 asU32; f32 asF32; };
			asF32 = (f32)value.immediateFloat;
			PrintOut(context, "%S DD %.8xH\n", name, asU32);
		} break;
		case 8:
			PrintOut(context, "%S DQ %.16llxH\n", name,
					value.immediate);
			break;
		default:
			ASSERT(!"Invalid immediate size");
		}
	}
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_STRUCT)
	{
		Array<IRValue> members = value.immediateStructMembers;
		X64PrintStaticData(context, name, members[0], members[0].typeTableIdx);
		for (int memberIdx = 1; memberIdx < members.size; ++memberIdx)
		{
			X64PrintStaticData(context, "   "_s, members[memberIdx],
					members[memberIdx].typeTableIdx);
		}
	}
	else if (value.valueType == IRVALUETYPE_MEMORY)
	{
		Value v = context->values[value.memory.baseValueIdx];
		ASSERT(v.flags & VALUEFLAGS_ON_STATIC_STORAGE);
		ASSERT(v.name.size);
		PrintOut(context, "%S DQ %S\n", name, v.name);
	}
	else if (value.valueType != IRVALUETYPE_INVALID)
	{
		TypeInfo typeInfo = context->typeTable[typeTableIdx];
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
	else
	{
		TypeInfo typeInfo = context->typeTable[typeTableIdx];
		PrintOut(context, "COMM %S:BYTE:0%llxH\n", name,
				typeInfo.size);
	}
}

void BackendMain(Context *context)
{
	BucketArrayInit(&context->patchedInstructions);

	x64InstructionInfos[X64_MOV] =      { "mov"_s,      ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL };
	x64InstructionInfos[X64_MOVZX] =    { "movzx"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_MOVSX] =    { "movsx"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_MOVSXD] =   { "movsxd"_s,   ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_CQO] =      { "cqo"_s,      ACCEPTEDOPERANDS_NONE,     ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_PUSH] =     { "push"_s,     ACCEPTEDOPERANDS_ALL,      ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_POP] =      { "pop"_s,      ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_JMP] =      { "jmp"_s,      ACCEPTEDOPERANDS_ALL,      ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_JE] =       { "je"_s,       ACCEPTEDOPERANDS_ALL,      ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_JNE] =      { "jne"_s,      ACCEPTEDOPERANDS_ALL,      ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_CALL] =     { "call"_s,     ACCEPTEDOPERANDS_ALL,      ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_LEAVE] =    { "leave"_s,    ACCEPTEDOPERANDS_NONE,     ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_RET] =      { "ret"_s,      ACCEPTEDOPERANDS_NONE,     ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_LEA] =      { "lea"_s,      ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_MEMORY };
	x64InstructionInfos[X64_CMP] =      { "cmp"_s,      ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL };
	x64InstructionInfos[X64_SETG] =     { "setg"_s,     ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_SETL] =     { "setl"_s,     ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_SETGE] =    { "setge"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_SETLE] =    { "setle"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_SETE] =     { "sete"_s,     ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_SETNE] =    { "setne"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_ADD] =      { "add"_s,      ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL };
	x64InstructionInfos[X64_SUB] =      { "sub"_s,      ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL };
	x64InstructionInfos[X64_MUL] =      { "mul"_s,      ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_IMUL] =     { "imul"_s,     ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL };
	x64InstructionInfos[X64_DIV] =      { "div"_s,      ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_IDIV] =     { "idiv"_s,     ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_SAR] =      { "sar"_s,      ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL };
	x64InstructionInfos[X64_SAL] =      { "sal"_s,      ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL };
	x64InstructionInfos[X64_AND] =      { "and"_s,      ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL };
	x64InstructionInfos[X64_OR] =       { "or"_s,       ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL };
	x64InstructionInfos[X64_XOR] =      { "xor"_s,      ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL };
	x64InstructionInfos[X64_NOT] =      { "not"_s,      ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_NEG] =      { "neg"_s,      ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_NONE };
	x64InstructionInfos[X64_MOVSS] =    { "movss"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_MOVSD] =    { "movsd"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_ADDSS] =    { "addss"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_ADDSD] =    { "addsd"_s,    ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_SUBSS] =    { "subss"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_SUBSD] =    { "subsd"_s,    ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_MULSS] =    { "mulss"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_MULSD] =    { "mulsd"_s,    ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_DIVSS] =    { "divss"_s,    ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_DIVSD] =    { "divsd"_s,    ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_XORPS] =    { "xorps"_s,    ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_XORPD] =    { "xorpd"_s,    ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_COMISS] =   { "comiss"_s,   ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_COMISD] =   { "comisd"_s,   ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_CVTSI2SS] = { "cvtsi2ss"_s, ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_CVTSI2SD] = { "cvtsi2sd"_s, ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_CVTSS2SI] = { "cvtss2si"_s, ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_CVTSD2SI] = { "cvtsd2si"_s, ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_CVTSS2SD] = { "cvtss2sd"_s, ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_REGMEM };
	x64InstructionInfos[X64_CVTSD2SS] = { "cvtsd2ss"_s, ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_REGMEM };

	u32 RAX_valueIdx = NewValue(context, "RAX"_s, TYPETABLEIDX_S64, VALUEFLAGS_IS_ALLOCATED);
	u32 RCX_valueIdx = NewValue(context, "RCX"_s, TYPETABLEIDX_S64, VALUEFLAGS_IS_ALLOCATED);
	u32 RDX_valueIdx = NewValue(context, "RDX"_s, TYPETABLEIDX_S64, VALUEFLAGS_IS_ALLOCATED);
	u32 RBX_valueIdx = NewValue(context, "RBX"_s, TYPETABLEIDX_S64, VALUEFLAGS_IS_ALLOCATED);
	u32 RSI_valueIdx = NewValue(context, "RSI"_s, TYPETABLEIDX_S64, VALUEFLAGS_IS_ALLOCATED);
	u32 RDI_valueIdx = NewValue(context, "RDI"_s, TYPETABLEIDX_S64, VALUEFLAGS_IS_ALLOCATED);
	u32 RSP_valueIdx = NewValue(context, "RSP"_s, TYPETABLEIDX_S64, VALUEFLAGS_IS_ALLOCATED);
	u32 RBP_valueIdx = NewValue(context, "RBP"_s, TYPETABLEIDX_S64, VALUEFLAGS_IS_ALLOCATED);
	u32 R8_valueIdx  = NewValue(context, "R8"_s,  TYPETABLEIDX_S64, VALUEFLAGS_IS_ALLOCATED);
	u32 R9_valueIdx  = NewValue(context, "R9"_s,  TYPETABLEIDX_S64, VALUEFLAGS_IS_ALLOCATED);
	u32 R10_valueIdx = NewValue(context, "R10"_s, TYPETABLEIDX_S64, VALUEFLAGS_IS_ALLOCATED);
	u32 R11_valueIdx = NewValue(context, "R11"_s, TYPETABLEIDX_S64, VALUEFLAGS_IS_ALLOCATED);
	u32 R12_valueIdx = NewValue(context, "R12"_s, TYPETABLEIDX_S64, VALUEFLAGS_IS_ALLOCATED);
	u32 R13_valueIdx = NewValue(context, "R13"_s, TYPETABLEIDX_S64, VALUEFLAGS_IS_ALLOCATED);
	u32 R14_valueIdx = NewValue(context, "R14"_s, TYPETABLEIDX_S64, VALUEFLAGS_IS_ALLOCATED);
	u32 R15_valueIdx = NewValue(context, "R15"_s, TYPETABLEIDX_S64, VALUEFLAGS_IS_ALLOCATED);

	u32 XMM0_valueIdx = NewValue(context, "XMM0"_s, TYPETABLEIDX_F64, VALUEFLAGS_IS_ALLOCATED);
	u32 XMM1_valueIdx = NewValue(context, "XMM1"_s, TYPETABLEIDX_F64, VALUEFLAGS_IS_ALLOCATED);
	u32 XMM2_valueIdx = NewValue(context, "XMM2"_s, TYPETABLEIDX_F64, VALUEFLAGS_IS_ALLOCATED);
	u32 XMM3_valueIdx = NewValue(context, "XMM3"_s, TYPETABLEIDX_F64, VALUEFLAGS_IS_ALLOCATED);
	u32 XMM4_valueIdx = NewValue(context, "XMM4"_s, TYPETABLEIDX_F64, VALUEFLAGS_IS_ALLOCATED);
	u32 XMM5_valueIdx = NewValue(context, "XMM5"_s, TYPETABLEIDX_F64, VALUEFLAGS_IS_ALLOCATED);
	u32 XMM6_valueIdx = NewValue(context, "XMM6"_s, TYPETABLEIDX_F64, VALUEFLAGS_IS_ALLOCATED);
	u32 XMM7_valueIdx = NewValue(context, "XMM7"_s, TYPETABLEIDX_F64, VALUEFLAGS_IS_ALLOCATED);

	for (int i = 0; i < X64REGISTER_Count; ++i)
	{
		context->values[RAX_valueIdx + i].flags = VALUEFLAGS_IS_ALLOCATED;
		context->values[RAX_valueIdx + i].allocatedRegister = i;
	}

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

	XMM0 = { IRVALUETYPE_VALUE, XMM0_valueIdx, TYPETABLEIDX_F64 };
	XMM1 = { IRVALUETYPE_VALUE, XMM1_valueIdx, TYPETABLEIDX_F64 };
	XMM2 = { IRVALUETYPE_VALUE, XMM2_valueIdx, TYPETABLEIDX_F64 };
	XMM3 = { IRVALUETYPE_VALUE, XMM3_valueIdx, TYPETABLEIDX_F64 };
	XMM4 = { IRVALUETYPE_VALUE, XMM4_valueIdx, TYPETABLEIDX_F64 };
	XMM5 = { IRVALUETYPE_VALUE, XMM5_valueIdx, TYPETABLEIDX_F64 };
	XMM6 = { IRVALUETYPE_VALUE, XMM6_valueIdx, TYPETABLEIDX_F64 };
	XMM7 = { IRVALUETYPE_VALUE, XMM7_valueIdx, TYPETABLEIDX_F64 };

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

	x64ParameterValuesRead[0] = RCX.valueIdx;
	x64ParameterValuesRead[1] = RDX.valueIdx;
	x64ParameterValuesRead[2] = R8.valueIdx;
	x64ParameterValuesRead[3] = R9.valueIdx;
	for (int paramIdx = 4; paramIdx < 32; ++paramIdx)
	{
		u32 newValueIdx = NewValue(context, TPrintF("_param%d", paramIdx), TYPETABLEIDX_S64,
				VALUEFLAGS_IS_ALLOCATED | VALUEFLAGS_IS_MEMORY);
		// Add 16, 8 for return address, and 8 because we push RBP
		// (remember by now we are at parameter index 4+).
		context->values[newValueIdx].stackOffset = 16 + paramIdx * 8;
		x64ParameterValuesRead[paramIdx] = newValueIdx;
	}

	x64ParameterValuesWrite[0] = RCX.valueIdx;
	x64ParameterValuesWrite[1] = RDX.valueIdx;
	x64ParameterValuesWrite[2] = R8.valueIdx;
	x64ParameterValuesWrite[3] = R9.valueIdx;
	for (int paramIdx = 4; paramIdx < 32; ++paramIdx)
	{
		u32 newValueIdx = NewValue(context, TPrintF("_param%d", paramIdx), TYPETABLEIDX_S64,
				VALUEFLAGS_IS_ALLOCATED | VALUEFLAGS_IS_MEMORY);
		// Add 16, 8 for return address, and 8 because we push RBP
		// (remember by now we are at parameter index 4+).
		context->values[newValueIdx].stackOffset = paramIdx * 8;
		x64ParameterValuesWrite[paramIdx] = newValueIdx;
	}

	Array<X64Procedure> x64Procedures;
	u64 procedureCount = BucketArrayCount(&context->procedures);
	ArrayInit(&x64Procedures, procedureCount, malloc);
	x64Procedures.size = procedureCount;

	// Create X64Procedures
	for (int procedureIdx = 0; procedureIdx < procedureCount; ++procedureIdx)
	{
		Procedure *proc = &context->procedures[procedureIdx];
		X64Procedure *x64Proc = &x64Procedures[procedureIdx];

		x64Proc->name = X64ProcedureToLabel(context, proc);
		x64Proc->allocatedParameterCount = proc->allocatedParameterCount;
		x64Proc->returnValueIdx = proc->returnValueIdx;
		x64Proc->stackSize = 0;
		DynamicArrayInit(&x64Proc->spilledValues, 8);

		BucketArrayInit(&x64Proc->instructions);

		// Allocate parameters
#if 0
		for (int paramIdx = 0; paramIdx < proc->parameters.size; ++paramIdx)
		{
			ProcedureParameter param = proc->parameters[paramIdx];
			Value *value = &context->values[param.valueIdx];
			ASSERT((value->flags & (VALUEFLAGS_IS_ALLOCATED | VALUEFLAGS_FORCE_REGISTER |
					VALUEFLAGS_FORCE_MEMORY | VALUEFLAGS_IS_MEMORY)) == 0);

			value->flags |= VALUEFLAGS_IS_ALLOCATED;
			switch (paramIdx)
			{
			case 0:
				value->allocatedRegister = RCX_idx;
				break;
			case 1:
				value->allocatedRegister = RDX_idx;
				break;
			case 2:
				value->allocatedRegister = R8_idx;
				break;
			case 3:
				value->allocatedRegister = R9_idx;
				break;
			default:
				value->flags |= VALUEFLAGS_IS_MEMORY | VALUEFLAGS_BASE_RELATIVE;
				// Add 16, 8 for return address, and 8 because we push RBP
				// (remember by now we are at parameter index 4+).
				value->stackOffset = 16 + paramIdx * 8;
			}
		}
#else
		for (int paramIdx = 0; paramIdx < proc->parameters.size; ++paramIdx)
		{
			ProcedureParameter param = proc->parameters[paramIdx];
			IRValue src;
			switch (paramIdx)
			{
			case 0:
				src = RCX;
				break;
			case 1:
				src = RDX;
				break;
			case 2:
				src = R8;
				break;
			case 3:
				src = R9;
				break;
			default:
				src = IRValueMemory(x64ParameterValuesRead[paramIdx], 0, TYPETABLEIDX_S64);
			}
			X64Mov(context, x64Proc, IRValueValue(context, param.valueIdx), src);
		}
#endif

		u64 instructionCount = BucketArrayCount(&proc->instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			IRInstruction inst = proc->instructions[instructionIdx];
			X64ConvertInstruction(context, inst, x64Proc);
		}

		if (proc->returnValueIdx != U32_MAX)
		{
			IRValue returnValue = IRValueValue(context, proc->returnValueIdx);
			X64Mov(context, x64Proc, RAX, returnValue);
		}
	}

	context->outputFile = CreateFileA(
			"x64_before_allocation.txt",
			GENERIC_WRITE,
			0,
			nullptr,
			CREATE_ALWAYS,
			FILE_ATTRIBUTE_NORMAL,
			nullptr
			);
	X64PrintInstructions(context, x64Procedures);
	CloseHandle(context->outputFile);

	X64AllocateRegisters(context, x64Procedures);

	// Remove instructions that reference unused values
	for (int procedureIdx = 0; procedureIdx < x64Procedures.size; ++procedureIdx)
	{
		X64InstructionStream stream = X64InstructionStreamBegin(&x64Procedures[procedureIdx]);
		X64Instruction *inst = X64InstructionStreamAdvance(&stream);
		while (inst)
		{
			switch (inst->type)
			{
			// dst write, src read
			case X64_MOV:
			case X64_MOVZX:
			case X64_MOVSX:
			case X64_MOVSXD:
			case X64_MOVSS:
			case X64_MOVSD:
			case X64_LEA:
			case X64_CVTSI2SS:
			case X64_CVTSI2SD:
			case X64_CVTSS2SI:
			case X64_CVTSD2SI:
			case X64_CVTSS2SD:
			case X64_CVTSD2SS:
			{
				if (inst->dst.valueType == IRVALUETYPE_VALUE ||
					inst->dst.valueType == IRVALUETYPE_MEMORY)
				{
					Value v = context->values[inst->dst.valueIdx];
					if (!(v.flags & (VALUEFLAGS_IS_ALLOCATED | VALUEFLAGS_ON_STATIC_STORAGE)))
						inst->type = X64_Ignore;
				}
			} break;
			}
			inst = X64InstructionStreamAdvance(&stream);
		}
	}

	context->outputFile = CreateFileA(
			"out.asm",
			GENERIC_WRITE,
			0,
			nullptr,
			CREATE_ALWAYS,
			FILE_ATTRIBUTE_NORMAL,
			nullptr
			);

	PrintOut(context, "include basic.asm\n\n");

	PrintOut(context, "_DATA SEGMENT\n");

	// TypeInfo immediate structs
	{
		s64 structMemberInfoTypeTableIdx = FindTypeInStackByName(context, {}, "TypeInfoStructMember"_s);
		s64 pointerToStructMemberInfoIdx = GetTypeInfoPointerOf(context, structMemberInfoTypeTableIdx);
		s64 typeInfoIdx = FindTypeInStackByName(context, {}, "TypeInfo"_s);
		s64 pointerToTypeInfoIdx = GetTypeInfoPointerOf(context, typeInfoIdx);

		u64 tableSize = BucketArrayCount(&context->typeTable);
		for (u64 typeTableIdx = 1; typeTableIdx < tableSize; ++typeTableIdx)
		{
			TypeInfo typeInfo = context->typeTable[typeTableIdx];

			context->values[typeInfo.valueIdx].typeTableIdx = typeInfoIdx;

			IRStaticVariable newStaticVar = { typeInfo.valueIdx };
			newStaticVar.initialValue.valueType = IRVALUETYPE_IMMEDIATE_STRUCT;

			switch (typeInfo.typeCategory)
			{
			case TYPECATEGORY_INTEGER:
			{
				ArrayInit(&newStaticVar.initialValue.immediateStructMembers, 3, FrameAlloc);
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(0, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(typeInfo.integerInfo.isSigned, TYPETABLEIDX_S32) };
			} break;
			case TYPECATEGORY_FLOATING:
			{
				ArrayInit(&newStaticVar.initialValue.immediateStructMembers, 2, FrameAlloc);
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
						structMemberInfoTypeTableIdx, VALUEFLAGS_ON_STATIC_STORAGE);
				IRStaticVariable membersStaticVar = { membersValueIdx };
				membersStaticVar.initialValue.valueType = IRVALUETYPE_IMMEDIATE_STRUCT;
				ArrayInit(&membersStaticVar.initialValue.immediateStructMembers,
						typeInfo.structInfo.members.size, FrameAlloc);
				for (s64 memberIdx = 0; memberIdx < (s64)typeInfo.structInfo.members.size; ++memberIdx)
				{
					StructMember member = typeInfo.structInfo.members[memberIdx];
					TypeInfo memberType = context->typeTable[member.typeTableIdx];

					IRValue memberImm;
					memberImm.valueType = IRVALUETYPE_IMMEDIATE_STRUCT;
					ArrayInit(&memberImm.immediateStructMembers, 4, FrameAlloc);
					*ArrayAdd(&memberImm.immediateStructMembers) =
						{ IRValueImmediateString(context, member.name) };
					*ArrayAdd(&memberImm.immediateStructMembers) =
						{ IRValueImmediate(memberIdx, TYPETABLEIDX_S64) };
					*ArrayAdd(&memberImm.immediateStructMembers) =
						{ IRValueMemory(memberType.valueIdx, 0, pointerToTypeInfoIdx) };
					*ArrayAdd(&memberImm.immediateStructMembers) =
						{ IRValueImmediate(member.offset, TYPETABLEIDX_S64) };

					*ArrayAdd(&membersStaticVar.initialValue.immediateStructMembers) = memberImm;
				}
				*DynamicArrayAdd(&context->irStaticVariables) = membersStaticVar;

				ArrayInit(&newStaticVar.initialValue.immediateStructMembers, 6, FrameAlloc);
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
				{ IRValueMemory(membersValueIdx, 0, pointerToStructMemberInfoIdx) };
			} break;
			case TYPECATEGORY_ENUM:
			{
				String enumName = "<anonymous enum>"_s;
				StaticDefinition *staticDefStruct = FindStaticDefinitionByTypeTableIdx(context,
						typeTableIdx);
				if (staticDefStruct)
					enumName = staticDefStruct->name;

				TypeInfo enumType = context->typeTable[typeInfo.enumInfo.typeTableIdx];

				ArrayInit(&newStaticVar.initialValue.immediateStructMembers, 4, FrameAlloc);
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(3, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediateString(context, enumName) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueMemory(enumType.valueIdx, 0, pointerToTypeInfoIdx) };
			} break;
			case TYPECATEGORY_POINTER:
			{
				TypeInfo pointedType = context->typeTable[typeInfo.pointerInfo.pointedTypeTableIdx];

				ArrayInit(&newStaticVar.initialValue.immediateStructMembers, 3, FrameAlloc);
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(4, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueMemory(pointedType.valueIdx, 0, pointerToTypeInfoIdx) };
			} break;
			case TYPECATEGORY_ARRAY:
			{
				TypeInfo elementType = context->typeTable[typeInfo.arrayInfo.elementTypeTableIdx];

				ArrayInit(&newStaticVar.initialValue.immediateStructMembers, 4, FrameAlloc);
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(5, TYPETABLEIDX_S8) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(typeInfo.size, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(typeInfo.arrayInfo.count, TYPETABLEIDX_S64) };
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueMemory(elementType.valueIdx, 0, pointerToTypeInfoIdx) };
			} break;
			case TYPECATEGORY_INVALID:
			{
				ArrayInit(&newStaticVar.initialValue.immediateStructMembers, 1, FrameAlloc);
				*ArrayAdd(&newStaticVar.initialValue.immediateStructMembers) =
				{ IRValueImmediate(6, TYPETABLEIDX_S8) };
			} break;
			}
			*DynamicArrayAdd(&context->irStaticVariables) = newStaticVar;
		}
	}

	// String literals
	s64 strCount = (s64)BucketArrayCount(&context->stringLiterals);
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
				}
				++in;
				++i;
				--size; // Don't count backslash for string size.
				first = false;
			}
			else
			{
				*out++ = *in++;

				if (i == str.size - 1 || *in == '\\')
				{
					if (!first) PrintOut(context, ", ");

					*out++ = 0;
					g_memory->framePtr = out;
					PrintOut(context, "'%s'", buffer);
					out = buffer;

					first = false;
				}
			}
		}
		PrintOut(context, "\n");
		g_memory->framePtr = buffer;
	}

	const u64 staticVariableCount = context->irStaticVariables.size;
	for (int staticVariableIdx = 0; staticVariableIdx < staticVariableCount; ++staticVariableIdx)
	{
		IRStaticVariable staticVar = context->irStaticVariables[staticVariableIdx];
		Value value = context->values[staticVar.valueIdx];
		String name = value.name;
		X64PrintStaticData(context, name, staticVar.initialValue, value.typeTableIdx);
	}

	PrintOut(context, "_DATA ENDS\n");

	for (int procedureIdx = 0; procedureIdx < procedureCount; ++procedureIdx)
	{
		Procedure *proc = &context->procedures[procedureIdx];
		StaticDefinition *staticDef = FindStaticDefinitionByProcedure(context, proc);
		if (staticDef)
			PrintOut(context, "PUBLIC %S\n", staticDef->name);
	}

	u64 externalProcedureCount = BucketArrayCount(&context->externalProcedures);
	for (int procedureIdx = 0; procedureIdx < externalProcedureCount; ++procedureIdx)
	{
		Procedure *proc = &context->externalProcedures[procedureIdx];
		StaticDefinition *staticDef = FindStaticDefinitionByProcedure(context, proc);
		if (staticDef)
			PrintOut(context, "EXTRN %S:proc\n", staticDef->name);
	}

	PrintOut(context, "_TEXT SEGMENT\n");

	// Code
	X64PrintInstructions(context, x64Procedures);

	PrintOut(context, "_TEXT ENDS\n");
	PrintOut(context, "END\n");

	CloseHandle(context->outputFile);

	// Run MASM
	PWSTR programFilesPathWstr;
	SHGetKnownFolderPath(FOLDERID_ProgramFilesX86, 0, NULL, &programFilesPathWstr);
	String programFilesPath = StupidStrToString(programFilesPathWstr, FrameAlloc);

	String visualStudioPath = TPrintF("%S\\Microsoft Visual Studio", programFilesPath);
	{
		// Get anything starting with 20...
		String wildcard = TPrintF("%S\\20*", visualStudioPath);
		WIN32_FIND_DATAA foundData = {};
		HANDLE findHandle = FindFirstFileA(StringToCStr(wildcard, FrameAlloc), &foundData);
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
		if (GetFileAttributes(StringToCStr(buildToolsPath, FrameAlloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = buildToolsPath;
		else if (GetFileAttributes(StringToCStr(enterprisePath, FrameAlloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = enterprisePath;
		else if (GetFileAttributes(StringToCStr(professionalPath, FrameAlloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = professionalPath;
		else if (GetFileAttributes(StringToCStr(communityPath, FrameAlloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = communityPath;
		msvcPath = TPrintF("%S\\VC\\Tools\\MSVC", msvcPath);

		String wildcard = StringConcat(msvcPath, "\\*"_s);
		WIN32_FIND_DATAA foundData = {};
		HANDLE findHandle = FindFirstFileA(StringToCStr(wildcard, FrameAlloc), &foundData);
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
		HANDLE findHandle = FindFirstFileA(StringToCStr(wildcard, FrameAlloc), &foundData);
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

	String subsystemArgument;
	if (context->config.windowsSubsystem)
		subsystemArgument = "/subsystem:WINDOWS "_s;
	else
		subsystemArgument = "/subsystem:CONSOLE "_s;

	String commandLine = TPrintF(
			"%S\\bin\\Hostx64\\x64\\ml64.exe "
			"out.asm "
			"/nologo "
			"/Zd "
			"/Zi "
			"/I \"%S\\include\" "
			"/I \"%S\\include\\%S\\ucrt\" "
			"/I \"%S\\include\\%S\\shared\" "
			"/I \"%S\\include\\%S\\um\" "
			"/I \"%S\\include\\%S\\winrt\" "
			"/I \"%S\\include\\%S\\cppwinrt\" "
			"/link "
			"%S "
			"kernel32.lib "
			"user32.lib "
			"gdi32.lib "
			"winmm.lib "
			"/nologo "
			"/entry:Main "
			"/libpath:\"%S\\lib\\x64\" "
			"/libpath:\"%S\\lib\\%S\\ucrt\\x64\" "
			"/libpath:\"%S\\lib\\%S\\um\\x64\" "
			"/out:bin/out.exe%c",
			msvcPath,
			msvcPath,
			windowsSDKPath, windowsSDKVersion,
			windowsSDKPath, windowsSDKVersion,
			windowsSDKPath, windowsSDKVersion,
			windowsSDKPath, windowsSDKVersion,
			windowsSDKPath, windowsSDKVersion,
			subsystemArgument,
			msvcPath,
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
			NULL,
			&startupInfo,
			&processInformation
			))
	{
		Print("Failed to call ml64.exe (%d)\n", GetLastError());
		CRASH;
	}
	WaitForSingleObject(processInformation.hProcess, INFINITE);
	CloseHandle(processInformation.hProcess);
	CloseHandle(processInformation.hThread);
}
