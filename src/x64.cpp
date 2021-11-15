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

const X64InstructionInfo MOV    = { "mov"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0 };
const X64InstructionInfo MOVZX  = { "movzx"_s,  ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0 };
const X64InstructionInfo MOVSX  = { "movsx"_s,  ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM, 0};
const X64InstructionInfo MOVSXD = { "movsxd"_s, ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_REGMEM, 0};
const X64InstructionInfo LEA    = { "lea"_s,    ACCEPTEDOPERANDS_REGISTER, ACCEPTEDOPERANDS_MEMORY, INSTRUCTIONFLAGS_DONT_CALCULATE_ADDRESSES };
const X64InstructionInfo CMP    = { "cmp"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0};
const X64InstructionInfo ADD    = { "add"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0};
const X64InstructionInfo SUB    = { "sub"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0};
const X64InstructionInfo IMUL   = { "imul"_s,   ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0};
const X64InstructionInfo SAR    = { "sar"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0};
const X64InstructionInfo SAL    = { "sal"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0};
const X64InstructionInfo AND    = { "and"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0};
const X64InstructionInfo OR     = { "or"_s,     ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0};
const X64InstructionInfo XOR    = { "xor"_s,    ACCEPTEDOPERANDS_REGMEM,   ACCEPTEDOPERANDS_ALL,    0};

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

String X64RegisterToStr(s64 registerIdx, s64 size)
{
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

	String result = "???REG"_s;
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
			result = "???REG"_s;
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
			result = "???REG"_s;
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
			result = "???REG"_s;
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
			result = "???REG"_s;
		}
		break;
	default:
		ASSERT(!"Invalid size for a register!");
	}
	return result;
}

String X64IRValueToStr(Context *context, IRValue value)
{
	String result = "???VALUE"_s;

	TypeInfo *typeInfo = &context->typeTable[value.typeTableIdx];
	u64 size = typeInfo->size;
	//size = 8;

	if (value.valueType == IRVALUETYPE_REGISTER)
	{
		result = X64RegisterToStr(value.registerIdx, size);
	}
	else if (value.valueType == IRVALUETYPE_MEMORY_REGISTER)
	{
		result = X64RegisterToStr(value.memory.baseRegister, 8);
		if (value.memory.offset)
			if (value.memory.offset > 0)
				result = TPrintF("%S+%llu", result, value.memory.offset);
			else
				result = TPrintF("%S-%llu", result, -value.memory.offset);
	}
	else if (value.valueType == IRVALUETYPE_MEMORY_VARIABLE)
	{
		s64 offset = value.memory.offset;

		if (value.memory.baseVariable->isStatic)
			result = TPrintF("%S", value.memory.baseVariable->name);
		else
		{
			result = X64RegisterToStr(RSP_idx, 8);
			ASSERT(value.memory.baseVariable->isAllocated);
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
	// No memory - memory
	if ((first.valueType  == IRVALUETYPE_MEMORY_REGISTER || first.valueType  == IRVALUETYPE_MEMORY_VARIABLE) &&
		(second.valueType == IRVALUETYPE_MEMORY_REGISTER || second.valueType == IRVALUETYPE_MEMORY_VARIABLE))
	{
		IRValue sizedRCX = IRValueRegister(RCX_idx, second.typeTableIdx);
		X64OutputInstruction(context, outputFile, MOV, sizedRCX, second);
		second = sizedRCX;
	}

	String firstStr;
	if (instInfo.acceptedOperandsLeft & IRValueTypeToFlags(first.valueType))
	{
		// Type of operator allowed
		firstStr = X64IRValueToStr(context, first);
	}
	else
	{
		// Type of operator not allowed, copy to register
		ASSERT(instInfo.acceptedOperandsLeft & ACCEPTEDOPERANDS_REGISTER);
		IRValue sizedRSI = IRValueRegister(RSI_idx, first.typeTableIdx);
		X64OutputInstruction(context, outputFile, MOV, sizedRSI, first);
		firstStr = X64RegisterToStr(RSI_idx, context->typeTable[first.typeTableIdx].size);
	}

	String secondStr;
	if (instInfo.acceptedOperandsRight & IRValueTypeToFlags(second.valueType))
	{
		// Type of operator allowed
		secondStr = X64IRValueToStr(context, second);
	}
	else
	{
		// Type of operator not allowed, copy to register
		ASSERT(instInfo.acceptedOperandsRight & ACCEPTEDOPERANDS_REGISTER);
		IRValue sizedRDI = IRValueRegister(RDI_idx, second.typeTableIdx);
		X64OutputInstruction(context, outputFile, MOV, sizedRDI, second);
		secondStr = X64RegisterToStr(RDI_idx, context->typeTable[second.typeTableIdx].size);
	}

	PrintOut(context, outputFile, "%S %S, %S\n", instInfo.mnemonic, firstStr, secondStr);
}

void X64OutputInstruction(Context *context, HANDLE outputFile, X64InstructionInfo instInfo,
		IRValue first)
{
	String firstStr;
	if (instInfo.acceptedOperandsLeft & IRValueTypeToFlags(first.valueType))
		firstStr = X64IRValueToStr(context, first);
	else
	{
		ASSERT(instInfo.acceptedOperandsLeft & ACCEPTEDOPERANDS_REGISTER);
		X64OutputInstruction(context, outputFile, MOV, RAX, first);
		firstStr = "rax"_s;
	}

	PrintOut(context, outputFile, "%S %S\n", instInfo.mnemonic, firstStr);
}

void X64OutputInstruction3(Context *context, HANDLE outputFile, X64InstructionInfo instInfo,
		IRValue out, IRValue left, IRValue right)
{
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

	X64OutputInstruction(context, outputFile, MOV, out, copyToOut);

	String operandStr = X64IRValueToStr(context, operand);
	String outStr = X64IRValueToStr(context, out);
	PrintOut(context, outputFile, "%S %S, %S\n", instInfo.mnemonic, outStr, operandStr);
}

void X64ProcessInstruction(Context *context, HANDLE outputFile, IRInstruction inst)
{
	switch (inst.type)
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
		X64OutputInstruction(context, outputFile, MOV, inst.assignment.dst, inst.assignment.src);
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
	case IRINSTRUCTIONTYPE_ADD:
	{
		X64OutputInstruction3(context, outputFile, ADD, inst.binaryOperation.out,
				inst.binaryOperation.left, inst.binaryOperation.right);
	} break;
	case IRINSTRUCTIONTYPE_SUBTRACT:
	{
		X64OutputInstruction3(context, outputFile, SUB, inst.binaryOperation.out,
				inst.binaryOperation.left, inst.binaryOperation.right);
	} break;
	case IRINSTRUCTIONTYPE_MULTIPLY:
	{
		X64OutputInstruction3(context, outputFile, IMUL, inst.binaryOperation.out,
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
		X64OutputInstruction3(context, outputFile, SAL, inst.binaryOperation.out,
				inst.binaryOperation.left, inst.binaryOperation.right);
	} break;
	case IRINSTRUCTIONTYPE_SHIFT_RIGHT:
	{
		X64OutputInstruction3(context, outputFile, SAR, inst.binaryOperation.out,
				inst.binaryOperation.left, inst.binaryOperation.right);
	} break;
	case IRINSTRUCTIONTYPE_BITWISE_AND:
	{
		X64OutputInstruction3(context, outputFile, AND, inst.binaryOperation.out,
				inst.binaryOperation.left, inst.binaryOperation.right);
	} break;
	case IRINSTRUCTIONTYPE_BITWISE_OR:
	{
		X64OutputInstruction3(context, outputFile, OR, inst.binaryOperation.out,
				inst.binaryOperation.left, inst.binaryOperation.right);
	} break;
	case IRINSTRUCTIONTYPE_BITWISE_XOR:
	{
		X64OutputInstruction3(context, outputFile, XOR, inst.binaryOperation.out,
				inst.binaryOperation.left, inst.binaryOperation.right);
	} break;
	case IRINSTRUCTIONTYPE_GREATER_THAN:
	{
		String left = X64IRValueToStr(context, inst.binaryOperation.left);
		String right = X64IRValueToStr(context, inst.binaryOperation.right);
		String out = X64IRValueToStr(context, inst.binaryOperation.out);
		X64OutputInstruction(context, outputFile, CMP, inst.binaryOperation.left,
				inst.binaryOperation.right);
		PrintOut(context, outputFile, "setg al\n");
		IRValue sizedRAX = IRValueRegister(RAX_idx, inst.binaryOperation.out.typeTableIdx);
		X64OutputInstruction(context, outputFile, MOV, inst.binaryOperation.out, sizedRAX);
	} break;
	case IRINSTRUCTIONTYPE_LESS_THAN:
	{
		String left = X64IRValueToStr(context, inst.binaryOperation.left);
		String right = X64IRValueToStr(context, inst.binaryOperation.right);
		String out = X64IRValueToStr(context, inst.binaryOperation.out);
		X64OutputInstruction(context, outputFile, CMP, inst.binaryOperation.left,
				inst.binaryOperation.right);
		PrintOut(context, outputFile, "setl al\n");
		IRValue sizedRAX = IRValueRegister(RAX_idx, inst.binaryOperation.out.typeTableIdx);
		X64OutputInstruction(context, outputFile, MOV, inst.binaryOperation.out, sizedRAX);
	} break;
	case IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS:
	{
		String left = X64IRValueToStr(context, inst.binaryOperation.left);
		String right = X64IRValueToStr(context, inst.binaryOperation.right);
		String out = X64IRValueToStr(context, inst.binaryOperation.out);
		X64OutputInstruction(context, outputFile, CMP, inst.binaryOperation.left,
				inst.binaryOperation.right);
		PrintOut(context, outputFile, "setge al\n");
		IRValue sizedRAX = IRValueRegister(RAX_idx, inst.binaryOperation.out.typeTableIdx);
		X64OutputInstruction(context, outputFile, MOV, inst.binaryOperation.out, sizedRAX);
	} break;
	case IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS:
	{
		String left = X64IRValueToStr(context, inst.binaryOperation.left);
		String right = X64IRValueToStr(context, inst.binaryOperation.right);
		String out = X64IRValueToStr(context, inst.binaryOperation.out);
		X64OutputInstruction(context, outputFile, CMP, inst.binaryOperation.left,
				inst.binaryOperation.right);
		PrintOut(context, outputFile, "setle al\n");
		IRValue sizedRAX = IRValueRegister(RAX_idx, inst.binaryOperation.out.typeTableIdx);
		X64OutputInstruction(context, outputFile, MOV, inst.binaryOperation.out, sizedRAX);
	} break;
	case IRINSTRUCTIONTYPE_EQUALS:
	{
		String left = X64IRValueToStr(context, inst.binaryOperation.left);
		String right = X64IRValueToStr(context, inst.binaryOperation.right);
		String out = X64IRValueToStr(context, inst.binaryOperation.out);
		X64OutputInstruction(context, outputFile, CMP, inst.binaryOperation.left,
				inst.binaryOperation.right);
		PrintOut(context, outputFile, "sete al\n");
		IRValue sizedRAX = IRValueRegister(RAX_idx, inst.binaryOperation.out.typeTableIdx);
		X64OutputInstruction(context, outputFile, MOV, inst.binaryOperation.out, sizedRAX);
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
		String in = X64IRValueToStr(context, inst.unaryOperation.in);
		String out = X64IRValueToStr(context, inst.unaryOperation.out);
		X64OutputInstruction(context, outputFile, MOV, inst.unaryOperation.out, inst.unaryOperation.in);
		PrintOut(context, outputFile, "not %S\n", out);
	} break;
	case IRINSTRUCTIONTYPE_SUBTRACT_UNARY:
	{
		String in = X64IRValueToStr(context, inst.unaryOperation.in);
		String out = X64IRValueToStr(context, inst.unaryOperation.out);
		X64OutputInstruction(context, outputFile, MOV, inst.unaryOperation.out, inst.unaryOperation.in);
		PrintOut(context, outputFile, "neg %S\n", out);
	} break;
	case IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS:
	{
		IRValue pointer = inst.unaryOperation.in;
		String in = X64IRValueToStr(context, pointer);
		String out = X64IRValueToStr(context, inst.unaryOperation.out);
		X64OutputInstruction(context, outputFile, LEA, inst.unaryOperation.out, inst.unaryOperation.in);
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
}

void WriteToX64(Context *context)
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
