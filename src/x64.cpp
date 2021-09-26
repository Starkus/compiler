enum AcceptedOperandFlags
{
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
	u8 ignoreSizeDifference;
};

const X64InstructionInfo MOV    = { "mov"_s,    ACCEPTEDOPERANDS_REGMEM, ACCEPTEDOPERANDS_ALL,    false };
const X64InstructionInfo MOVSX  = { "movsx"_s,  ACCEPTEDOPERANDS_REGMEM, ACCEPTEDOPERANDS_REGMEM, true };
const X64InstructionInfo MOVSXD = { "movsxd"_s, ACCEPTEDOPERANDS_REGMEM, ACCEPTEDOPERANDS_REGMEM, true };
const X64InstructionInfo CMP    = { "cmp"_s,    ACCEPTEDOPERANDS_REGMEM, ACCEPTEDOPERANDS_ALL,    false };
const X64InstructionInfo ADD    = { "add"_s,    ACCEPTEDOPERANDS_REGMEM, ACCEPTEDOPERANDS_ALL,    false };
const X64InstructionInfo SUB    = { "sub"_s,    ACCEPTEDOPERANDS_REGMEM, ACCEPTEDOPERANDS_ALL,    false };

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

const IRValue RAX = { IRVALUETYPE_REGISTER, RAX_idx, false, TYPETABLEIDX_S64 };
const IRValue RCX = { IRVALUETYPE_REGISTER, RCX_idx, false, TYPETABLEIDX_S64 };
const IRValue RDX = { IRVALUETYPE_REGISTER, RDX_idx, false, TYPETABLEIDX_S64 };
const IRValue RBX = { IRVALUETYPE_REGISTER, RBX_idx, false, TYPETABLEIDX_S64 };
const IRValue RSI = { IRVALUETYPE_REGISTER, RSI_idx, false, TYPETABLEIDX_S64 };
const IRValue RDI = { IRVALUETYPE_REGISTER, RDI_idx, false, TYPETABLEIDX_S64 };
const IRValue RSP = { IRVALUETYPE_REGISTER, RSP_idx, false, TYPETABLEIDX_S64 };
const IRValue RBP = { IRVALUETYPE_REGISTER, RBP_idx, false, TYPETABLEIDX_S64 };
const IRValue R8  = { IRVALUETYPE_REGISTER, R8_idx,  false, TYPETABLEIDX_S64 };
const IRValue R9  = { IRVALUETYPE_REGISTER, R9_idx,  false, TYPETABLEIDX_S64 };
const IRValue R10 = { IRVALUETYPE_REGISTER, R10_idx, false, TYPETABLEIDX_S64 };
const IRValue R11 = { IRVALUETYPE_REGISTER, R11_idx, false, TYPETABLEIDX_S64 };
const IRValue R12 = { IRVALUETYPE_REGISTER, R12_idx, false, TYPETABLEIDX_S64 };
const IRValue R13 = { IRVALUETYPE_REGISTER, R13_idx, false, TYPETABLEIDX_S64 };
const IRValue R14 = { IRVALUETYPE_REGISTER, R14_idx, false, TYPETABLEIDX_S64 };
const IRValue R15 = { IRVALUETYPE_REGISTER, R15_idx, false, TYPETABLEIDX_S64 };

void PrintOut(Context *context, const char *format, ...)
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
	WriteFile(context->outputFile, buffer, (DWORD)strlen(buffer), &bytesWritten, nullptr);

	va_end(args);
}

inline u8 IRValueTypeToFlags(IRValueType type)
{
	switch (type)
	{
	case IRVALUETYPE_REGISTER:
	case IRVALUETYPE_PARAMETER:
		return ACCEPTEDOPERANDS_REGISTER;
	case IRVALUETYPE_STACK_OFFSET:
	case IRVALUETYPE_DATA_OFFSET:
	case IRVALUETYPE_IMMEDIATE_FLOAT:
	case IRVALUETYPE_IMMEDIATE_STRING:
	case IRVALUETYPE_TYPEOF:
		return ACCEPTEDOPERANDS_MEMORY;
	case IRVALUETYPE_IMMEDIATE_INTEGER:
	case IRVALUETYPE_SIZEOF:
		return ACCEPTEDOPERANDS_IMMEDIATE;
	}
	ASSERT(!"Couldn't convert IRValueType to a kind of x64 operand");
	return 0;
}

String X64IRValueToStr(Context *context, IRValue value)
{
	String result = "???VALUE"_s;

	u64 size = 8;
	//TypeInfo *typeInfo = &context->typeTable[value.typeTableIdx];
	//size = typeInfo->size;

	if (value.valueType == IRVALUETYPE_REGISTER)
	{
		switch (size)
		{
			case 8:
				switch (value.registerIdx)
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
				default:
					result = "rax"_s;
					break;
				}
				break;
			case 4:
				switch (value.registerIdx)
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
				default:
					result = "eax"_s;
					break;
				}
				break;
			case 2:
				switch (value.registerIdx)
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
				default:
					result = "ax"_s;
					break;
				}
				break;
			case 1:
				switch (value.registerIdx)
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
				default:
					result = "al"_s;
					break;
				}
				break;
		}
	}
	else if (value.valueType == IRVALUETYPE_PARAMETER)
	{
		switch (value.parameterIdx)
		{
		case 0:
			result = "rcx"_s;
			break;
		case 1:
			result = "rdx"_s;
			break;
		case 2:
			result = "r8"_s;
			break;
		case 3:
			result = "r9"_s;
			break;
		}
	}
	else if (value.valueType == IRVALUETYPE_STACK_OFFSET)
	{
		if (value.stackOffset)
			result = TPrintF("rbp-%llu", value.stackOffset);
		else
			result = "rbp"_s;
	}
	else if (value.valueType == IRVALUETYPE_DATA_OFFSET)
	{
		result = TPrintF("%S+%llu", value.dataOffset.variable->name, value.dataOffset.offset);
	}
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
	{
		result = TPrintF("%lld", value.immediate);
	}
	else if (value.valueType == IRVALUETYPE_SIZEOF)
	{
		result = TPrintF("%llu", size);
	}
	else if (value.valueType == IRVALUETYPE_TYPEOF)
	{
		result = TPrintF("_typeInfo%lld", value.typeOfTypeTableIdx);
	}
	else
		ASSERT(!"Invalid value type!");

	if (value.dereference)
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
	else if (IRValueTypeToFlags(value.valueType) == ACCEPTEDOPERANDS_MEMORY)
	{
		switch (size)
		{
		case 1:
			result = TPrintF("BYTE PTR %S", result);
			break;
		case 2:
			result = TPrintF("WORD PTR %S", result);
			break;
		case 4:
			result = TPrintF("DWORD PTR %S", result);
			break;
		case 8:
		default:
			result = TPrintF("QWORD PTR %S", result);
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

void X64OutputInstruction(Context *context, X64InstructionInfo instInfo, IRValue first,
		IRValue second)
{
	// No memory - memory
	if ((first.dereference || IRValueTypeToFlags(first.valueType) == ACCEPTEDOPERANDS_MEMORY) &&
		(second.dereference || IRValueTypeToFlags(second.valueType) == ACCEPTEDOPERANDS_MEMORY))
	{
		X64OutputInstruction(context, MOV, RCX, second);
		second = RCX;
	}

	// Do memory offset manually if not used as pointer
	if (first.valueType == IRVALUETYPE_STACK_OFFSET && !first.dereference)
	{
		X64OutputInstruction(context, MOV, RBX, RBP);
		X64OutputInstruction(context, SUB, RBX, RBP);
		first = RBX;
	}

	String firstStr;
	if (instInfo.acceptedOperandsLeft & IRValueTypeToFlags(first.valueType))
		firstStr = X64IRValueToStr(context, first);
	else
	{
		ASSERT(instInfo.acceptedOperandsLeft & ACCEPTEDOPERANDS_REGISTER);
		X64OutputInstruction(context, MOV, RBX, first);
		firstStr = "rbx"_s;
	}

	if (!instInfo.ignoreSizeDifference)
	{
	}

	// Do memory offset manually if not used as pointer
	if (second.valueType == IRVALUETYPE_STACK_OFFSET && !second.dereference)
	{
		X64OutputInstruction(context, MOV, RCX, RBP);
		X64OutputInstruction(context, SUB, RCX, RBP);
		second = RCX;
	}

	String secondStr;
	if (instInfo.acceptedOperandsRight & IRValueTypeToFlags(second.valueType))
		secondStr = X64IRValueToStr(context, second);
	else
	{
		ASSERT(instInfo.acceptedOperandsRight & ACCEPTEDOPERANDS_REGISTER);
		X64OutputInstruction(context, MOV, RCX, second);
		secondStr = "rcx"_s;
	}

	PrintOut(context, "%S %S, %S\n", instInfo.mnemonic, firstStr, secondStr);
}

void X64OutputInstruction(Context *context, X64InstructionInfo instInfo, IRValue first)
{
	String firstStr;
	if (instInfo.acceptedOperandsLeft & IRValueTypeToFlags(first.valueType))
		firstStr = X64IRValueToStr(context, first);
	else
	{
		ASSERT(instInfo.acceptedOperandsLeft & ACCEPTEDOPERANDS_REGISTER);
		X64OutputInstruction(context, MOV, RBX, first);
		firstStr = "rbx"_s;
	}

	PrintOut(context, "%S %S\n", instInfo.mnemonic, firstStr);
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

	PrintOut(context, "include basic.asm\n\n");

	PrintOut(context, "_DATA SEGMENT\n");

	const u64 staticVariableCount = context->irStaticVariables.size;
	for (int staticVariableIdx = 0; staticVariableIdx < staticVariableCount; ++staticVariableIdx)
	{
		IRStaticVariable staticVar = context->irStaticVariables[staticVariableIdx];

		TypeInfo *typeInfo  = &context->typeTable[staticVar.variable->typeTableIdx];

		if (staticVar.initialValue.valueType == IRVALUETYPE_IMMEDIATE_STRING)
		{
			// @Cleanup
			PrintOut(context, outputFile, "_str_%S DB '%S'\n", staticVar.variable->name,
					staticVar.initialValue.immediateString);
			PrintOut(context, outputFile, "%S DQ %.16llxH\n", staticVar.variable->name,
					staticVar.initialValue.immediateString.size);
			PrintOut(context, outputFile, "\tDQ _str_%S\n", staticVar.variable->name);
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

	PrintOut(context, "_DATA ENDS\n");

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

	PrintOut(context, "_TEXT SEGMENT\n");

	for (int procedureIdx = 0; procedureIdx < procedureCount; ++procedureIdx)
	{
		Procedure *proc = &context->procedures[procedureIdx];

		// @Speed: separate array of external procedures to avoid branching
		if (proc->isExternal)
			continue;

		String procLabel = X64ProcedureToLabel(context, proc);
		PrintOut(context, "\n%S PROC\n", procLabel);

		PrintOut(context, "push rbp\n");
		PrintOut(context, "mov rbp, rsp\n");

		if (proc->stackSize)
			PrintOut(context, "sub rsp, %llu\n", proc->stackSize);

		u64 instructionCount = BucketArrayCount(&proc->instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			IRInstruction inst = proc->instructions[instructionIdx];

			switch (inst.type)
			{
			case IRINSTRUCTIONTYPE_COMMENT:
			{
				PrintOut(context, "; %S\n", inst.comment);
			} break;
			case IRINSTRUCTIONTYPE_ASSIGNMENT:
			{
				X64OutputInstruction(context, MOV, inst.assignment.dst, inst.assignment.src);
			} break;
			case IRINSTRUCTIONTYPE_ADD:
			{
				String left = X64IRValueToStr(context, inst.binaryOperation.left);
				String right = X64IRValueToStr(context, inst.binaryOperation.right);
				String out = X64IRValueToStr(context, inst.binaryOperation.out);
				X64OutputInstruction(context, MOV, inst.binaryOperation.out, inst.binaryOperation.left);
				PrintOut(context, "add %S, %S\n", out, right);
			} break;
			case IRINSTRUCTIONTYPE_SUBTRACT:
			{
				String left = X64IRValueToStr(context, inst.binaryOperation.left);
				String right = X64IRValueToStr(context, inst.binaryOperation.right);
				String out = X64IRValueToStr(context, inst.binaryOperation.out);
				X64OutputInstruction(context, MOV, inst.binaryOperation.out, inst.binaryOperation.left);
				PrintOut(context, "sub %S, %S\n", out, right);
			} break;
			case IRINSTRUCTIONTYPE_MULTIPLY:
			{
				String left = X64IRValueToStr(context, inst.binaryOperation.left);
				String right = X64IRValueToStr(context, inst.binaryOperation.right);
				String out = X64IRValueToStr(context, inst.binaryOperation.out);
				X64OutputInstruction(context, MOV, inst.binaryOperation.out, inst.binaryOperation.left);
				PrintOut(context, "imul %S, %S\n", out, right);
			} break;
			case IRINSTRUCTIONTYPE_DIVIDE:
			{
				String left = X64IRValueToStr(context, inst.binaryOperation.left);
				String right = X64IRValueToStr(context, inst.binaryOperation.right);
				String out = X64IRValueToStr(context, inst.binaryOperation.out);
				X64OutputInstruction(context, MOV, inst.binaryOperation.out, inst.binaryOperation.left);
				X64OutputInstruction(context, MOV, RAX, inst.binaryOperation.left);
				PrintOut(context, "cqo\n");
				if (inst.binaryOperation.right.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
				{
					X64OutputInstruction(context, MOV, RCX, inst.binaryOperation.right);
					PrintOut(context, "idiv rcx\n");
				}
				else
					PrintOut(context, "idiv %S\n", right);
				X64OutputInstruction(context, MOV, inst.binaryOperation.out, RAX);
			} break;
			case IRINSTRUCTIONTYPE_MODULO:
			{
				String left = X64IRValueToStr(context, inst.binaryOperation.left);
				String right = X64IRValueToStr(context, inst.binaryOperation.right);
				String out = X64IRValueToStr(context, inst.binaryOperation.out);
				X64OutputInstruction(context, MOV, RAX, inst.binaryOperation.left);
				PrintOut(context, "cqo\n");
				if (inst.binaryOperation.right.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
				{
					X64OutputInstruction(context, MOV, RCX, inst.binaryOperation.right);
					PrintOut(context, "idiv rcx\n");
				}
				else
					PrintOut(context, "idiv %S\n", right);
				X64OutputInstruction(context, MOV, inst.binaryOperation.out, RDX);
			} break;
			case IRINSTRUCTIONTYPE_SHIFT_LEFT:
			{
				String left = X64IRValueToStr(context, inst.binaryOperation.left);
				String right = X64IRValueToStr(context, inst.binaryOperation.right);
				String out = X64IRValueToStr(context, inst.binaryOperation.out);
				X64OutputInstruction(context, MOV, inst.binaryOperation.out, inst.binaryOperation.left);
				PrintOut(context, "sal %S, %S\n", out, right);
			} break;
			case IRINSTRUCTIONTYPE_SHIFT_RIGHT:
			{
				String left = X64IRValueToStr(context, inst.binaryOperation.left);
				String right = X64IRValueToStr(context, inst.binaryOperation.right);
				String out = X64IRValueToStr(context, inst.binaryOperation.out);
				X64OutputInstruction(context, MOV, inst.binaryOperation.out, inst.binaryOperation.left);
				PrintOut(context, "sar %S, %S\n", out, right);
			} break;
			case IRINSTRUCTIONTYPE_BITWISE_AND:
			{
				String left = X64IRValueToStr(context, inst.binaryOperation.left);
				String right = X64IRValueToStr(context, inst.binaryOperation.right);
				String out = X64IRValueToStr(context, inst.binaryOperation.out);
				X64OutputInstruction(context, MOV, inst.binaryOperation.out, inst.binaryOperation.left);
				PrintOut(context, "and %S, %S\n", out, right);
			} break;
			case IRINSTRUCTIONTYPE_BITWISE_OR:
			{
				String left = X64IRValueToStr(context, inst.binaryOperation.left);
				String right = X64IRValueToStr(context, inst.binaryOperation.right);
				String out = X64IRValueToStr(context, inst.binaryOperation.out);
				X64OutputInstruction(context, MOV, inst.binaryOperation.out, inst.binaryOperation.left);
				PrintOut(context, "or %S, %S\n", out, right);
			} break;
			case IRINSTRUCTIONTYPE_BITWISE_XOR:
			{
				String left = X64IRValueToStr(context, inst.binaryOperation.left);
				String right = X64IRValueToStr(context, inst.binaryOperation.right);
				String out = X64IRValueToStr(context, inst.binaryOperation.out);
				X64OutputInstruction(context, MOV, inst.binaryOperation.out, inst.binaryOperation.left);
				PrintOut(context, "xor %S, %S\n", out, right);
			} break;
			case IRINSTRUCTIONTYPE_GREATER_THAN:
			{
				String left = X64IRValueToStr(context, inst.binaryOperation.left);
				String right = X64IRValueToStr(context, inst.binaryOperation.right);
				String out = X64IRValueToStr(context, inst.binaryOperation.out);
				X64OutputInstruction(context, CMP, inst.binaryOperation.left,
						inst.binaryOperation.right);
				PrintOut(context, "setg al\n");
				X64OutputInstruction(context, MOV, inst.binaryOperation.out, RAX);
			} break;
			case IRINSTRUCTIONTYPE_LESS_THAN:
			{
				String left = X64IRValueToStr(context, inst.binaryOperation.left);
				String right = X64IRValueToStr(context, inst.binaryOperation.right);
				String out = X64IRValueToStr(context, inst.binaryOperation.out);
				X64OutputInstruction(context, CMP, inst.binaryOperation.left,
						inst.binaryOperation.right);
				PrintOut(context, "setg al\n");
				X64OutputInstruction(context, MOV, inst.binaryOperation.out, RAX);
			} break;
			case IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS:
			{
				String left = X64IRValueToStr(context, inst.binaryOperation.left);
				String right = X64IRValueToStr(context, inst.binaryOperation.right);
				String out = X64IRValueToStr(context, inst.binaryOperation.out);
				X64OutputInstruction(context, CMP, inst.binaryOperation.left,
						inst.binaryOperation.right);
				PrintOut(context, "setge al\n");
				X64OutputInstruction(context, MOV, inst.binaryOperation.out, RAX);
			} break;
			case IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS:
			{
				String left = X64IRValueToStr(context, inst.binaryOperation.left);
				String right = X64IRValueToStr(context, inst.binaryOperation.right);
				String out = X64IRValueToStr(context, inst.binaryOperation.out);
				X64OutputInstruction(context, CMP, inst.binaryOperation.left,
						inst.binaryOperation.right);
				PrintOut(context, "setge al\n");
				X64OutputInstruction(context, MOV, inst.binaryOperation.out, RAX);
			} break;
			case IRINSTRUCTIONTYPE_EQUALS:
			{
				String left = X64IRValueToStr(context, inst.binaryOperation.left);
				String right = X64IRValueToStr(context, inst.binaryOperation.right);
				String out = X64IRValueToStr(context, inst.binaryOperation.out);
				X64OutputInstruction(context, CMP, inst.binaryOperation.left,
						inst.binaryOperation.right);
				PrintOut(context, "sete al\n");
				X64OutputInstruction(context, MOV, inst.binaryOperation.out, RAX);
			} break;
			case IRINSTRUCTIONTYPE_NOT:
			{
				String in = X64IRValueToStr(context, inst.unaryOperation.in);
				String out = X64IRValueToStr(context, inst.unaryOperation.out);
				X64OutputInstruction(context, CMP, inst.unaryOperation.in, IRValueImmediate(0));
				PrintOut(context, "sete al\n");
				PrintOut(context, "movzx eax, al\n");
				X64OutputInstruction(context, MOV, inst.unaryOperation.out, RAX);
			} break;
			case IRINSTRUCTIONTYPE_BITWISE_NOT:
			{
				String in = X64IRValueToStr(context, inst.unaryOperation.in);
				String out = X64IRValueToStr(context, inst.unaryOperation.out);
				X64OutputInstruction(context, MOV, inst.unaryOperation.out, inst.unaryOperation.in);
				PrintOut(context, "not %S\n", out);
			} break;
			case IRINSTRUCTIONTYPE_SUBTRACT_UNARY:
			{
				String in = X64IRValueToStr(context, inst.unaryOperation.in);
				String out = X64IRValueToStr(context, inst.unaryOperation.out);
				X64OutputInstruction(context, MOV, inst.unaryOperation.out, inst.unaryOperation.in);
				PrintOut(context, "neg %S\n", out);
			} break;
			case IRINSTRUCTIONTYPE_PROCEDURE_CALL:
			{
				for (int i = 0; i < inst.procedureCall.parameters.size; ++i)
				{
					IRValue param = inst.procedureCall.parameters[i];
					switch(i)
					{
					case 0:
						X64OutputInstruction(context, MOV, RCX, param);
						break;
					case 1:
						X64OutputInstruction(context, MOV, RDX, param);
						break;
					case 2:
						X64OutputInstruction(context, MOV, R8, param);
						break;
					case 3:
						X64OutputInstruction(context, MOV, R9, param);
						break;
					}
				}

				String callProcLabel = X64ProcedureToLabel(context, inst.procedureCall.procedure);
				PrintOut(context, "call %S\n", callProcLabel);
			} break;
			case IRINSTRUCTIONTYPE_RETURN:
			{
			} break;
			case IRINSTRUCTIONTYPE_LABEL:
			{
				PrintOut(context, "%S:\n", inst.label);
			} break;
			case IRINSTRUCTIONTYPE_JUMP:
			{
				String label = inst.conditionalJump.label;
				PrintOut(context, "jmp %S\n", label);
			} break;
			case IRINSTRUCTIONTYPE_JUMP_IF_ZERO:
			{
				String label = inst.conditionalJump.label;
				X64OutputInstruction(context, CMP, inst.conditionalJump.condition,
						IRValueImmediate(0));
				PrintOut(context, "je %S\n", label);
			} break;
			case IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO:
			{
				String label = inst.conditionalJump.label;
				String condition = X64IRValueToStr(context, inst.conditionalJump.condition);
				X64OutputInstruction(context, CMP, inst.conditionalJump.condition,
						IRValueImmediate(0));
				PrintOut(context, "jne %S\n", label);
			} break;
			case IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY:
			{
				String src = X64IRValueToStr(context, inst.memcpy.src);
				String dst = X64IRValueToStr(context, inst.memcpy.dst);
				String size = X64IRValueToStr(context, inst.memcpy.size);
				X64OutputInstruction(context, MOV, RCX, inst.memcpy.dst);
				X64OutputInstruction(context, MOV, RDX, inst.memcpy.src);
				X64OutputInstruction(context, MOV, R8, inst.memcpy.size);
				PrintOut(context, "call CopyMemory\n");
			} break;
			default:
			{
				ASSERT(!"Didn't recognize instruction type");
				PrintOut(context, "???INST\n");
			} break;
			}
		}

		PrintOut(context, "leave\n");
		PrintOut(context, "ret\n");

		PrintOut(context, "%S ENDP\n", procLabel);
	}

	PrintOut(context, "_TEXT ENDS\n");
	PrintOut(context, "END\n");

	CloseHandle(outputFile);
}
