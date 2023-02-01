#define PRINTIR_PRINT_TYPES 0

s64 PIRPrintOut(Context *context, const char *format, ...)
{
	char *buffer = (char *)t_threadMemPtr;

	va_list args;
	va_start(args, format);

	s64 size = stbsp_vsprintf(buffer, format, args);

	OutputBufferPut(context, size, buffer);

#if DEBUG_BUILD
	memset(t_threadMemPtr, 0x00, size + 1);
#endif

	va_end(args);
	return size;
}

inline Value PIRGetValue(Context *context, BucketArrayView<Value> localValues, u32 valueIdx)
{
	ASSERT(valueIdx > 0);
	if (valueIdx & VALUE_GLOBAL_BIT)
		return GetGlobalValue(context, valueIdx);
	else {
		return localValues[valueIdx];
	}
}

inline IRValue PIRValueValue(Context *context, BucketArrayView<Value> localValues, u32 valueIdx)
{
	IRValue result;
	result.valueType = IRVALUETYPE_VALUE;
	result.valueIdx = valueIdx;
	result.typeTableIdx = PIRGetValue(context, localValues, valueIdx).typeTableIdx;
	return result;
}

inline String PIRValueToStr(Context *context, BucketArrayView<Value> localValues, u32 valueIdx)
{
	Value v = PIRGetValue(context, localValues, valueIdx);
#if DEBUG_BUILD
	if (v.name) {
		if (valueIdx & VALUE_GLOBAL_BIT)
			return TPrintF("$gv%u\"%S\"", valueIdx & VALUE_GLOBAL_MASK, v.name);
		else
			return TPrintF("$v%u\"%S\"", valueIdx, v.name);
	}
	else
#endif
	{
		if (valueIdx & VALUE_GLOBAL_BIT)
			return TPrintF("$gv%u", valueIdx & VALUE_GLOBAL_MASK);
		else
			return TPrintF("$v%u", valueIdx);
	}
}

void PrintIRValue(Context *context, BucketArrayView<Value> localValues, IRValue value)
{
	if (value.valueType == IRVALUETYPE_MEMORY) {
		PIRPrintOut(context, "[%S", PIRValueToStr(context, localValues, value.mem.baseValueIdx));
		if (value.mem.offset)
			PIRPrintOut(context, "+0x%llx", value.mem.offset);
		if (value.mem.elementSize) {
			String indexValueStr = PIRValueToStr(context, localValues, value.mem.indexValueIdx);
			PIRPrintOut(context, "+%S*%lld", indexValueStr, value.mem.elementSize);
		}
		PIRPrintOut(context, "]");
	}
	else if (value.valueType == IRVALUETYPE_VALUE)
		PIRPrintOut(context, "%S", PIRValueToStr(context, localValues, value.valueIdx));
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		PIRPrintOut(context, "0x%llx", value.immediate);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_FLOAT)
		PIRPrintOut(context, "%f", value.immediateFloat);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_STRING)
		PIRPrintOut(context, "\"%S\"", context->stringLiterals.unsafe[value.immediateStringIdx]);
	else
		PIRPrintOut(context, "???");

#if PRINTIR_PRINT_TYPES
	PIRPrintOut(context, " : %S", TypeInfoToString(context, value.typeTableIdx));
#endif
}

void PrintIRInstructionOperator(Context *context, IRInstruction inst)
{
	switch (inst.type)
	{
	case IRINSTRUCTIONTYPE_ADD:
		PIRPrintOut(context, "+");
		break;
	case IRINSTRUCTIONTYPE_SUBTRACT:
	case IRINSTRUCTIONTYPE_SUBTRACT_UNARY:
		PIRPrintOut(context, "-");
		break;
	case IRINSTRUCTIONTYPE_MULTIPLY:
		PIRPrintOut(context, "*");
		break;
	case IRINSTRUCTIONTYPE_DIVIDE:
		PIRPrintOut(context, "/");
		break;
	case IRINSTRUCTIONTYPE_MODULO:
		PIRPrintOut(context, "%");
		break;
	case IRINSTRUCTIONTYPE_SHIFT_LEFT:
		PIRPrintOut(context, "<<");
		break;
	case IRINSTRUCTIONTYPE_SHIFT_RIGHT:
		PIRPrintOut(context, ">>");
		break;
	case IRINSTRUCTIONTYPE_OR:
		PIRPrintOut(context, "||");
		break;
	case IRINSTRUCTIONTYPE_AND:
		PIRPrintOut(context, "&&");
		break;
	case IRINSTRUCTIONTYPE_BITWISE_OR:
		PIRPrintOut(context, "|");
		break;
	case IRINSTRUCTIONTYPE_BITWISE_XOR:
		PIRPrintOut(context, "^");
		break;
	case IRINSTRUCTIONTYPE_BITWISE_AND:
		PIRPrintOut(context, "&");
		break;
	case IRINSTRUCTIONTYPE_BITWISE_NOT:
		PIRPrintOut(context, "~");
		break;
	case IRINSTRUCTIONTYPE_EQUALS:
	case IRINSTRUCTIONTYPE_JUMP_IF_EQUALS:
		PIRPrintOut(context, "==");
		break;
	case IRINSTRUCTIONTYPE_NOT_EQUALS:
	case IRINSTRUCTIONTYPE_JUMP_IF_NOT_EQUALS:
		PIRPrintOut(context, "!=");
		break;
	case IRINSTRUCTIONTYPE_GREATER_THAN:
	case IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN:
		PIRPrintOut(context, ">");
		break;
	case IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS:
	case IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN_OR_EQUALS:
		PIRPrintOut(context, ">=");
		break;
	case IRINSTRUCTIONTYPE_LESS_THAN:
	case IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN:
		PIRPrintOut(context, "<");
		break;
	case IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS:
	case IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN_OR_EQUALS:
		PIRPrintOut(context, "<=");
		break;
	case IRINSTRUCTIONTYPE_NOT:
		PIRPrintOut(context, "!");
		break;
	case IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS:
		PIRPrintOut(context, "address of ");
		break;
	default:
		CRASH;
	}
}

void PrintIRInstruction(Context *context, BucketArrayView<Value> localValues, IRInstruction inst)
{
	if (inst.type >= IRINSTRUCTIONTYPE_UnaryBegin && inst.type <= IRINSTRUCTIONTYPE_UnaryEnd) {
		PrintIRValue(context, localValues, inst.unaryOperation.out);
		PIRPrintOut(context, " := ");
		PrintIRInstructionOperator(context, inst);
		PrintIRValue(context, localValues, inst.unaryOperation.in);
		PIRPrintOut(context, "\n");
	}
	else if (inst.type >= IRINSTRUCTIONTYPE_BinaryBegin && inst.type <= IRINSTRUCTIONTYPE_BinaryEnd) {
		PrintIRValue(context, localValues, inst.binaryOperation.out);
		PIRPrintOut(context, " := ");
		PrintIRValue(context, localValues, inst.binaryOperation.left);
		PIRPrintOut(context, " ");
		PrintIRInstructionOperator(context, inst);
		PIRPrintOut(context, " ");
		PrintIRValue(context, localValues, inst.binaryOperation.right);
		PIRPrintOut(context, "\n");
	}
	else if (inst.type >= IRINSTRUCTIONTYPE_CompareJumpBegin && inst.type <=
			IRINSTRUCTIONTYPE_CompareJumpEnd) {
		PIRPrintOut(context, "if ");
		PrintIRValue(context, localValues, inst.conditionalJump2.left);
		PIRPrintOut(context, " ");
		PrintIRInstructionOperator(context, inst);
		PIRPrintOut(context, " ");
		PrintIRValue(context, localValues, inst.conditionalJump2.right);
		PIRPrintOut(context, " jump %S\n", inst.conditionalJump2.label->name);
	}
	else switch (inst.type) {
	case IRINSTRUCTIONTYPE_LABEL:
	{
		PIRPrintOut(context, "%S:\n", inst.label->name);
	} break;
	case IRINSTRUCTIONTYPE_NOP:
	{
		PIRPrintOut(context, "NOP\n", inst.comment);
	} break;
	case IRINSTRUCTIONTYPE_COMMENT:
	{
		PIRPrintOut(context, "\t// %S\n", inst.comment);
	} break;
	case IRINSTRUCTIONTYPE_JUMP:
	{
		PIRPrintOut(context, "jump \"%S\"\n", inst.jump.label->name);
	} break;
	case IRINSTRUCTIONTYPE_JUMP_IF_ZERO:
	{
		PIRPrintOut(context, "if !");
		PrintIRValue(context, localValues, inst.conditionalJump.condition);
		PIRPrintOut(context, " jump %S\n", inst.conditionalJump.label->name);
	} break;
	case IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO:
	{
		PIRPrintOut(context, "if ");
		PrintIRValue(context, localValues, inst.conditionalJump.condition);
		PIRPrintOut(context, " jump %S\n", inst.conditionalJump.label->name);
	} break;
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL:
	{
		if (inst.procedureCall.returnValues.size) {
			for (int i = 0; i < inst.procedureCall.returnValues.size; ++i)
				PrintIRValue(context, localValues, inst.procedureCall.returnValues[i]);
			PIRPrintOut(context, " := ");
		}
		String name = GetProcedureRead(context, inst.procedureCall.procedureIdx).name;
		PIRPrintOut(context, "call %S(", name);

		for (int i = 0; i < inst.procedureCall.parameters.size; ++i) {
			if (i) PIRPrintOut(context, ", ");
			PrintIRValue(context, localValues, inst.procedureCall.parameters[i]);
		}
		PIRPrintOut(context, ")\n");
	} break;
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL_INDIRECT:
	{
		if (inst.procedureCall.returnValues.size) {
			for (int i = 0; i < inst.procedureCall.returnValues.size; ++i)
				PrintIRValue(context, localValues, inst.procedureCall.returnValues[i]);
			PIRPrintOut(context, " := ");
		}
		PIRPrintOut(context, "call virtual ");
		PrintIRValue(context, localValues, inst.procedureCall.procIRValue);
		PIRPrintOut(context, "(");

		for (int i = 0; i < inst.procedureCall.parameters.size; ++i)
		{
			if (i) PIRPrintOut(context, ", ");
			PrintIRValue(context, localValues, inst.procedureCall.parameters[i]);
		}
		PIRPrintOut(context, ")\n");
	} break;
	case IRINSTRUCTIONTYPE_PUSH_VALUE:
	{
		PIRPrintOut(context, "push value %S\n", PIRValueToStr(context, localValues, inst.pushValue.valueIdx));
	} break;
	case IRINSTRUCTIONTYPE_PUSH_SCOPE:
	{
		PIRPrintOut(context, "push scope\n");
	} break;
	case IRINSTRUCTIONTYPE_POP_SCOPE:
	{
		PIRPrintOut(context, "pop scope\n");
	} break;
	case IRINSTRUCTIONTYPE_RETURN:
	{
		PIRPrintOut(context, "return ");
		for (int i = 0; i < inst.returnInst.returnValueIndices.size; ++i) {
			if (i) PIRPrintOut(context, ", ");
			IRValue irValue = PIRValueValue(context, localValues, inst.returnInst.returnValueIndices[i]);
			PrintIRValue(context, localValues, irValue);
		}
	} break;
	case IRINSTRUCTIONTYPE_ASSIGNMENT:
	case IRINSTRUCTIONTYPE_CONVERT_INT_TO_FLOAT:
	case IRINSTRUCTIONTYPE_CONVERT_FLOAT_TO_INT:
	case IRINSTRUCTIONTYPE_CONVERT_PRECISION:
	case IRINSTRUCTIONTYPE_SIGN_EXTEND:
	case IRINSTRUCTIONTYPE_ZERO_EXTEND:
	case IRINSTRUCTIONTYPE_TRUNCATE:
	{
		switch (inst.type) {
		case IRINSTRUCTIONTYPE_CONVERT_INT_TO_FLOAT:
			PIRPrintOut(context, "int to float "); break;
		case IRINSTRUCTIONTYPE_CONVERT_FLOAT_TO_INT:
			PIRPrintOut(context, "float to int "); break;
		case IRINSTRUCTIONTYPE_CONVERT_PRECISION:
			PIRPrintOut(context, "change precision "); break;
		case IRINSTRUCTIONTYPE_SIGN_EXTEND:
			PIRPrintOut(context, "sign extend "); break;
		case IRINSTRUCTIONTYPE_ZERO_EXTEND:
			PIRPrintOut(context, "zero extend "); break;
		case IRINSTRUCTIONTYPE_TRUNCATE:
			PIRPrintOut(context, "truncate "); break;
		}
		PrintIRValue(context, localValues, inst.assignment.dst);
		PIRPrintOut(context, " = ");
		PrintIRValue(context, localValues, inst.assignment.src);
		PIRPrintOut(context, "\n");
	} break;
	case IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS:
	{
		PrintIRValue(context, localValues, inst.assignment.dst);
		PIRPrintOut(context, " = ^");
		PrintIRValue(context, localValues, inst.assignment.src);
		PIRPrintOut(context, "\n");
	} break;
	case IRINSTRUCTIONTYPE_COPY_MEMORY:
	{
		PIRPrintOut(context, "copyMemory(");
		PrintIRValue(context, localValues, inst.copyMemory.dst);
		PIRPrintOut(context, ", ");
		PrintIRValue(context, localValues, inst.copyMemory.src);
		PIRPrintOut(context, ", ");
		PrintIRValue(context, localValues, inst.copyMemory.size);
		PIRPrintOut(context, ")\n");
	} break;
	case IRINSTRUCTIONTYPE_ZERO_MEMORY:
	{
		PIRPrintOut(context, "zeroMemory(");
		PrintIRValue(context, localValues, inst.zeroMemory.dst);
		PIRPrintOut(context, ", ");
		PrintIRValue(context, localValues, inst.zeroMemory.size);
		PIRPrintOut(context, ")\n");
	} break;
	default:
	{
		PIRPrintOut(context, "???INST\n");
	}
	}
}

void PrintJobIRInstructions(IRContext *context)
{
	static Mutex printIRMutex = SYSCreateMutex();

	const int padding = 20;
	Procedure proc = GetProcedureRead(context->global, context->procedureIdx);
	TypeInfoProcedure procTypeInfo = GetTypeInfo(context, proc.typeTableIdx).procedureInfo;

	SYSMutexLock(printIRMutex);

	OutputBufferReset(context->global);

	String name = proc.name;
	PIRPrintOut(context->global, "proc %S(", name);

	for (int paramIdx = 0; paramIdx < proc.parameterValues.size; ++paramIdx) {
		if (paramIdx) PIRPrintOut(context->global, ", ");
		u32 paramValueIdx = proc.parameterValues[paramIdx];
		Value paramValue = IRGetValue(context, paramValueIdx);
		String typeStr = TypeInfoToString(context, paramValue.typeTableIdx);
		PIRPrintOut(context->global, "%S", PIRValueToStr(context->global, proc.localValues, paramValueIdx));
#if PRINTIR_PRINT_TYPES
		PIRPrintOut(context->global, " : %S", typeStr);
#endif
	}
	PIRPrintOut(context->global, ")");
	if (procTypeInfo.returnTypeIndices.size) {
		PIRPrintOut(context->global, " -> ");
		for (int returnIdx = 0; returnIdx < procTypeInfo.returnTypeIndices.size; ++returnIdx) {
			if (returnIdx) PIRPrintOut(context->global, ", ");
			String returnTypeStr = TypeInfoToString(context,
					procTypeInfo.returnTypeIndices[returnIdx]);
			PIRPrintOut(context->global, "%S", returnTypeStr);
		}
	}
	PIRPrintOut(context->global, "\n");

	u32 lastFileIdx = U32_MAX;
	u32 lastLine = U32_MAX;

	const auto &instructions = proc.irInstructions;
	const u64 instructionCount = instructions.count;
	for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx) {
		IRInstruction inst = instructions[instructionIdx];

		if (inst.loc.fileIdx != 0) {
			FatSourceLocation fatLoc = ExpandSourceLocation(context->global, inst.loc);
			if (inst.loc.fileIdx != lastFileIdx || fatLoc.line != lastLine)
				PIRPrintOut(context->global, "On: %S\n", String{ fatLoc.lineSize, fatLoc.beginingOfLine });

			lastFileIdx = inst.loc.fileIdx;
			lastLine = fatLoc.line;
		}

		if (inst.type == IRINSTRUCTIONTYPE_LABEL) {
			PIRPrintOut(context->global, "%S: ", inst.label->name);

			IRInstruction nextInst = instructions[instructionIdx + 1];
			if (nextInst.type != IRINSTRUCTIONTYPE_LABEL &&
				nextInst.type != IRINSTRUCTIONTYPE_PUSH_SCOPE &&
				nextInst.type != IRINSTRUCTIONTYPE_POP_SCOPE &&
				nextInst.type != IRINSTRUCTIONTYPE_PUSH_VALUE &&
				nextInst.type != IRINSTRUCTIONTYPE_NOP) {
				for (s64 i = inst.label->name.size + 2; i < padding; ++i)
					PIRPrintOut(context->global, " ");

				++instructionIdx;
				if (instructionIdx >= instructionCount)
					break;
				inst = instructions[instructionIdx];
			}
			else {
				PIRPrintOut(context->global, "\n");
				continue;
			}
		}
		else if (inst.type == IRINSTRUCTIONTYPE_PUSH_SCOPE ||
				 inst.type == IRINSTRUCTIONTYPE_POP_SCOPE ||
				 inst.type == IRINSTRUCTIONTYPE_PUSH_VALUE ||
				 inst.type == IRINSTRUCTIONTYPE_NOP)
			continue;
		else {
			for (s64 i = 0; i < padding; ++i)
				PIRPrintOut(context->global, " ");
		}

		PrintIRInstruction(context->global, proc.localValues, inst);
	}
	PIRPrintOut(context->global, "\n");

	OutputBufferWriteToFile(context->global, TPrintF("output/ir_%S.txt", proc.name));
	SYSMutexUnlock(printIRMutex);
}
