#define PRINTIR_PRINT_TYPES 0

s64 PIRPrintOut(const char *format, ...)
{
	char *buffer = (char *)t_threadMemPtr;

	va_list args;
	va_start(args, format);

	s64 size = stbsp_vsprintf(buffer, format, args);

	OutputBufferPut(size, buffer);

#if DEBUG_BUILD
	memset(t_threadMemPtr, 0x00, size + 1);
#endif

	va_end(args);
	return size;
}

inline Value PIRGetValue(BucketArrayView<Value> localValues, u32 valueIdx)
{
	ASSERT(valueIdx > 0);
	if (valueIdx & VALUE_GLOBAL_BIT)
		return GetGlobalValue(valueIdx);
	else {
		return localValues[valueIdx];
	}
}

inline IRValue PIRValueValue(BucketArrayView<Value> localValues, u32 valueIdx)
{
	IRValue result;
	result.valueType = IRVALUETYPE_VALUE;
	result.valueIdx = valueIdx;
	result.typeTableIdx = PIRGetValue(localValues, valueIdx).typeTableIdx;
	return result;
}

inline String PIRValueToStr(BucketArrayView<Value> localValues, u32 valueIdx)
{
	Value v = PIRGetValue(localValues, valueIdx);
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

void PrintIRValue(BucketArrayView<Value> localValues, IRValue value)
{
	if (value.valueType == IRVALUETYPE_MEMORY) {
		PIRPrintOut("[%S", PIRValueToStr(localValues, value.mem.baseValueIdx));
		if (value.mem.offset)
			PIRPrintOut("+0x%llx", value.mem.offset);
		if (value.mem.elementSize) {
			String indexValueStr = PIRValueToStr(localValues, value.mem.indexValueIdx);
			PIRPrintOut("+%S*%lld", indexValueStr, value.mem.elementSize);
		}
		PIRPrintOut("]");
	}
	else if (value.valueType == IRVALUETYPE_VALUE)
		PIRPrintOut("%S", PIRValueToStr(localValues, value.valueIdx));
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		PIRPrintOut("0x%llx", value.immediate);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_FLOAT)
		PIRPrintOut("%f", value.immediateFloat);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_STRING)
		PIRPrintOut("\"%S\"", g_context->stringLiterals.unsafe[value.immediateStringIdx]);
	else
		PIRPrintOut("???");

#if PRINTIR_PRINT_TYPES
	PIRPrintOut(" : %S", TypeInfoToString(value.typeTableIdx));
#endif
}

void PrintIRInstructionOperator(IRInstruction inst)
{
	switch (inst.type)
	{
	case IRINSTRUCTIONTYPE_ADD:
		PIRPrintOut("+");
		break;
	case IRINSTRUCTIONTYPE_SUBTRACT:
	case IRINSTRUCTIONTYPE_SUBTRACT_UNARY:
		PIRPrintOut("-");
		break;
	case IRINSTRUCTIONTYPE_MULTIPLY:
		PIRPrintOut("*");
		break;
	case IRINSTRUCTIONTYPE_DIVIDE:
		PIRPrintOut("/");
		break;
	case IRINSTRUCTIONTYPE_MODULO:
		PIRPrintOut("%");
		break;
	case IRINSTRUCTIONTYPE_SHIFT_LEFT:
		PIRPrintOut("<<");
		break;
	case IRINSTRUCTIONTYPE_SHIFT_RIGHT:
		PIRPrintOut(">>");
		break;
	case IRINSTRUCTIONTYPE_OR:
		PIRPrintOut("||");
		break;
	case IRINSTRUCTIONTYPE_AND:
		PIRPrintOut("&&");
		break;
	case IRINSTRUCTIONTYPE_BITWISE_OR:
		PIRPrintOut("|");
		break;
	case IRINSTRUCTIONTYPE_BITWISE_XOR:
		PIRPrintOut("^");
		break;
	case IRINSTRUCTIONTYPE_BITWISE_AND:
		PIRPrintOut("&");
		break;
	case IRINSTRUCTIONTYPE_BITWISE_NOT:
		PIRPrintOut("~");
		break;
	case IRINSTRUCTIONTYPE_EQUALS:
	case IRINSTRUCTIONTYPE_JUMP_IF_EQUALS:
		PIRPrintOut("==");
		break;
	case IRINSTRUCTIONTYPE_NOT_EQUALS:
	case IRINSTRUCTIONTYPE_JUMP_IF_NOT_EQUALS:
		PIRPrintOut("!=");
		break;
	case IRINSTRUCTIONTYPE_GREATER_THAN:
	case IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN:
		PIRPrintOut(">");
		break;
	case IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS:
	case IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN_OR_EQUALS:
		PIRPrintOut(">=");
		break;
	case IRINSTRUCTIONTYPE_LESS_THAN:
	case IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN:
		PIRPrintOut("<");
		break;
	case IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS:
	case IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN_OR_EQUALS:
		PIRPrintOut("<=");
		break;
	case IRINSTRUCTIONTYPE_NOT:
		PIRPrintOut("!");
		break;
	case IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS:
		PIRPrintOut("address of ");
		break;
	default:
		CRASH;
	}
}

void PrintIRInstruction(BucketArrayView<Value> localValues, IRInstruction inst)
{
	if (inst.type >= IRINSTRUCTIONTYPE_UnaryBegin && inst.type <= IRINSTRUCTIONTYPE_UnaryEnd) {
		PrintIRValue(localValues, inst.unaryOperation.out);
		PIRPrintOut(" := ");
		PrintIRInstructionOperator(inst);
		PrintIRValue(localValues, inst.unaryOperation.in);
		PIRPrintOut("\n");
	}
	else if (inst.type >= IRINSTRUCTIONTYPE_BinaryBegin && inst.type <= IRINSTRUCTIONTYPE_BinaryEnd) {
		PrintIRValue(localValues, inst.binaryOperation.out);
		PIRPrintOut(" := ");
		PrintIRValue(localValues, inst.binaryOperation.left);
		PIRPrintOut(" ");
		PrintIRInstructionOperator(inst);
		PIRPrintOut(" ");
		PrintIRValue(localValues, inst.binaryOperation.right);
		PIRPrintOut("\n");
	}
	else if (inst.type >= IRINSTRUCTIONTYPE_CompareJumpBegin && inst.type <=
			IRINSTRUCTIONTYPE_CompareJumpEnd) {
		PIRPrintOut("if ");
		PrintIRValue(localValues, inst.conditionalJump2.left);
		PIRPrintOut(" ");
		PrintIRInstructionOperator(inst);
		PIRPrintOut(" ");
		PrintIRValue(localValues, inst.conditionalJump2.right);
		PIRPrintOut(" jump %S\n", inst.conditionalJump2.label->name);
	}
	else switch (inst.type) {
	case IRINSTRUCTIONTYPE_LABEL:
	{
		PIRPrintOut("%S:\n", inst.label->name);
	} break;
	case IRINSTRUCTIONTYPE_NOP:
	{
		PIRPrintOut("NOP\n", inst.comment);
	} break;
	case IRINSTRUCTIONTYPE_COMMENT:
	{
		PIRPrintOut("\t// %S\n", inst.comment);
	} break;
	case IRINSTRUCTIONTYPE_JUMP:
	{
		PIRPrintOut("jump \"%S\"\n", inst.jump.label->name);
	} break;
	case IRINSTRUCTIONTYPE_JUMP_IF_ZERO:
	{
		PIRPrintOut("if !");
		PrintIRValue(localValues, inst.conditionalJump.condition);
		PIRPrintOut(" jump %S\n", inst.conditionalJump.label->name);
	} break;
	case IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO:
	{
		PIRPrintOut("if ");
		PrintIRValue(localValues, inst.conditionalJump.condition);
		PIRPrintOut(" jump %S\n", inst.conditionalJump.label->name);
	} break;
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL:
	{
		if (inst.procedureCall.returnValues.size) {
			for (int i = 0; i < inst.procedureCall.returnValues.size; ++i)
				PrintIRValue(localValues, inst.procedureCall.returnValues[i]);
			PIRPrintOut(" := ");
		}
		String name = GetProcedureRead(inst.procedureCall.procedureIdx).name;
		PIRPrintOut("call %S(", name);

		for (int i = 0; i < inst.procedureCall.parameters.size; ++i) {
			if (i) PIRPrintOut(", ");
			PrintIRValue(localValues, inst.procedureCall.parameters[i]);
		}
		PIRPrintOut(")\n");
	} break;
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL_INDIRECT:
	{
		if (inst.procedureCall.returnValues.size) {
			for (int i = 0; i < inst.procedureCall.returnValues.size; ++i)
				PrintIRValue(localValues, inst.procedureCall.returnValues[i]);
			PIRPrintOut(" := ");
		}
		PIRPrintOut("call virtual ");
		PrintIRValue(localValues, inst.procedureCall.procIRValue);
		PIRPrintOut("(");

		for (int i = 0; i < inst.procedureCall.parameters.size; ++i)
		{
			if (i) PIRPrintOut(", ");
			PrintIRValue(localValues, inst.procedureCall.parameters[i]);
		}
		PIRPrintOut(")\n");
	} break;
	case IRINSTRUCTIONTYPE_PUSH_VALUE:
	{
		PIRPrintOut("push value %S\n", PIRValueToStr(localValues, inst.pushValue.valueIdx));
	} break;
	case IRINSTRUCTIONTYPE_PUSH_SCOPE:
	{
		PIRPrintOut("push scope\n");
	} break;
	case IRINSTRUCTIONTYPE_POP_SCOPE:
	{
		PIRPrintOut("pop scope\n");
	} break;
	case IRINSTRUCTIONTYPE_RETURN:
	{
		PIRPrintOut("return ");
		for (int i = 0; i < inst.returnInst.returnValueIndices.size; ++i) {
			if (i) PIRPrintOut(", ");
			IRValue irValue = PIRValueValue(localValues, inst.returnInst.returnValueIndices[i]);
			PrintIRValue(localValues, irValue);
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
			PIRPrintOut("int to float "); break;
		case IRINSTRUCTIONTYPE_CONVERT_FLOAT_TO_INT:
			PIRPrintOut("float to int "); break;
		case IRINSTRUCTIONTYPE_CONVERT_PRECISION:
			PIRPrintOut("change precision "); break;
		case IRINSTRUCTIONTYPE_SIGN_EXTEND:
			PIRPrintOut("sign extend "); break;
		case IRINSTRUCTIONTYPE_ZERO_EXTEND:
			PIRPrintOut("zero extend "); break;
		case IRINSTRUCTIONTYPE_TRUNCATE:
			PIRPrintOut("truncate "); break;
		}
		PrintIRValue(localValues, inst.assignment.dst);
		PIRPrintOut(" = ");
		PrintIRValue(localValues, inst.assignment.src);
		PIRPrintOut("\n");
	} break;
	case IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS:
	{
		PrintIRValue(localValues, inst.assignment.dst);
		PIRPrintOut(" = ^");
		PrintIRValue(localValues, inst.assignment.src);
		PIRPrintOut("\n");
	} break;
	case IRINSTRUCTIONTYPE_COPY_MEMORY:
	{
		PIRPrintOut("copyMemory(");
		PrintIRValue(localValues, inst.copyMemory.dst);
		PIRPrintOut(", ");
		PrintIRValue(localValues, inst.copyMemory.src);
		PIRPrintOut(", ");
		PrintIRValue(localValues, inst.copyMemory.size);
		PIRPrintOut(")\n");
	} break;
	case IRINSTRUCTIONTYPE_ZERO_MEMORY:
	{
		PIRPrintOut("zeroMemory(");
		PrintIRValue(localValues, inst.zeroMemory.dst);
		PIRPrintOut(", ");
		PrintIRValue(localValues, inst.zeroMemory.size);
		PIRPrintOut(")\n");
	} break;
	default:
	{
		PIRPrintOut("???INST\n");
	}
	}
}

void PrintJobIRInstructions(IRContext *context)
{
	static Mutex printIRMutex = SYSCreateMutex();

	const int padding = 20;
	Procedure proc = GetProcedureRead(context->procedureIdx);
	TypeInfoProcedure procTypeInfo = GetTypeInfo(proc.typeTableIdx).procedureInfo;

	SYSMutexLock(printIRMutex);

	OutputBufferReset();

	String name = proc.name;
	PIRPrintOut("proc %S(", name);

	for (int paramIdx = 0; paramIdx < proc.parameterValues.size; ++paramIdx) {
		if (paramIdx) PIRPrintOut(", ");
		u32 paramValueIdx = proc.parameterValues[paramIdx];
		Value paramValue = IRGetValue(context, paramValueIdx);
		String typeStr = TypeInfoToString(paramValue.typeTableIdx);
		PIRPrintOut("%S", PIRValueToStr(proc.localValues, paramValueIdx));
#if PRINTIR_PRINT_TYPES
		PIRPrintOut(" : %S", typeStr);
#endif
	}
	PIRPrintOut(")");
	if (procTypeInfo.returnTypeIndices.size) {
		PIRPrintOut(" -> ");
		for (int returnIdx = 0; returnIdx < procTypeInfo.returnTypeIndices.size; ++returnIdx) {
			if (returnIdx) PIRPrintOut(", ");
			String returnTypeStr = TypeInfoToString(procTypeInfo.returnTypeIndices[returnIdx]);
			PIRPrintOut("%S", returnTypeStr);
		}
	}
	PIRPrintOut("\n");

	u32 lastFileIdx = U32_MAX;
	u32 lastLine = U32_MAX;

	const auto &instructions = proc.irInstructions;
	const u64 instructionCount = instructions.count;
	for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx) {
		IRInstruction inst = instructions[instructionIdx];

		if (inst.loc.fileIdx != 0) {
			FatSourceLocation fatLoc = ExpandSourceLocation(inst.loc);
			if (inst.loc.fileIdx != lastFileIdx || fatLoc.line != lastLine)
				PIRPrintOut("On: %S\n", String{ fatLoc.lineSize, fatLoc.beginingOfLine });

			lastFileIdx = inst.loc.fileIdx;
			lastLine = fatLoc.line;
		}

		if (inst.type == IRINSTRUCTIONTYPE_LABEL) {
			PIRPrintOut("%S: ", inst.label->name);

			IRInstruction nextInst = instructions[instructionIdx + 1];
			if (nextInst.type != IRINSTRUCTIONTYPE_LABEL &&
				nextInst.type != IRINSTRUCTIONTYPE_PUSH_SCOPE &&
				nextInst.type != IRINSTRUCTIONTYPE_POP_SCOPE &&
				nextInst.type != IRINSTRUCTIONTYPE_PUSH_VALUE &&
				nextInst.type != IRINSTRUCTIONTYPE_NOP) {
				for (s64 i = inst.label->name.size + 2; i < padding; ++i)
					PIRPrintOut(" ");

				++instructionIdx;
				if (instructionIdx >= instructionCount)
					break;
				inst = instructions[instructionIdx];
			}
			else {
				PIRPrintOut("\n");
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
				PIRPrintOut(" ");
		}

		PrintIRInstruction(proc.localValues, inst);
	}
	PIRPrintOut("\n");

	OutputBufferWriteToFile(TPrintF("output/ir_%S.txt", proc.name));
	SYSMutexUnlock(printIRMutex);
}
