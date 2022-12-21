s64 PIRPrintOut(Context *context, const char *format, ...)
{
	char *buffer = (char *)t_threadMemPtr;

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
	memset(t_threadMemPtr, 0x00, size + 1);
#endif

	va_end(args);
	return size;
}

inline Value PIRGetValue(Context *context, u32 procedureIdx, u32 valueIdx) {
	ASSERT(valueIdx > 0);
	if (valueIdx & VALUE_GLOBAL_BIT)
		return GetGlobalValue(context, valueIdx);
	else {
		Procedure *proc = &context->procedures.unsafe[procedureIdx];
		return proc->localValues[valueIdx];
	}
}

inline String PIRValueToStr(Context *context, u32 procedureIdx, u32 valueIdx)
{
	Value v = PIRGetValue(context, procedureIdx, valueIdx);
	if (v.name)
		return TPrintF("$v%u\"%S\"", valueIdx, v.name);
	else
		return TPrintF("$v%u", valueIdx);
}

void PrintIRValue(Context *context, u32 procedureIdx, IRValue value)
{
	if (value.valueType == IRVALUETYPE_VALUE_DEREFERENCE)
	{
		PIRPrintOut(context, "[%S", PIRValueToStr(context, procedureIdx, value.value.valueIdx));
		if (value.value.offset)
			PIRPrintOut(context, "+0x%llx", value.value.offset);
		PIRPrintOut(context, "]");
	}
	else if (value.valueType == IRVALUETYPE_VALUE)
	{
		PIRPrintOut(context, "%S", PIRValueToStr(context, procedureIdx, value.value.valueIdx));
		if (value.value.offset)
			PIRPrintOut(context, "+0x%llx", value.value.offset);
	}
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		PIRPrintOut(context, "0x%llx", value.immediate);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_FLOAT)
		PIRPrintOut(context, "%f", value.immediateFloat);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_STRING)
		PIRPrintOut(context, "\"%S\"", context->stringLiterals.unsafe[value.immediateStringIdx]);
	else
		PIRPrintOut(context, "???");


	PIRPrintOut(context, " : %S", TypeInfoToString(context, value.typeTableIdx));
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
		PIRPrintOut(context, "==");
		break;
	case IRINSTRUCTIONTYPE_GREATER_THAN:
		PIRPrintOut(context, ">");
		break;
	case IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS:
		PIRPrintOut(context, ">=");
		break;
	case IRINSTRUCTIONTYPE_LESS_THAN:
		PIRPrintOut(context, "<");
		break;
	case IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS:
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

void PrintIRInstruction(Context *context, u32 procedureIdx, IRInstruction inst)
{
	if (inst.type >= IRINSTRUCTIONTYPE_UnaryBegin && inst.type <= IRINSTRUCTIONTYPE_UnaryEnd)
	{
		PrintIRValue(context, procedureIdx, inst.unaryOperation.out);
		PIRPrintOut(context, " := ");
		PrintIRInstructionOperator(context, inst);
		PrintIRValue(context, procedureIdx, inst.unaryOperation.in);
		PIRPrintOut(context, "\n");
	}
	else if (inst.type >= IRINSTRUCTIONTYPE_BinaryBegin && inst.type <= IRINSTRUCTIONTYPE_BinaryEnd)
	{
		PrintIRValue(context, procedureIdx, inst.binaryOperation.out);
		PIRPrintOut(context, " := ");
		PrintIRValue(context, procedureIdx, inst.binaryOperation.left);
		PIRPrintOut(context, " ");
		PrintIRInstructionOperator(context, inst);
		PIRPrintOut(context, " ");
		PrintIRValue(context, procedureIdx, inst.binaryOperation.right);
		PIRPrintOut(context, "\n");
	}
	else switch (inst.type)
	{
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
		PrintIRValue(context, procedureIdx, inst.conditionalJump.condition);
		PIRPrintOut(context, " jump %S\n", inst.conditionalJump.label->name);
	} break;
	case IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO:
	{
		PIRPrintOut(context, "if ");
		PrintIRValue(context, procedureIdx, inst.conditionalJump.condition);
		PIRPrintOut(context, " jump %S\n", inst.conditionalJump.label->name);
	} break;
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL:
	{
		if (inst.procedureCall.returnValues.size) {
			for (int i = 0; i < inst.procedureCall.returnValues.size; ++i)
				PrintIRValue(context, procedureIdx, inst.procedureCall.returnValues[i]);
			PIRPrintOut(context, " := ");
		}
		String name = GetProcedureRead(context, inst.procedureCall.procedureIdx).name;
		PIRPrintOut(context, "call %S(", name);

		for (int i = 0; i < inst.procedureCall.parameters.size; ++i) {
			if (i) PIRPrintOut(context, ", ");
			PrintIRValue(context, procedureIdx, inst.procedureCall.parameters[i]);
		}
		PIRPrintOut(context, ")\n");
	} break;
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL_INDIRECT:
	{
		if (inst.procedureCall.returnValues.size) {
			for (int i = 0; i < inst.procedureCall.returnValues.size; ++i)
				PrintIRValue(context, procedureIdx, inst.procedureCall.returnValues[i]);
			PIRPrintOut(context, " := ");
		}
		PIRPrintOut(context, "call virtual ");
		PrintIRValue(context, procedureIdx, inst.procedureCall.procIRValue);
		PIRPrintOut(context, "(");

		for (int i = 0; i < inst.procedureCall.parameters.size; ++i)
		{
			if (i) PIRPrintOut(context, ", ");
			PrintIRValue(context, procedureIdx, inst.procedureCall.parameters[i]);
		}
		PIRPrintOut(context, ")\n");
	} break;
	case IRINSTRUCTIONTYPE_PUSH_VALUE:
	{
		PIRPrintOut(context, "push value %S\n", PIRValueToStr(context, procedureIdx, inst.pushValue.valueIdx));
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
		PIRPrintOut(context, "return\n");
	} break;
	case IRINSTRUCTIONTYPE_ASSIGNMENT:
	{
		PrintIRValue(context, procedureIdx, inst.assignment.dst);
		PIRPrintOut(context, " = ");
		PrintIRValue(context, procedureIdx, inst.assignment.src);
		PIRPrintOut(context, "\n");
	} break;
	case IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS:
	{
		PrintIRValue(context, procedureIdx, inst.assignment.dst);
		PIRPrintOut(context, " = ^");
		PrintIRValue(context, procedureIdx, inst.assignment.src);
		PIRPrintOut(context, "\n");
	} break;
	case IRINSTRUCTIONTYPE_COPY_MEMORY:
	{
		PIRPrintOut(context, "copyMemory(");
		PrintIRValue(context, procedureIdx, inst.copyMemory.dst);
		PIRPrintOut(context, ", ");
		PrintIRValue(context, procedureIdx, inst.copyMemory.src);
		PIRPrintOut(context, ", ");
		PrintIRValue(context, procedureIdx, inst.copyMemory.size);
		PIRPrintOut(context, ")\n");
	} break;
	case IRINSTRUCTIONTYPE_ZERO_MEMORY:
	{
		PIRPrintOut(context, "zeroMemory(");
		PrintIRValue(context, procedureIdx, inst.zeroMemory.dst);
		PIRPrintOut(context, ", ");
		PrintIRValue(context, procedureIdx, inst.zeroMemory.size);
		PIRPrintOut(context, ")\n");
	} break;
	default:
	{
		PIRPrintOut(context, "???INST\n");
	}
	}
}

void PrintJobIRInstructions(Context *context)
{
	static Mutex printIRMutex = SYSCreateMutex();

	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	const int padding = 20;
	Procedure proc = GetProcedureRead(context, jobData->procedureIdx);
	TypeInfoProcedure procTypeInfo = GetTypeInfo(context, proc.typeTableIdx).procedureInfo;

	SYSMutexLock(printIRMutex);

	BucketArrayInit(&context->outputBuffer);

	String name = proc.name;
	PIRPrintOut(context, "proc %S(", name);

	for (int paramIdx = 0; paramIdx < proc.parameterValues.size; ++paramIdx) {
		if (paramIdx) PIRPrintOut(context, ", ");
		u32 paramValueIdx = proc.parameterValues[paramIdx];
		Value paramValue = IRGetValue(context, paramValueIdx);
		String typeStr = TypeInfoToString(context, paramValue.typeTableIdx);
		PIRPrintOut(context, "%S : %S", paramValue.name, typeStr);
	}
	PIRPrintOut(context, ")");
	if (procTypeInfo.returnTypeIndices.size) {
		PIRPrintOut(context, " -> ");
		for (int returnIdx = 0; returnIdx < procTypeInfo.returnTypeIndices.size; ++returnIdx) {
			if (returnIdx) PIRPrintOut(context, ", ");
			String returnTypeStr = TypeInfoToString(context,
					procTypeInfo.returnTypeIndices[returnIdx]);
			PIRPrintOut(context, "%S", returnTypeStr);
		}
	}
	PIRPrintOut(context, "\n");

	const auto &instructions = proc.irInstructions;
	const u64 instructionCount = BucketArrayCount(&instructions);
	for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx) {
		IRInstruction inst = instructions[instructionIdx];

		if (inst.type == IRINSTRUCTIONTYPE_LABEL) {
			PIRPrintOut(context, "%S: ", inst.label->name);

			IRInstruction nextInst = instructions[instructionIdx + 1];
			if (nextInst.type != IRINSTRUCTIONTYPE_LABEL &&
				nextInst.type != IRINSTRUCTIONTYPE_PUSH_SCOPE &&
				nextInst.type != IRINSTRUCTIONTYPE_POP_SCOPE &&
				nextInst.type != IRINSTRUCTIONTYPE_NOP) {
				for (s64 i = inst.label->name.size + 2; i < padding; ++i)
					PIRPrintOut(context, " ");

				++instructionIdx;
				if (instructionIdx >= instructionCount)
					break;
				inst = instructions[instructionIdx];
			}
			else {
				PIRPrintOut(context, "\n");
				continue;
			}
		}
		else if (inst.type == IRINSTRUCTIONTYPE_PUSH_SCOPE ||
				 inst.type == IRINSTRUCTIONTYPE_POP_SCOPE ||
				 inst.type == IRINSTRUCTIONTYPE_NOP)
			continue;
		else {
			for (s64 i = 0; i < padding; ++i)
				PIRPrintOut(context, " ");
		}

		PrintIRInstruction(context, jobData->procedureIdx, inst);
	}
	PIRPrintOut(context, "\n");

	FileHandle outputFile = SYSOpenFileWrite("output/ir.txt"_s);
	for (int i = 0; i < context->outputBuffer.buckets.size; ++i) {
		SYSWriteFile(outputFile,
				context->outputBuffer.buckets[i].data,
				context->outputBuffer.buckets[i].size);
	}
	SYSCloseFile(outputFile);
	SYSMutexUnlock(printIRMutex);
}
