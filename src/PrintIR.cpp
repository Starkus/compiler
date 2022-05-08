s64 PIRPrintOut(Context *context, const char *format, ...)
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

inline String PIRValueToStr(Context *context, u32 valueIdx)
{
	Value v = context->values[valueIdx];
	if (v.name)
		return TPrintF("$v%u\"%S\"", valueIdx, v.name);
	else
		return TPrintF("$v%u", valueIdx);
}

void PrintIRValue(Context *context, IRValue value)
{
	if (value.valueType == IRVALUETYPE_VALUE_DEREFERENCE)
	{
		PIRPrintOut(context, "[%S", PIRValueToStr(context, value.value.valueIdx));
		if (value.value.offset)
			PIRPrintOut(context, "+0x%llx", value.value.offset);
		PIRPrintOut(context, "]");
	}
	else if (value.valueType == IRVALUETYPE_VALUE)
	{
		PIRPrintOut(context, "%S", PIRValueToStr(context, value.value.valueIdx));
		if (value.value.offset)
			PIRPrintOut(context, "+0x%llx", value.value.offset);
	}
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		PIRPrintOut(context, "0x%llx", value.immediate);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_FLOAT)
		PIRPrintOut(context, "%f", value.immediateFloat);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_STRING)
		PIRPrintOut(context, "\"%S\"", context->stringLiterals[value.immediateStringIdx]);
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

void PrintIRInstruction(Context *context, IRInstruction inst)
{
	if (inst.type >= IRINSTRUCTIONTYPE_UNARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_UNARY_END)
	{
		PrintIRValue(context, inst.unaryOperation.out);
		PIRPrintOut(context, " := ");
		PrintIRInstructionOperator(context, inst);
		PrintIRValue(context, inst.unaryOperation.in);
		PIRPrintOut(context, "\n");
	}
	else if (inst.type >= IRINSTRUCTIONTYPE_BINARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_BINARY_END)
	{
		PrintIRValue(context, inst.binaryOperation.out);
		PIRPrintOut(context, " := ");
		PrintIRValue(context, inst.binaryOperation.left);
		PIRPrintOut(context, " ");
		PrintIRInstructionOperator(context, inst);
		PIRPrintOut(context, " ");
		PrintIRValue(context, inst.binaryOperation.right);
		PIRPrintOut(context, "\n");
	}
	else switch (inst.type)
	{
	case IRINSTRUCTIONTYPE_LABEL:
	{
		PIRPrintOut(context, "%S:\n", inst.label);
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
		PIRPrintOut(context, "jump \"%S\"\n", inst.jump.label);
	} break;
	case IRINSTRUCTIONTYPE_JUMP_IF_ZERO:
	{
		PIRPrintOut(context, "if !");
		PrintIRValue(context, inst.conditionalJump.condition);
		PIRPrintOut(context, " jump %S\n", inst.conditionalJump.label);
	} break;
	case IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO:
	{
		PIRPrintOut(context, "if ");
		PrintIRValue(context, inst.conditionalJump.condition);
		PIRPrintOut(context, " jump %S\n", inst.conditionalJump.label);
	} break;
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL:
	{
		if (inst.procedureCall.out.valueType != IRVALUETYPE_INVALID)
		{
			PrintIRValue(context, inst.procedureCall.out);
			PIRPrintOut(context, " := ");
		}
		String name = GetProcedure(context,
				inst.procedureCall.procedureIdx)->name;
		PIRPrintOut(context, "call %S(", name);

		for (int i = 0; i < inst.procedureCall.parameters.size; ++i)
		{
			if (i) PIRPrintOut(context, ", ");
			PrintIRValue(context, inst.procedureCall.parameters[i]);
		}
		PIRPrintOut(context, ")\n");
	} break;
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL_INDIRECT:
	{
		if (inst.procedureCall.out.valueType != IRVALUETYPE_INVALID)
		{
			PrintIRValue(context, inst.procedureCall.out);
			PIRPrintOut(context, " := ");
		}
		PIRPrintOut(context, "call virtual ");
		PrintIRValue(context, inst.procedureCall.procIRValue);
		PIRPrintOut(context, "(");

		for (int i = 0; i < inst.procedureCall.parameters.size; ++i)
		{
			if (i) PIRPrintOut(context, ", ");
			PrintIRValue(context, inst.procedureCall.parameters[i]);
		}
		PIRPrintOut(context, ")\n");
	} break;
	case IRINSTRUCTIONTYPE_PUSH_VALUE:
	{
		PIRPrintOut(context, "push value %S\n", PIRValueToStr(context, inst.pushValue.valueIdx));
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
		PrintIRValue(context, inst.assignment.dst);
		PIRPrintOut(context, " = ");
		PrintIRValue(context, inst.assignment.src);
		PIRPrintOut(context, "\n");
	} break;
	case IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS:
	{
		PrintIRValue(context, inst.assignment.dst);
		PIRPrintOut(context, " = ^");
		PrintIRValue(context, inst.assignment.src);
		PIRPrintOut(context, "\n");
	} break;
	case IRINSTRUCTIONTYPE_COPY_MEMORY:
	{
		PIRPrintOut(context, "copyMemory(");
		PrintIRValue(context, inst.copyMemory.dst);
		PIRPrintOut(context, ", ");
		PrintIRValue(context, inst.copyMemory.src);
		PIRPrintOut(context, ", ");
		PrintIRValue(context, inst.copyMemory.size);
		PIRPrintOut(context, ")\n");
	} break;
	case IRINSTRUCTIONTYPE_ZERO_MEMORY:
	{
		PIRPrintOut(context, "zeroMemory(");
		PrintIRValue(context, inst.zeroMemory.dst);
		PIRPrintOut(context, ", ");
		PrintIRValue(context, inst.zeroMemory.size);
		PIRPrintOut(context, ")\n");
	} break;
	default:
	{
		PIRPrintOut(context, "???INST\n");
	}
	}
}

void PrintIRInstructions(Context *context)
{
	BucketArrayInit(&context->outputBuffer);

	const int padding = 20;
	const u64 procedureCount = BucketArrayCount(&context->procedures);
	for (int procedureIdx = 1; procedureIdx < procedureCount; ++procedureIdx)
	{
		Procedure *proc = GetProcedure(context, procedureIdx);
		TypeInfoProcedure procTypeInfo = context->typeTable[proc->typeTableIdx].procedureInfo;

		String returnTypeStr = TypeInfoToString(context, procTypeInfo.returnTypeTableIdx);

		String name = GetProcedure(context, procedureIdx)->name;
		PIRPrintOut(context, "proc %S(", name);

		for (int paramIdx = 0; paramIdx < proc->parameterValues.size; ++paramIdx)
		{
			if (paramIdx) PIRPrintOut(context, ", ");
			u32 paramValueIdx = proc->parameterValues[paramIdx];
			Value paramValue = context->values[paramValueIdx];
			String typeStr = TypeInfoToString(context, paramValue.typeTableIdx);
			PIRPrintOut(context, "%S : %S", paramValue.name, typeStr);
		}
		PIRPrintOut(context, ")");
		if (procTypeInfo.returnTypeTableIdx != TYPETABLEIDX_VOID)
			PIRPrintOut(context, " -> %S", returnTypeStr);
		PIRPrintOut(context, "\n");

		const u64 instructionCount = BucketArrayCount(&proc->instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			IRInstruction inst = proc->instructions[instructionIdx];

			if (inst.type == IRINSTRUCTIONTYPE_LABEL)
			{
				PIRPrintOut(context, "%S: ", inst.label->name);

				IRInstruction nextInst = proc->instructions[instructionIdx + 1];
				if (nextInst.type != IRINSTRUCTIONTYPE_LABEL &&
					nextInst.type != IRINSTRUCTIONTYPE_PUSH_SCOPE &&
					nextInst.type != IRINSTRUCTIONTYPE_POP_SCOPE &&
					nextInst.type != IRINSTRUCTIONTYPE_NOP)
				{
					for (s64 i = inst.label->name.size + 2; i < padding; ++i)
						PIRPrintOut(context, " ");

					++instructionIdx;
					if (instructionIdx >= instructionCount)
						break;
					inst = proc->instructions[instructionIdx];
				}
				else
				{
					PIRPrintOut(context, "\n");
					continue;
				}
			}
			else if (inst.type == IRINSTRUCTIONTYPE_PUSH_SCOPE ||
					 inst.type == IRINSTRUCTIONTYPE_POP_SCOPE ||
					 inst.type == IRINSTRUCTIONTYPE_NOP)
				continue;
			else
			{
				for (s64 i = 0; i < padding; ++i)
					PIRPrintOut(context, " ");
			}

			PrintIRInstruction(context, inst);
		}
	}
	PIRPrintOut(context, "\n");

	HANDLE outputFile = CreateFileA(
			"output/ir.txt",
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
