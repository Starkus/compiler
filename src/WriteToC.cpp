struct VariableStack
{
	DynamicArray<String, malloc, realloc> names;
	DynamicArray<u64,    malloc, realloc> offsets;
	u64 cursor;
};

String IRValueToStr(VariableStack *variableStack, IRValue value)
{
	String result = "???"_s;
	bool printTypeMemberAccess = false;

	String typeStr = IRTypeToStr(value.typeInfo.type);

	if (value.valueType == IRVALUETYPE_REGISTER)
	{
		result = TPrintF("r%d", value.registerIdx);
		printTypeMemberAccess = true;
	}
	else if (value.valueType == IRVALUETYPE_VARIABLE)
	{
		for (int i = 0; i < variableStack->names.size; ++i)
		{
			String varName = variableStack->names[i];
			if (StringEquals(varName, value.variable))
			{
				u64 offset = variableStack->offsets[i];
				if (offset == U64_MAX)
				{
					result = TPrintF("%.*s", varName.size, varName.data);
					printTypeMemberAccess = true;
				}
				else
				{
					String cast = value.typeInfo.isPointer ? "ptr"_s : typeStr;
					result = TPrintF("*((%.*s*)&stackBase[%d])", cast.size, cast.data, offset);
				}
				break;
			}
		}
	}
	else if (value.valueType == IRVALUETYPE_IMMEDIATE)
	{
		result = TPrintF("0x%x", value.immediate);
	}

	if (printTypeMemberAccess)
	{
		String member = value.typeInfo.isPointer ? "ptr"_s : typeStr;
		result = TPrintF("%.*s.%.*s_", result.size, result.data, member.size, member.data);
	}

	if (value.pointerType == IRPOINTERTYPE_POINTERTO)
		result = TPrintF("&(%.*s)", result.size, result.data);
	else if (value.pointerType == IRPOINTERTYPE_DEREFERENCE)
		result = TPrintF("*(%.*s*)%.*s", typeStr.size, typeStr.data, result.size, result.data);

	return result;
}

String IRValueToStrAsRegister(VariableStack *variableStack, IRValue value)
{
	if (value.valueType == IRVALUETYPE_REGISTER)
		return TPrintF("r%d", value.registerIdx);
	else
	{
		String result = IRValueToStr(variableStack, value);

		if (value.valueType == IRVALUETYPE_IMMEDIATE)
			result = TPrintF("FromU64(%.*s)", result.size, result.data);

		return result;
	}
}

String OperatorToStr(IRInstruction inst)
{
	switch (inst.type)
	{
	case IRINSTRUCTIONTYPE_ADD:
		return "+"_s;
	case IRINSTRUCTIONTYPE_SUBTRACT:
		return "-"_s;
	case IRINSTRUCTIONTYPE_MULTIPLY:
		return "*"_s;
	case IRINSTRUCTIONTYPE_DIVIDE:
		return "/"_s;
	case IRINSTRUCTIONTYPE_EQUALS:
		return "=="_s;
	case IRINSTRUCTIONTYPE_GREATER_THAN:
		return ">"_s;
	case IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS:
		return ">="_s;
	case IRINSTRUCTIONTYPE_LESS_THAN:
		return "<"_s;
	case IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS:
		return "<="_s;
	case IRINSTRUCTIONTYPE_NOT:
		return "!"_s;
	default:
		return "<?>"_s;
	}
}

void PrintOut(HANDLE outputFile, const char *format, ...)
{
	char *buffer = (char *)g_memory->framePtr;

	va_list args;
	va_start(args, format);

	stbsp_vsprintf(buffer, format, args);
	OutputDebugStringA(buffer);

	// Stdout
	DWORD bytesWritten;
	WriteFile(g_hStdout, buffer, (DWORD)strlen(buffer), &bytesWritten, nullptr);

	WriteFile(outputFile, buffer, (DWORD)strlen(buffer), &bytesWritten, nullptr);

	va_end(args);
}

void WriteToC(Context *context)
{
	VariableStack variableStack;
	DynamicArrayInit(&variableStack.names, 128);
	DynamicArrayInit(&variableStack.offsets, 128);
	variableStack.cursor = 0;

	HANDLE outputFile = CreateFileA(
			"out.c",
			GENERIC_WRITE,
			0,
			nullptr,
			CREATE_ALWAYS,
			FILE_ATTRIBUTE_NORMAL,
			nullptr
			);

	PrintOut(outputFile, "#include \"emi.c\"\n\n");

	const u64 procedureCount = context->irProcedures.size;
	for (int procedureIdx = 0; procedureIdx < procedureCount; ++procedureIdx)
	{
		IRProcedure proc = context->irProcedures[procedureIdx];

		{
			String returnTypeStr = IRTypeInfoToStr(proc.returnTypeInfo);

			// @Hack: write 'main' return type as 'int' for compatibility
			if (StringEquals(proc.name, "main"_s))
				returnTypeStr = "int"_s;

			PrintOut(outputFile, "%.*s %.*s(", returnTypeStr.size, returnTypeStr.data, proc.name.size, proc.name.data);
			for (int i = 0; i < proc.parameters.size; ++i)
			{
				if (i) PrintOut(outputFile, ", ");
				PrintOut(outputFile, "Register %.*s", proc.parameters[i].name.size,
						proc.parameters[i].name.data);
			}
			PrintOut(outputFile, ") {\n");
		}

		PrintOut(outputFile, "u8 stackBase[%d];\n", 256);

		// Declare registers
		PrintOut(outputFile, "Register r1");
		for (int regIdx = 2; regIdx < context->currentRegisterId; ++regIdx)
		{
			PrintOut(outputFile, ", r%d", regIdx);
		}
		PrintOut(outputFile, ";\n");

		// Add parameters to variable stack
		for (int paramIdx = 0; paramIdx < proc.parameters.size; ++paramIdx)
		{
			IRVariable *param = &proc.parameters[paramIdx];
			*DynamicArrayAdd(&variableStack.names) = param->name;
			*DynamicArrayAdd(&variableStack.offsets) = U64_MAX; // FFFFFFFF offset means parameter @Cleanup
		}

		const u64 instructionCount = BucketArrayCount(&proc.instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			IRInstruction inst = proc.instructions[instructionIdx];

			if (inst.type == IRINSTRUCTIONTYPE_VARIABLE_DECLARATION)
			{
				u64 offset = variableStack.cursor;
				*DynamicArrayAdd(&variableStack.names) = inst.variableDeclaration.name;
				*DynamicArrayAdd(&variableStack.offsets) = offset;
				variableStack.cursor += inst.variableDeclaration.size;

				PrintOut(outputFile, "/* Declare variable '%.*s' at stackBase[%d] (size %d) */\n",
						inst.variableDeclaration.name.size, inst.variableDeclaration.name.data, offset,
						inst.variableDeclaration.size);
			}
			else if (inst.type >= IRINSTRUCTIONTYPE_BINARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_BINARY_END)
			{
				String out = IRValueToStr(&variableStack, inst.binaryOperation.out);
				String left = IRValueToStr(&variableStack, inst.binaryOperation.left);
				String op = OperatorToStr(inst);
				String right = IRValueToStr(&variableStack, inst.binaryOperation.right);
				PrintOut(outputFile, "%.*s = %.*s %.*s %.*s;\n", out.size, out.data, left.size, left.data, op.size,
						op.data, right.size, right.data);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_ASSIGNMENT)
			{
				String src = IRValueToStr(&variableStack, inst.assignment.src);
				String dst = IRValueToStr(&variableStack, inst.assignment.dst);
				PrintOut(outputFile, "%.*s = %.*s;\n", dst.size, dst.data, src.size, src.data);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_DEREFERENCE)
			{
				String src = IRValueToStr(&variableStack, inst.assignment.src);
				String dst = IRValueToStr(&variableStack, inst.assignment.dst);
				String type = IRTypeInfoToStr(inst.assignment.dst.typeInfo);
				PrintOut(outputFile, "%.*s = *(%.*s*)%.*s;\n", dst.size, dst.data, type.size,
						type.data, src.size, src.data);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_MEMBER_ADDRESS)
			{
				// Compute struct offset
				TypeInfo *typeInfo  = nullptr;
				for (int i = 0; i < context->typeTable.size; ++i)
				{
					TypeInfo *currentTypeInfo  = &context->typeTable[i];
					if (currentTypeInfo->typeCategory == TYPECATEGORY_STRUCT &&
							StringEquals(currentTypeInfo->structInfo.name,
								inst.memberAddress.structName))
					{
						typeInfo = currentTypeInfo;
						break;
					}
				}
				ASSERT(typeInfo);

				u64 offset = U64_MAX;
				for (int i = 0; i < typeInfo->structInfo.members.size; ++i)
				{
					StructMember *currentMember = &typeInfo->structInfo.members[i];
					if (StringEquals(currentMember->name, inst.memberAddress.memberName))
					{
						offset = currentMember->offset;
					}
				}
				ASSERT(offset != U64_MAX);

				String out = IRValueToStr(&variableStack, inst.memberAddress.out);
				String base = IRValueToStr(&variableStack, inst.memberAddress.in);
				PrintOut(outputFile, "%.*s = ((ptr)&(%.*s)) + %d;\n", out.size, out.data, base.size, base.data, offset);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_CALL)
			{
				PrintOut(outputFile, "%.*s(", inst.call.label.size, inst.call.label.data);
				for (int i = 0; i < inst.call.parameters.size; ++i)
				{
					String param = IRValueToStrAsRegister(&variableStack, inst.call.parameters[i]);
					if (i > 0) PrintOut(outputFile, ", ");
					PrintOut(outputFile, "%.*s", param.size, param.data);
				}
				PrintOut(outputFile, ");\n");
			}
			else if (inst.type == IRINSTRUCTIONTYPE_RETURN)
			{
				String returnStr = IRValueToStr(&variableStack, inst.returnValue);
				PrintOut(outputFile, "return %.*s;\n", returnStr.size, returnStr.data);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_LABEL)
			{
				String label = inst.label;
				PrintOut(outputFile, "%.*s:\n", label.size, label.data);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_JUMP)
			{
				String label = inst.conditionalJump.label;
				PrintOut(outputFile, "goto %.*s;\n", label.size, label.data);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_JUMP_IF_ZERO)
			{
				String label = inst.conditionalJump.label;
				String condition = IRValueToStr(&variableStack, inst.conditionalJump.condition);
				PrintOut(outputFile, "if (!%.*s) goto %.*s;\n", condition.size, condition.data,
						label.size, label.data);
			}
		}
		PrintOut(outputFile, "}\n");
	}

	CloseHandle(outputFile);
}
