struct VariableStack
{
	DynamicArray<String, malloc, realloc> names;
	DynamicArray<u64,    malloc, realloc> offsets;
	u64 cursor;
};

String IRValueToStr(Context *context, VariableStack *variableStack, IRValue value)
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
		bool found = false;
		// Search stack variables
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
					String cast;
					if (value.typeInfo.isPointer)
						cast = "ptr"_s;
					else
						cast = typeStr;
					result = TPrintF("/* %.*s */ *(%.*s*)(stack + 0x%x)", varName.size,
							varName.data, cast.size, cast.data, offset);
				}
				found = true;
				break;
			}
		}
		if (!found)
		{
			// Search static variables
			const u64 staticVariableCount = context->irStaticVariables.size;
			for (int staticVariableIdx = 0; staticVariableIdx < staticVariableCount; ++staticVariableIdx)
			{
				String staticVarName = context->irStaticVariables[staticVariableIdx].name;
				if (StringEquals(staticVarName, value.variable))
				{
					result = TPrintF("%.*s", staticVarName.size, staticVarName.data);
				}
			}
		}
	}
	else if (value.valueType == IRVALUETYPE_IMMEDIATE)
	{
		result = TPrintF("0x%x", value.immediate);
	}
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_FLOAT)
	{
		result = TPrintF("%f", value.immediateFloat);
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

String IRValueToStrAsRegister(Context *context, VariableStack *variableStack, IRValue value)
{
	if (value.valueType == IRVALUETYPE_REGISTER)
		return TPrintF("r%d", value.registerIdx);
	else
	{
		String result = IRValueToStr(context, variableStack, value);

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

String GetProcedureSignature(IRProcedure proc)
{
	String result;

	String returnTypeStr = IRTypeInfoToStr(proc.returnTypeInfo);
	// @Hack: write 'main' return type as 'int' for compatibility
	if (StringEquals(proc.name, "main"_s))
		returnTypeStr = "int"_s;

	result = TPrintF("%.*s %.*s(", returnTypeStr.size, returnTypeStr.data, proc.name.size, proc.name.data);
	for (int i = 0; i < proc.parameters.size; ++i)
	{
		if (i) result = StringConcat(result, ", "_s);
		result = TPrintF("%.*sRegister %.*s", result.size, result.data, proc.parameters[i].name.size,
				proc.parameters[i].name.data);
	}
	result = StringConcat(result, ")"_s);
	return result;
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
	const u32 stackSize = 256; // @Improve

	VariableStack variableStack;
	DynamicArrayInit(&variableStack.names, 128);
	DynamicArrayInit(&variableStack.offsets, 128);
	variableStack.cursor = stackSize;

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

	const u64 staticVariableCount = context->irStaticVariables.size;
	for (int staticVariableIdx = 0; staticVariableIdx < staticVariableCount; ++staticVariableIdx)
	{
		IRStaticVariable staticVar = context->irStaticVariables[staticVariableIdx];

		String varType = IRTypeInfoToStr(staticVar.typeInfo);

		PrintOut(outputFile, "%.*s %.*s", varType.size, varType.data, staticVar.name.size,
				staticVar.name.data);
		if (staticVar.initialValue.valueType != IRVALUETYPE_INVALID)
		{
			if (staticVar.initialValue.valueType == IRVALUETYPE_IMMEDIATE)
			{
				PrintOut(outputFile, " = 0x%x", staticVar.initialValue.immediate);
			}
			else if (staticVar.initialValue.valueType == IRVALUETYPE_IMMEDIATE_FLOAT)
			{
				PrintOut(outputFile, " = %f", staticVar.initialValue.immediateFloat);
			}
			else
			{
				CRASH; // Non literal values not supported here.
			}
		}
		PrintOut(outputFile, ";\n");
	}

	PrintOut(outputFile, "\n");

	PrintOut(outputFile, "// Forward declaration of procedures\n");
	const u64 procedureCount = context->irProcedures.size;
	for (int procedureIdx = 0; procedureIdx < procedureCount; ++procedureIdx)
	{
		IRProcedure proc = context->irProcedures[procedureIdx];
		String signature = GetProcedureSignature(proc);
		PrintOut(outputFile, "%.*s;\n", signature.size, signature.data);
	}

	PrintOut(outputFile, "\n");

	for (int procedureIdx = 0; procedureIdx < procedureCount; ++procedureIdx)
	{
		IRProcedure proc = context->irProcedures[procedureIdx];

		String signature = GetProcedureSignature(proc);
		PrintOut(outputFile, "%.*s {\n", signature.size, signature.data);

		PrintOut(outputFile, "u8 stack[%d];\n", stackSize);

		// Declare registers
		if (proc.registerCount)
		{
			PrintOut(outputFile, "Register r0");
			for (int regIdx = 1; regIdx < proc.registerCount; ++regIdx)
			{
				PrintOut(outputFile, ", r%d", regIdx);
			}
			PrintOut(outputFile, ";\n");
		}

		PrintOut(outputFile, "\n");

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
				variableStack.cursor -= inst.variableDeclaration.size;
				u64 offset = variableStack.cursor;
				*DynamicArrayAdd(&variableStack.names) = inst.variableDeclaration.name;
				*DynamicArrayAdd(&variableStack.offsets) = offset;

				PrintOut(outputFile, "/* Declare variable '%.*s' at stack + 0x%x (size %d) */\n",
						inst.variableDeclaration.name.size, inst.variableDeclaration.name.data, offset,
						inst.variableDeclaration.size);
			}
			else if (inst.type >= IRINSTRUCTIONTYPE_UNARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_UNARY_END)
			{
				String out = IRValueToStr(context, &variableStack, inst.unaryOperation.out);
				String in = IRValueToStr(context, &variableStack, inst.unaryOperation.in);
				String op = OperatorToStr(inst);
				PrintOut(outputFile, "%.*s = %.*s%.*s;\n", out.size, out.data, op.size, op.data, in.size,
						in.data);
			}
			else if (inst.type >= IRINSTRUCTIONTYPE_BINARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_BINARY_END)
			{
				String out = IRValueToStr(context, &variableStack, inst.binaryOperation.out);
				String left = IRValueToStr(context, &variableStack, inst.binaryOperation.left);
				String op = OperatorToStr(inst);
				String right = IRValueToStr(context, &variableStack, inst.binaryOperation.right);
				PrintOut(outputFile, "%.*s = %.*s %.*s %.*s;\n", out.size, out.data, left.size, left.data, op.size,
						op.data, right.size, right.data);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_ASSIGNMENT)
			{
				String src = IRValueToStr(context, &variableStack, inst.assignment.src);
				String dst = IRValueToStr(context, &variableStack, inst.assignment.dst);
				PrintOut(outputFile, "%.*s = %.*s;\n", dst.size, dst.data, src.size, src.data);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_MEMBER_ACCESS)
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

				String out = IRValueToStr(context, &variableStack, inst.memberAddress.out);
				String base = IRValueToStr(context, &variableStack, inst.memberAddress.in);
				PrintOut(outputFile, "%.*s = ((u8*)&(%.*s)) + %d;\n", out.size, out.data, base.size, base.data, offset);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_CALL)
			{
				PrintOut(outputFile, "%.*s(", inst.call.label.size, inst.call.label.data);
				for (int i = 0; i < inst.call.parameters.size; ++i)
				{
					String param = IRValueToStrAsRegister(context, &variableStack, inst.call.parameters[i]);
					if (i > 0) PrintOut(outputFile, ", ");
					PrintOut(outputFile, "%.*s", param.size, param.data);
				}
				PrintOut(outputFile, ");\n");
			}
			else if (inst.type == IRINSTRUCTIONTYPE_RETURN)
			{
				String returnStr = IRValueToStr(context, &variableStack, inst.returnValue);
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
				String condition = IRValueToStr(context, &variableStack, inst.conditionalJump.condition);
				PrintOut(outputFile, "if (!%.*s) goto %.*s;\n", condition.size, condition.data,
						label.size, label.data);
			}
		}
		PrintOut(outputFile, "}\n\n");
	}

	CloseHandle(outputFile);
}
