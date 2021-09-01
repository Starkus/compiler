String IRValueToStr(Context *context, IRValue value)
{
	String result = "???"_s;
	bool printTypeMemberAccess = false;

	String typeStr = IRTypeToStr(value.typeInfo.type);

	String castStr;
	if (value.typeInfo.isPointer)
		castStr = "ptr"_s;
	else
		castStr = typeStr;

	if (value.valueType == IRVALUETYPE_REGISTER)
	{
		result = TPrintF("r%d", value.registerIdx);
		printTypeMemberAccess = true;
	}
	else if (value.valueType == IRVALUETYPE_VARIABLE)
	{
		u64 offset = value.variable->stackOffset;
		if (offset == U64_MAX)
		{
			result = TPrintF("%.*s", value.variable->name.size, value.variable->name.data);
			printTypeMemberAccess = true;
		}
		else
		{
			result = TPrintF("/* %.*s */ *(%.*s*)(stack + 0x%x)", value.variable->name.size,
					value.variable->name.data, castStr.size, castStr.data,
					value.variable->stackOffset);
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
		result = TPrintF("%.*s.%.*s_", result.size, result.data, castStr.size, castStr.data);
	}

	if (value.pointerType == IRPOINTERTYPE_POINTERTO)
		result = TPrintF("&(%.*s)", result.size, result.data);
	else if (value.pointerType == IRPOINTERTYPE_DEREFERENCE)
		result = TPrintF("*(%.*s*)%.*s", typeStr.size, typeStr.data, result.size, result.data);

	return result;
}

String IRValueToStrAsRegister(Context *context, IRValue value)
{
	if (value.valueType == IRVALUETYPE_REGISTER)
		return TPrintF("r%d", value.registerIdx);
	else
	{
		String result = IRValueToStr(context, value);

		if (value.valueType == IRVALUETYPE_IMMEDIATE)
			result = TPrintF("FromU64(%.*s)", result.size, result.data);
		else if (value.valueType == IRVALUETYPE_IMMEDIATE_FLOAT)
			result = TPrintF("FromF64(%.*s)", result.size, result.data);

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
		Variable *param = proc.parameters[i];
		result = TPrintF("%.*sRegister %.*s", result.size, result.data, param->name.size,
				param->name.data);
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

	DWORD bytesWritten;
#if PRINT_C_OUTPUT
	OutputDebugStringA(buffer);
	WriteFile(g_hStdout, buffer, (DWORD)strlen(buffer), &bytesWritten, nullptr); // Stdout
#endif
	WriteFile(outputFile, buffer, (DWORD)strlen(buffer), &bytesWritten, nullptr);

	va_end(args);
}

void WriteToC(Context *context)
{
	u64 stackCursor;

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
		staticVar.variable->stackOffset = U64_MAX;

		String varType;
		if (staticVar.initialValue.valueType == IRVALUETYPE_IMMEDIATE_STRING)
			varType = "String"_s;
		else
			varType = IRTypeInfoToStr(staticVar.typeInfo);

		PrintOut(outputFile, "%.*s %.*s", varType.size, varType.data, staticVar.variable->name.size,
				staticVar.variable->name.data);
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
			else if (staticVar.initialValue.valueType == IRVALUETYPE_IMMEDIATE_STRING)
			{
				PrintOut(outputFile, " = { %llu, (u8 *)\"%.*s\" }", staticVar.initialValue.immediateString.size,
						staticVar.initialValue.immediateString.size,
						staticVar.initialValue.immediateString.data);
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

		// @Speed: separate array of external procedures to avoid branching
		if (proc.isExternal)
			continue;

		String signature = GetProcedureSignature(proc);
		PrintOut(outputFile, "%.*s;\n", signature.size, signature.data);
	}

	PrintOut(outputFile, "\n");

	for (int procedureIdx = 0; procedureIdx < procedureCount; ++procedureIdx)
	{
		IRProcedure proc = context->irProcedures[procedureIdx];

		// @Speed: separate array of external procedures to avoid branching
		if (proc.isExternal)
			continue;

		String signature = GetProcedureSignature(proc);
		PrintOut(outputFile, "%.*s {\n", signature.size, signature.data);

		u64 stackSize = 0;
		// Dry run to know stack size
		const u64 instructionCount = BucketArrayCount(&proc.instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			IRInstruction inst = proc.instructions[instructionIdx];
			if (inst.type == IRINSTRUCTIONTYPE_VARIABLE_DECLARATION)
			{
				stackSize += inst.variableDeclaration.size;
			}
		}
		stackCursor = stackSize;

		if (stackSize)
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
			Variable *param = proc.parameters[paramIdx];
			param->stackOffset = U64_MAX; // FFFFFFFF offset means parameter @Cleanup
		}

		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			IRInstruction inst = proc.instructions[instructionIdx];

			if (inst.type == IRINSTRUCTIONTYPE_VARIABLE_DECLARATION)
			{
				Variable *var = inst.variableDeclaration.variable;
				stackCursor -= inst.variableDeclaration.size;
				var->stackOffset = stackCursor;

				PrintOut(outputFile, "/* Declare variable '%.*s' at stack + 0x%x (size %d) */\n",
						var->name.size, var->name.data, var->stackOffset,
						inst.variableDeclaration.size);
			}
			else if (inst.type >= IRINSTRUCTIONTYPE_UNARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_UNARY_END)
			{
				String out = IRValueToStr(context, inst.unaryOperation.out);
				String in = IRValueToStr(context, inst.unaryOperation.in);
				String op = OperatorToStr(inst);
				PrintOut(outputFile, "%.*s = %.*s%.*s;\n", out.size, out.data, op.size, op.data, in.size,
						in.data);
			}
			else if (inst.type >= IRINSTRUCTIONTYPE_BINARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_BINARY_END)
			{
				String out = IRValueToStr(context, inst.binaryOperation.out);
				String left = IRValueToStr(context, inst.binaryOperation.left);
				String op = OperatorToStr(inst);
				String right = IRValueToStr(context, inst.binaryOperation.right);
				PrintOut(outputFile, "%.*s = %.*s %.*s %.*s;\n", out.size, out.data, left.size, left.data, op.size,
						op.data, right.size, right.data);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_ASSIGNMENT)
			{
				String src = IRValueToStr(context, inst.assignment.src);
				String dst = IRValueToStr(context, inst.assignment.dst);
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
								inst.memberAccess.structName))
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
					if (StringEquals(currentMember->name, inst.memberAccess.memberName))
					{
						offset = currentMember->offset;
					}
				}
				ASSERT(offset != U64_MAX);

				String out = IRValueToStr(context, inst.memberAccess.out);
				String base = IRValueToStr(context, inst.memberAccess.in);
				PrintOut(outputFile, "%.*s = ((u8*)(%.*s)) + %d;\n", out.size, out.data, base.size, base.data, offset);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_ARRAY_ACCESS)
			{
				String out = IRValueToStr(context, inst.arrayAccess.out);
				String array = IRValueToStr(context, inst.arrayAccess.left);
				String index = IRValueToStr(context, inst.arrayAccess.right);
				PrintOut(outputFile, "%.*s = ((u8*)&(%.*s)) + %.*s * %llu;\n", out.size, out.data, array.size,
						array.data, index.size, index.data, inst.arrayAccess.elementSize);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_PROCEDURE_CALL)
			{
				if (inst.procedureCall.out.valueType != IRVALUETYPE_INVALID)
				{
					String out = IRValueToStr(context, inst.procedureCall.out);
					PrintOut(outputFile, "%.*s = ", out.size, out.data);
				}
				PrintOut(outputFile, "%.*s(", inst.procedureCall.label.size, inst.procedureCall.label.data);
				for (int i = 0; i < inst.procedureCall.parameters.size; ++i)
				{
					String param;
					if (inst.procedureCall.isExternal)
						param = IRValueToStr(context, inst.procedureCall.parameters[i]);
					else
						param = IRValueToStrAsRegister(context, inst.procedureCall.parameters[i]);
					if (i > 0) PrintOut(outputFile, ", ");
					PrintOut(outputFile, "%.*s", param.size, param.data);
				}
				PrintOut(outputFile, ");\n");
			}
			else if (inst.type == IRINSTRUCTIONTYPE_RETURN)
			{
				String returnStr = IRValueToStr(context, inst.returnValue);
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
				String condition = IRValueToStr(context, inst.conditionalJump.condition);
				PrintOut(outputFile, "if (!%.*s) goto %.*s;\n", condition.size, condition.data,
						label.size, label.data);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY)
			{
				String src = IRValueToStrAsRegister(context, inst.memcpy.src);
				String dst = IRValueToStrAsRegister(context, inst.memcpy.dst);
				PrintOut(outputFile, "memcpy(%.*s, %.*s, %llu);\n", dst.size, dst.data, src.size,
						src.data, inst.memcpy.size);
			}
		}
		PrintOut(outputFile, "}\n\n");
	}

	CloseHandle(outputFile);
}
