String CTypeInfoToString(Context *context, s64 typeTableIdx)
{
	TypeInfo *typeInfo = &context->typeTable[typeTableIdx];
	switch (typeInfo->typeCategory)
	{
	case TYPECATEGORY_INVALID:
		return "void"_s;
	case TYPECATEGORY_POINTER:
		return "ptr"_s;
	case TYPECATEGORY_STRUCT:
		return "strct"_s;
	case TYPECATEGORY_ARRAY:
		return "arr"_s;
	case TYPECATEGORY_ENUM:
		return CTypeInfoToString(context, typeInfo->enumInfo.typeTableIdx);
	case TYPECATEGORY_INTEGER:
	{
		if (typeInfo->integerInfo.isSigned) switch (typeInfo->integerInfo.size)
		{
			case 1: return "s8"_s;
			case 2: return "s16"_s;
			case 4: return "s32"_s;
			case 8: return "s64"_s;
		}
		else switch (typeInfo->integerInfo.size)
		{
			case 1: return "u8"_s;
			case 2: return "u16"_s;
			case 4: return "u32"_s;
			case 8: return "u64"_s;
		}
	} break;
	case TYPECATEGORY_FLOATING:
	{
		switch (typeInfo->floatingInfo.size)
		{
			case 4: return "f32"_s;
			case 8: return "f64"_s;
		}
	} break;
	}
	return "???TYPE"_s;
}

s64 CCalculateTypeSize(Context *context, s64 typeTableIdx)
{
	ASSERT(typeTableIdx >= 0);
	TypeInfo *typeInfo  = &context->typeTable[typeTableIdx];

	switch (typeInfo->typeCategory)
	{
	case TYPECATEGORY_INTEGER:
		return typeInfo->integerInfo.size;

	case TYPECATEGORY_FLOATING:
		return typeInfo->floatingInfo.size;

	case TYPECATEGORY_STRUCT:
		return typeInfo->structInfo.size;

	case TYPECATEGORY_ENUM:
		return CCalculateTypeSize(context, typeInfo->enumInfo.typeTableIdx);

	case TYPECATEGORY_POINTER:
		return 8;

	case TYPECATEGORY_ARRAY:
		return CCalculateTypeSize(context, typeInfo->arrayInfo.elementTypeTableIdx) * typeInfo->arrayInfo.count;
	}

	return 0;
}

String CIRValueToStr(Context *context, IRValue value)
{
	String result = "???VALUE"_s;
	bool printTypeMemberAccess = false;

	if (value.valueType == IRVALUETYPE_REGISTER)
	{
		switch (value.registerIdx)
		{
		case IRSPECIALREGISTER_RETURN:
			result = "rRet"_s;
			break;
		case IRSPECIALREGISTER_SHOULD_RETURN:
			result = "rDoRet"_s;
			break;
		default:
			result = TPrintF("r%lld", value.registerIdx);
			break;
		}
		printTypeMemberAccess = true;
	}
	else if (value.valueType == IRVALUETYPE_PARAMETER)
	{
		result = TPrintF("param%hhd", value.variable->parameterIndex);
		printTypeMemberAccess = true;
	}
	else if (value.valueType == IRVALUETYPE_VARIABLE)
	{
		if (value.variable->isStatic)
		{
			result = TPrintF("%S", value.variable->name);
		}
		else if (value.variable->parameterIndex >= 0)
		{
			result = TPrintF("param%hhd", value.variable->parameterIndex);
			printTypeMemberAccess = true; // Parameters are registers
		}
		else
		{
			result = TPrintF("/* %S */ stack + 0x%x", value.variable->name,
					value.variable->stackOffset);
		}
	}
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
	{
		result = TPrintF("0x%x", value.immediate);
	}
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_FLOAT)
	{
		result = TPrintF("%f", value.immediateFloat);
	}
	else if (value.valueType == IRVALUETYPE_SIZEOF)
	{
		result = TPrintF("%llu", CCalculateTypeSize(context, value.sizeOfTypeTableIdx));
	}
	else if (value.valueType == IRVALUETYPE_TYPEOF)
	{
		result = TPrintF("&_typeInfo%lld", value.typeOfTypeTableIdx);
	}
	else
		ASSERT(!"Invalid value type!");

	if (printTypeMemberAccess)
	{
		TypeInfo *typeInfo = &context->typeTable[value.typeTableIdx];
		bool isPointer = typeInfo->typeCategory == TYPECATEGORY_POINTER;

		String castStr;
		if (isPointer || value.dereference)
			castStr = "ptr"_s;
		else
			castStr = CTypeInfoToString(context, value.typeTableIdx);
		result = TPrintF("%S.%S_", result, castStr);
	}

	if (value.dereference)
	{
		String castStr = CTypeInfoToString(context, value.typeTableIdx);
		result = TPrintF("*(%S*)(%S)", castStr, result);
	}

	return result;
}

String CIRValueToStrAsRegister(Context *context, IRValue value)
{
	if (value.valueType == IRVALUETYPE_REGISTER)
		return TPrintF("r%d", value.registerIdx);
	else
		ASSERT(!"Value is not a register!");
	return "???REG"_s;
}

String OperatorToStr(IRInstruction inst)
{
	switch (inst.type)
	{
	case IRINSTRUCTIONTYPE_ADD:
		return "+"_s;
	case IRINSTRUCTIONTYPE_SUBTRACT:
	case IRINSTRUCTIONTYPE_SUBTRACT_UNARY:
		return "-"_s;
	case IRINSTRUCTIONTYPE_MULTIPLY:
		return "*"_s;
	case IRINSTRUCTIONTYPE_DIVIDE:
		return "/"_s;
	case IRINSTRUCTIONTYPE_MODULO:
		return "%"_s;
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

String CProcedureToLabel(Context *context, Procedure *proc)
{
	StaticDefinition *staticDef = FindStaticDefinitionByProcedure(context, proc);
	if (staticDef)
		return staticDef->name;
	else
		return TPrintF("0x%X", proc);
}

String CGetProcedureSignature(Context *context, Procedure *proc)
{
	String result;

	String procLabel = CProcedureToLabel(context, proc);
	String returnTypeStr = proc->returnTypeTableIdx == TYPETABLEIDX_VOID ? "void"_s : "Register"_s;

	result = TPrintF("%S %S(", returnTypeStr, procLabel);

	if (IRShouldReturnByCopy(context, proc->returnTypeTableIdx))
	{
		if (proc->parameters.size)
			result = StringConcat(result, "Register rRet, "_s);
		else
			result = StringConcat(result, "Register rRet"_s);
	}
	for (int i = 0; i < proc->parameters.size; ++i)
	{
		if (i) result = StringConcat(result, ", "_s);
		Variable *param = proc->parameters[i].variable;
		result = TPrintF("%SRegister param%hhu", result, param->parameterIndex);
	}
	result = StringConcat(result, ")"_s);
	return result;
}

void PrintOut(Context *context, HANDLE outputFile, const char *format, ...)
{
	char *buffer = (char *)g_memory->framePtr;

	va_list args;
	va_start(args, format);

	stbsp_vsprintf(buffer, format, args);

	DWORD bytesWritten;
#if PRINT_C_OUTPUT
	if (!context->config.silent)
	{
		OutputDebugStringA(buffer);
		WriteFile(g_hStdout, buffer, (DWORD)strlen(buffer), &bytesWritten, nullptr); // Stdout
	}
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

	PrintOut(context, outputFile, "#include \"emi.c\"\n\n");

	// Calculate size of structs
	u64 tableSize = BucketArrayCount(&context->typeTable);
	for (int typeInfoIdx = 0; typeInfoIdx < tableSize; ++typeInfoIdx)
	{
		TypeInfo *typeInfo = &context->typeTable[typeInfoIdx];
		if (typeInfo->typeCategory != TYPECATEGORY_STRUCT)
			continue;

		typeInfo->structInfo.size = 0;
		if (!typeInfo->structInfo.isUnion)
		{
			for (int i = 0; i < typeInfo->structInfo.members.size; ++i)
			{
				StructMember *member = &typeInfo->structInfo.members[i];
				member->offset = typeInfo->structInfo.size;
				typeInfo->structInfo.size += CCalculateTypeSize(context, member->typeTableIdx);
			}
		}
		else
		{
			for (int i = 0; i < typeInfo->structInfo.members.size; ++i)
			{
				StructMember *member = &typeInfo->structInfo.members[i];
				member->offset = 0;
				typeInfo->structInfo.size = Max(typeInfo->structInfo.size,
						CCalculateTypeSize(context, member->typeTableIdx));
			}
		}
	}

	const u64 staticVariableCount = context->irStaticVariables.size;
	for (int staticVariableIdx = 0; staticVariableIdx < staticVariableCount; ++staticVariableIdx)
	{
		IRStaticVariable staticVar = context->irStaticVariables[staticVariableIdx];
		staticVar.variable->stackOffset = U64_MAX;

		String varType;
		if (staticVar.initialValue.valueType == IRVALUETYPE_IMMEDIATE_STRING)
			varType = "String"_s;
		else
			varType = CTypeInfoToString(context, staticVar.typeTableIdx);

		s64 size = CCalculateTypeSize(context, staticVar.typeTableIdx);

		if (staticVar.initialValue.valueType == IRVALUETYPE_IMMEDIATE_STRING)
		{
			// @Cleanup
			PrintOut(context, outputFile, "String _%S = { %llu, (u8 *)\"%S\" };\n",
					staticVar.variable->name, staticVar.initialValue.immediateString.size,
					staticVar.initialValue.immediateString);
			PrintOut(context, outputFile, "u8 *%S = &_%S", staticVar.variable->name,
					staticVar.variable->name);
		}
		else
		{
			PrintOut(context, outputFile, "u8 %S[%d]", staticVar.variable->name, size);
			if (staticVar.initialValue.valueType != IRVALUETYPE_INVALID)
			{
				union
				{
					s64 asS64;
					f32 asF32;
					f64 asF64;
				};

				if (staticVar.initialValue.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
				{
					asS64 = staticVar.initialValue.immediate;
				}
				else if (staticVar.initialValue.valueType == IRVALUETYPE_IMMEDIATE_FLOAT)
				{
					if (size == 4)
						asF32 = (f32)staticVar.initialValue.immediateFloat;
					else
						asF64 = staticVar.initialValue.immediateFloat;
				}
				else
				{
					CRASH; // Non literal values not supported here.
				}

				PrintOut(context, outputFile, " = { ");
				s64 scan = asS64;
				for (int i = 0; i < size; ++i)
				{
					if (i)
						PrintOut(context, outputFile, ", ");
					PrintOut(context, outputFile, "0x%02X", scan & 0xFF);
					scan = scan >> 8;
				}
				PrintOut(context, outputFile, " }");
			}
		}
		PrintOut(context, outputFile, ";\n");
	}

	// TypeInfo data
	{
#pragma pack(push, 1) // We'll pack these manually
		struct ProgramTypeInfo
		{
			u8 typeCategory;
		};
		struct ProgramTypeInfoInteger
		{
			u8 typeCategory;
			s32 isSigned;
			s64 size;
		};
		struct ProgramTypeInfoFloating
		{
			u8 typeCategory;
			s64 size;
		};
		struct ProgramStructMemberInfo
		{
			s64 nameSize;
			u8 *nameData;
			void *typeInfo;
			u64 offset;
		};
		struct ProgramTypeInfoStruct
		{
			u8 typeCategory;
			s64 nameSize;
			u8 *nameData;
			s32 isUnion;
			s64 memberCount;
			void *memberData;
			u64 size;
		};
		struct TypeInfoEnum
		{
			u8 typeCategory;
			s64 nameSize;
			u8 *nameData;
			void *typeInfo;
		};
		struct TypeInfoPointer
		{
			u8 typeCategory;
			void *typeInfo;
		};
		struct TypeInfoArray
		{
			u8 typeCategory;
			u64 count;
			void *typeInfo;
		};
#pragma pack(pop)

		for (u64 typeTableIdx = 0; typeTableIdx < tableSize; ++typeTableIdx)
		{
			TypeInfo *typeInfo = &context->typeTable[typeTableIdx];
			switch (typeInfo->typeCategory)
			{
			case TYPECATEGORY_INTEGER:
			{
				PrintOut(context, outputFile, "ProgramTypeInfoInteger _typeInfo%lld = { %d, "
						"%d, %lld };\n",
						typeTableIdx, 0, typeInfo->integerInfo.isSigned, typeInfo->integerInfo.size);
			} break;
			case TYPECATEGORY_FLOATING:
			{
				PrintOut(context, outputFile, "ProgramTypeInfoFloating _typeInfo%lld = { %d, "
						"%lld };\n",
						typeTableIdx, 1, typeInfo->floatingInfo.size);
			} break;
			case TYPECATEGORY_STRUCT:
			{
				String memberArrayName = TPrintF("_memberInfos%d", typeTableIdx);

				String structName = ""_s;
				StaticDefinition *staticDefStruct = FindStaticDefinitionByTypeTableIdx(context,
						typeTableIdx);
				if (staticDefStruct)
					structName = staticDefStruct->name;

				PrintOut(context, outputFile, "ProgramStructMemberInfo %S[] = { ", memberArrayName);
				for (int memberIdx = 0; memberIdx < typeInfo->structInfo.members.size; ++memberIdx)
				{
					StructMember member = typeInfo->structInfo.members[memberIdx];
					if (memberIdx) PrintOut(context, outputFile, ", ");
					PrintOut(context, outputFile, "{ %lld, \"%S\", &_typeInfo%lld, %llu }",
							member.name.size, member.name, member.typeTableIdx, member.offset);
				}
				PrintOut(context, outputFile, " };\n");

				PrintOut(context, outputFile, "ProgramTypeInfoStruct _typeInfo%lld = { %d, "
						"%lld, \"%S\", %d, %lld, %S, %llu };\n",
						typeTableIdx, 2, structName.size, structName,
						(s32)typeInfo->structInfo.isUnion, typeInfo->structInfo.members.size,
						memberArrayName);
			} break;
			case TYPECATEGORY_ENUM:
			{
				String enumName = ""_s;
				StaticDefinition *staticDefStruct = FindStaticDefinitionByTypeTableIdx(context,
						typeTableIdx);
				if (staticDefStruct)
					enumName = staticDefStruct->name;

				PrintOut(context, outputFile, "ProgramTypeInfoEnum _typeInfo%lld = { %d, "
						"%lld, \"%S\", &_typeInfo%lld };\n",
						typeTableIdx, 3, enumName.size, enumName, typeInfo->enumInfo.typeTableIdx);
			} break;
			case TYPECATEGORY_POINTER:
			{
				PrintOut(context, outputFile, "ProgramTypeInfoPointer _typeInfo%lld = { %d, "
						"&_typeInfo%lld };\n",
						typeTableIdx, 4, typeInfo->pointerInfo.pointedTypeTableIdx);
			} break;
			case TYPECATEGORY_ARRAY:
			{
				PrintOut(context, outputFile, "ProgramTypeInfoArray _typeInfo%lld = { %d, "
						"%llu, &_typeInfo%lld };\n",
						typeTableIdx, 5, typeInfo->arrayInfo.count,
						typeInfo->arrayInfo.elementTypeTableIdx);
			} break;
			case TYPECATEGORY_INVALID:
			{
				PrintOut(context, outputFile, "ProgramTypeInfo _typeInfo%lld = { %d };\n",
						typeTableIdx, 6);
			} break;
			}
		}
	}

	PrintOut(context, outputFile, "\n");

	PrintOut(context, outputFile, "// Forward declaration of procedures\n");
	const u64 procedureCount = BucketArrayCount(&context->procedures);
	for (int procedureIdx = 0; procedureIdx < procedureCount; ++procedureIdx)
	{
		Procedure *proc = &context->procedures[procedureIdx];

		// @Speed: separate array of external procedures to avoid branching
		if (proc->isExternal)
		{
			String returnTypeStr = CTypeInfoToString(context, proc->returnTypeTableIdx);
			PrintOut(context, outputFile, "%S %S(", returnTypeStr, CProcedureToLabel(context, proc));

			for (int i = 0; i < proc->parameters.size; ++i)
			{
				if (i) PrintOut(context, outputFile, ", ");
				Variable *param = proc->parameters[i].variable;
				String type = CTypeInfoToString(context, param->typeTableIdx);
				PrintOut(context, outputFile, "%S %S", type, param->name);
			}
			if (proc->isVarargs)
				PrintOut(context, outputFile, ", ...");
			PrintOut(context, outputFile, ");\n");
		}
		else
		{
			String signature = CGetProcedureSignature(context, proc);
			PrintOut(context, outputFile, "%S;\n", signature);
		}
	}

	PrintOut(context, outputFile, "\n");

	for (int procedureIdx = 0; procedureIdx < procedureCount; ++procedureIdx)
	{
		Procedure *proc = &context->procedures[procedureIdx];

		// @Speed: separate array of external procedures to avoid branching
		if (proc->isExternal)
			continue;

		String signature = CGetProcedureSignature(context, proc);
		PrintOut(context, outputFile, "%S {\n", signature);

		u64 stackSize = 0;
		// Dry run to know stack size
		const u64 instructionCount = BucketArrayCount(&proc->instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			IRInstruction inst = proc->instructions[instructionIdx];
			if (inst.type == IRINSTRUCTIONTYPE_VARIABLE_DECLARATION)
			{
				stackSize += CCalculateTypeSize(context,
						inst.variableDeclaration.variable->typeTableIdx);
			}
		}
		stackCursor = stackSize;

		if (stackSize)
			PrintOut(context, outputFile, "u8 stack[0x%x];\n", stackSize);

		// Declare registers
		if (proc->registerCount)
		{
			PrintOut(context, outputFile, "Register r0");
			for (int regIdx = 1; regIdx < proc->registerCount; ++regIdx)
			{
				PrintOut(context, outputFile, ", r%d", regIdx);
			}
			PrintOut(context, outputFile, ";\n");
		}
		if (!IRShouldReturnByCopy(context, proc->returnTypeTableIdx))
			PrintOut(context, outputFile, "Register rRet;\n");
		PrintOut(context, outputFile, "Register rDoRet;\n");

		PrintOut(context, outputFile, "\n");

		// Add parameters to variable stack
		for (int paramIdx = 0; paramIdx < proc->parameters.size; ++paramIdx)
		{
			Variable *param = proc->parameters[paramIdx].variable;
			param->stackOffset = U64_MAX; // FFFFFFFF offset means parameter @Cleanup
		}

		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			IRInstruction inst = proc->instructions[instructionIdx];

			if (inst.type != IRINSTRUCTIONTYPE_LABEL)
				PrintOut(context, outputFile, "\t");

			if (inst.type == IRINSTRUCTIONTYPE_COMMENT)
				PrintOut(context, outputFile, "// %S\n", inst.comment);
			else if (inst.type == IRINSTRUCTIONTYPE_VARIABLE_DECLARATION)
			{
				Variable *var = inst.variableDeclaration.variable;
				u64 size = CCalculateTypeSize(context,
						inst.variableDeclaration.variable->typeTableIdx);
				stackCursor -= size;
				var->stackOffset = stackCursor;

				PrintOut(context, outputFile, "/* Declare variable '%S' at stack + 0x%x (size %d) */\n",
						var->name, var->stackOffset, size);
			}
			else if (inst.type >= IRINSTRUCTIONTYPE_UNARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_UNARY_END)
			{
				String out = CIRValueToStr(context, inst.unaryOperation.out);
				String in = CIRValueToStr(context, inst.unaryOperation.in);
				String op = OperatorToStr(inst);
				PrintOut(context, outputFile, "%S = %S%S;\n", out, op, in);
			}
			else if (inst.type >= IRINSTRUCTIONTYPE_BINARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_BINARY_END)
			{
				String out = CIRValueToStr(context, inst.binaryOperation.out);
				String left = CIRValueToStr(context, inst.binaryOperation.left);
				String op = OperatorToStr(inst);
				String right = CIRValueToStr(context, inst.binaryOperation.right);
				PrintOut(context, outputFile, "%S = %S %S %S;\n", out, left, op, right);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_ASSIGNMENT)
			{
				String src = CIRValueToStr(context, inst.assignment.src);
				String dst = CIRValueToStr(context, inst.assignment.dst);
				PrintOut(context, outputFile, "%S = %S;\n", dst, src);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_MEMBER_ACCESS)
			{
				u64 offset = inst.memberAccess.structMember->offset;

				String out = CIRValueToStr(context, inst.memberAccess.out);
				String base = CIRValueToStr(context, inst.memberAccess.in);
				PrintOut(context, outputFile, "%S = %S + %llu;\n", out, base, offset);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_ARRAY_ACCESS)
			{
				String out = CIRValueToStr(context, inst.arrayAccess.out);
				String array = CIRValueToStr(context, inst.arrayAccess.array);
				String index = CIRValueToStr(context, inst.arrayAccess.index);
				u64 elementSize = CCalculateTypeSize(context, inst.arrayAccess.elementTypeTableIdx);
				PrintOut(context, outputFile, "%S = %S + %S * %llu;\n", out, array, index, elementSize);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_PROCEDURE_CALL)
			{
				if (inst.procedureCall.out.valueType != IRVALUETYPE_INVALID)
				{
					String out;
					if (inst.procedureCall.procedure->isExternal)
						out = CIRValueToStr(context, inst.procedureCall.out);
					else
						out = CIRValueToStrAsRegister(context, inst.procedureCall.out);
					PrintOut(context, outputFile, "%S = ", out);
				}
				String procLabel = CProcedureToLabel(context, inst.procedureCall.procedure);
				PrintOut(context, outputFile, "%S(", procLabel);
				for (int i = 0; i < inst.procedureCall.parameters.size; ++i)
				{
					String param;
					if (inst.procedureCall.procedure->isExternal)
						param = CIRValueToStr(context, inst.procedureCall.parameters[i]);
					else
						param = CIRValueToStrAsRegister(context, inst.procedureCall.parameters[i]);
					if (i > 0) PrintOut(context, outputFile, ", ");
					PrintOut(context, outputFile, "%S", param);
				}
				PrintOut(context, outputFile, ");\n");
			}
			else if (inst.type == IRINSTRUCTIONTYPE_RETURN)
			{
				PrintOut(context, outputFile, "return rRet;\n");
			}
			else if (inst.type == IRINSTRUCTIONTYPE_LABEL)
			{
				String label = inst.label;
				PrintOut(context, outputFile, "%S:\n", label);
				if (instructionIdx == instructionCount - 1)
					// Label can't be right before a closing brace
					PrintOut(context, outputFile, ";\n");
			}
			else if (inst.type == IRINSTRUCTIONTYPE_JUMP)
			{
				String label = inst.conditionalJump.label;
				PrintOut(context, outputFile, "goto %S;\n", label);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_JUMP_IF_ZERO)
			{
				String label = inst.conditionalJump.label;
				String condition = CIRValueToStr(context, inst.conditionalJump.condition);
				PrintOut(context, outputFile, "if (!%S) goto %S;\n", condition, label);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY)
			{
				String src = CIRValueToStr(context, inst.memcpy.src);
				String dst = CIRValueToStr(context, inst.memcpy.dst);
				String size = CIRValueToStr(context, inst.memcpy.size);
				PrintOut(context, outputFile, "memcpy(%S, %S, %S);\n", dst, src, size);
			}
			else
			{
				PrintOut(context, outputFile, "???INST\n");
			}
		}
		PrintOut(context, outputFile, "}\n\n");
	}

	CloseHandle(outputFile);
}
