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
		ASSERT(!"Value can't be of struct type!");
		return "strct"_s;
	case TYPECATEGORY_ARRAY:
		ASSERT(!"Value can't be of array type!");
		return "arr"_s;
	case TYPECATEGORY_ENUM:
		return CTypeInfoToString(context, typeInfo->enumInfo.typeTableIdx);
	case TYPECATEGORY_INTEGER:
	{
		if (typeInfo->integerInfo.isSigned) switch (typeInfo->size)
		{
			case 1: return "s8"_s;
			case 2: return "s16"_s;
			case 4: return "s32"_s;
			case 8: return "s64"_s;
		}
		else switch (typeInfo->size)
		{
			case 1: return "u8"_s;
			case 2: return "u16"_s;
			case 4: return "u32"_s;
			case 8: return "u64"_s;
		}
	} break;
	case TYPECATEGORY_FLOATING:
	{
		switch (typeInfo->size)
		{
			case 4: return "f32"_s;
			case 8: return "f64"_s;
		}
	} break;
	}
	return "???TYPE"_s;
}

String CRegisterToStr(s64 registerIdx)
{
	switch (registerIdx)
	{
	case IRSPECIALREGISTER_STACK_BASE:
		return "rBasePtr"_s;
	case IRSPECIALREGISTER_RETURN:
		return "rRet"_s;
	case IRSPECIALREGISTER_SHOULD_RETURN:
		return "rDoRet"_s;
	default:
		ASSERT(registerIdx >= 0);
		return TPrintF("r%lld", registerIdx);
	}
}

String CIRValueToStr(Context *context, IRValue value, bool asPointer = false)
{
	String result = "???VALUE"_s;

	switch (value.valueType)
	{
	case IRVALUETYPE_REGISTER:
	{
		String memberName = CTypeInfoToString(context, value.typeTableIdx);
		result = TPrintF("%S.%S_", CRegisterToStr(value.registerIdx), memberName);
	} break;
	case IRVALUETYPE_MEMORY_REGISTER:
	case IRVALUETYPE_MEMORY_VARIABLE:
	{
		s64 offset = value.memory.offset;
		if (value.valueType == IRVALUETYPE_MEMORY_VARIABLE)
		{
			if (value.memory.baseVariable->isStatic)
				result = TPrintF("%S", value.memory.baseVariable->name);
			else
			{
				ASSERT(value.memory.baseVariable->isAllocated);
				result = "rBasePtr.ptr_"_s;
				offset += value.memory.baseVariable->stackOffset;
			}
		}
		else
			result = TPrintF("%S.ptr_", CRegisterToStr(value.memory.baseRegister));

		if (offset)
		{
			if (offset > 0)
				result = TPrintF("%S+0x%llx", result, offset);
			else
				result = TPrintF("%S-0x%llx", result, -offset);
		}

		if (!asPointer)
		{
			String castStr = CTypeInfoToString(context, value.typeTableIdx);
			result = TPrintF("*(%S*)(%S)", castStr, result);
		}
	} break;
	case IRVALUETYPE_IMMEDIATE_INTEGER:
	{
		result = TPrintF("0x%llx", value.immediate);
	} break;
	case IRVALUETYPE_IMMEDIATE_FLOAT:
	{
		result = TPrintF("%f", value.immediateFloat);
	} break;
	case IRVALUETYPE_TYPEOF:
	{
		result = TPrintF("&_typeInfo%lld", value.typeOfTypeTableIdx);
	} break;
	default:
		ASSERT(!"Invalid value type!");
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
	case IRINSTRUCTIONTYPE_SHIFT_LEFT:
		return "<<"_s;
	case IRINSTRUCTIONTYPE_SHIFT_RIGHT:
		return ">>"_s;
	case IRINSTRUCTIONTYPE_OR:
		return "||"_s;
	case IRINSTRUCTIONTYPE_AND:
		return "&&"_s;
	case IRINSTRUCTIONTYPE_BITWISE_OR:
		return "|"_s;
	case IRINSTRUCTIONTYPE_BITWISE_XOR:
		return "^"_s;
	case IRINSTRUCTIONTYPE_BITWISE_AND:
		return "&"_s;
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
	case IRINSTRUCTIONTYPE_BITWISE_NOT:
		return "!"_s;
	default:
		ASSERT(!"Unrecognized operator");
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

	if (IRShouldPassByCopy(context, proc->returnTypeTableIdx))
	{
		if (proc->parameters.size)
			result = StringConcat(result, "Register rRet, "_s);
		else
			result = StringConcat(result, "Register rRet"_s);
	}
	for (int i = 0; i < proc->parameters.size; ++i)
	{
		if (i) result = StringConcat(result, ", "_s);
		result = TPrintF("%SRegister param%hhu", result, i);
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

void CPrintOutInstruction(Context *context, HANDLE outputFile, IRInstruction inst)
{
	// Silent instructions
	switch (inst.type)
	{
	case IRINSTRUCTIONTYPE_NOP:
	case IRINSTRUCTIONTYPE_PUSH_SCOPE:
	case IRINSTRUCTIONTYPE_POP_SCOPE:
		return;
	}

	if (inst.type != IRINSTRUCTIONTYPE_LABEL)
		PrintOut(context, outputFile, "\t");

	switch (inst.type)
	{
	case IRINSTRUCTIONTYPE_PUSH_VARIABLE:
	{
		PrintOut(context, outputFile, "/* Push variable \"%S\" to stackBase - 0x%llx */\n",
				inst.pushVariable.variable->name, -inst.pushVariable.variable->stackOffset);
	} break;
	case IRINSTRUCTIONTYPE_COMMENT:
	{
		PrintOut(context, outputFile, "// %S\n", inst.comment);
	} break;
	case IRINSTRUCTIONTYPE_ASSIGNMENT:
	{
		String src = CIRValueToStr(context, inst.assignment.src);
		String dst = CIRValueToStr(context, inst.assignment.dst);
		PrintOut(context, outputFile, "%S = %S;\n", dst, src);
	} break;
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL:
	{
		PrintOut(context, outputFile, "{\n");
		for (s64 i = 0; i < (s64)inst.procedureCall.parameters.size; ++i)
		{
			String param = CIRValueToStr(context, inst.procedureCall.parameters[i]);
			String memberName = CTypeInfoToString(context, inst.procedureCall.parameters[i].typeTableIdx);
			PrintOut(context, outputFile, "\t\tRegister param%lld; param%lld.%S_ = %S;\n", i, i, memberName, param);
		}

		PrintOut(context, outputFile, "\t\t");
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
		for (s64 i = 0; i < (s64)inst.procedureCall.parameters.size; ++i)
		{
			if (i > 0) PrintOut(context, outputFile, ", ");

			if (inst.procedureCall.procedure->isExternal)
			{
				String memberName = CTypeInfoToString(context, inst.procedureCall.parameters[i].typeTableIdx);
				PrintOut(context, outputFile, "param%lld.%S_", i, memberName);
			}
			else
				PrintOut(context, outputFile, "param%lld", i);
		}
		PrintOut(context, outputFile, ");\n\t}\n");
	} break;
	case IRINSTRUCTIONTYPE_GET_PARAMETER:
	{
		String dst = CIRValueToStr(context, inst.getParameter.dst);
		String memberName = CTypeInfoToString(context, inst.getParameter.dst.typeTableIdx);
		PrintOut(context, outputFile, "%S = param%lld.%S_;\n", dst, inst.getParameter.parameterIdx, memberName);
	} break;
	case IRINSTRUCTIONTYPE_RETURN:
	{
		PrintOut(context, outputFile, "return rRet;\n");
	} break;
	case IRINSTRUCTIONTYPE_LABEL:
	{
		String label = inst.label->name;
		PrintOut(context, outputFile, "%S:;\n", label);
		//if (instructionIdx == instructionCount - 1) // @Fix
			// Label can't be right before a closing brace
			//PrintOut(context, outputFile, ";\n");
	} break;
	case IRINSTRUCTIONTYPE_JUMP:
	{
		String label = inst.conditionalJump.label->name;
		PrintOut(context, outputFile, "goto %S;\n", label);
	} break;
	case IRINSTRUCTIONTYPE_JUMP_IF_ZERO:
	{
		String label = inst.conditionalJump.label->name;
		String condition = CIRValueToStr(context, inst.conditionalJump.condition);
		PrintOut(context, outputFile, "if (!%S) goto %S;\n", condition, label);
	} break;
	case IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO:
	{
		String label = inst.conditionalJump.label->name;
		String condition = CIRValueToStr(context, inst.conditionalJump.condition);
		PrintOut(context, outputFile, "if (%S) goto %S;\n", condition, label);
	} break;
	case IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY:
	{
		String src = CIRValueToStr(context, inst.memcpy.src);
		String dst = CIRValueToStr(context, inst.memcpy.dst);
		String size = CIRValueToStr(context, inst.memcpy.size);
		PrintOut(context, outputFile, "memcpy(%S, %S, %S);\n", dst, src, size);
	} break;
	case IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS:
	{
		String in = CIRValueToStr(context, inst.unaryOperation.in, true);
		String out = CIRValueToStr(context, inst.unaryOperation.out);
		PrintOut(context, outputFile, "%S = %S;\n", out, in);
	} break;
	case IRINSTRUCTIONTYPE_PATCH:
	{
		CPrintOutInstruction(context, outputFile, *inst.patch.first);
		CPrintOutInstruction(context, outputFile, *inst.patch.second);
	} break;
	case IRINSTRUCTIONTYPE_PATCH_MANY:
	{
		for (int i = 0; i < inst.patchMany.instructions.size; ++i)
			CPrintOutInstruction(context, outputFile, inst.patchMany.instructions[i]);
	} break;
	default:
	{
		if (inst.type >= IRINSTRUCTIONTYPE_UNARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_UNARY_END)
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
		else
		{
			ASSERT(!"Didn't recognize instruction type");
			PrintOut(context, outputFile, "???INST\n");
		}
	}
	}
}

void WriteToC(Context *context)
{
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

	const u64 staticVariableCount = context->irStaticVariables.size;
	for (int staticVariableIdx = 0; staticVariableIdx < staticVariableCount; ++staticVariableIdx)
	{
		IRStaticVariable staticVar = context->irStaticVariables[staticVariableIdx];

		String varType;
		if (staticVar.initialValue.valueType == IRVALUETYPE_IMMEDIATE_STRING)
			varType = "String"_s;
		else
			varType = CTypeInfoToString(context, staticVar.variable->typeTableIdx);

		TypeInfo *typeInfo  = &context->typeTable[staticVar.variable->typeTableIdx];

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
			PrintOut(context, outputFile, "u8 %S[%d]", staticVar.variable->name, typeInfo->size);
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
					if (typeInfo->size == 4)
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
				for (int i = 0; i < typeInfo->size; ++i)
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
		u64 tableSize = BucketArrayCount(&context->typeTable);
		for (u64 typeTableIdx = 0; typeTableIdx < tableSize; ++typeTableIdx)
		{
			TypeInfo *typeInfo = &context->typeTable[typeTableIdx];
			switch (typeInfo->typeCategory)
			{
			case TYPECATEGORY_INTEGER:
			{
				PrintOut(context, outputFile, "ProgramTypeInfoInteger _typeInfo%lld = { 0, "
						"%lld, %d };\n",
						typeTableIdx, typeInfo->size, typeInfo->integerInfo.isSigned);
			} break;
			case TYPECATEGORY_FLOATING:
			{
				PrintOut(context, outputFile, "ProgramTypeInfo _typeInfo%lld = { 1, %lld };\n",
						typeTableIdx, typeInfo->size);
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

				PrintOut(context, outputFile, "ProgramTypeInfoStruct _typeInfo%lld = { 2, "
						"%lld, %lld, \"%S\", %d, %lld, %S };\n",
						typeTableIdx, typeInfo->size, structName.size, structName,
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

				PrintOut(context, outputFile, "ProgramTypeInfoEnum _typeInfo%lld = { 3, "
						"%lld, %lld, \"%S\", &_typeInfo%lld };\n",
						typeTableIdx, typeInfo->size, enumName.size, enumName,
						typeInfo->enumInfo.typeTableIdx);
			} break;
			case TYPECATEGORY_POINTER:
			{
				PrintOut(context, outputFile, "ProgramTypeInfoPointer _typeInfo%lld = { 4, "
						"%lld, &_typeInfo%lld };\n",
						typeTableIdx, typeInfo->size, typeInfo->pointerInfo.pointedTypeTableIdx);
			} break;
			case TYPECATEGORY_ARRAY:
			{
				PrintOut(context, outputFile, "ProgramTypeInfoArray _typeInfo%lld = { 5, "
						"%llu, %llu, &_typeInfo%lld };\n",
						typeTableIdx, typeInfo->size, typeInfo->arrayInfo.count,
						typeInfo->arrayInfo.elementTypeTableIdx);
			} break;
			case TYPECATEGORY_INVALID:
			{
				PrintOut(context, outputFile, "ProgramTypeInfo _typeInfo%lld = { 6 };\n",
						typeTableIdx);
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

		if (proc->stackSize)
		{
			PrintOut(context, outputFile, "\tu8 stack[0x%llx];\n", proc->stackSize);
			PrintOut(context, outputFile, "\tRegister rBasePtr; rBasePtr.ptr_ = stack + 0x%llx;\n", proc->stackSize);
		}

		// Declare registers
		if (proc->registerCount)
		{
			PrintOut(context, outputFile, "\tRegister r0");
			for (int regIdx = 1; regIdx < proc->registerCount; ++regIdx)
			{
				PrintOut(context, outputFile, ", r%d", regIdx);
			}
			PrintOut(context, outputFile, ";\n");
		}
		if (!IRShouldPassByCopy(context, proc->returnTypeTableIdx))
			PrintOut(context, outputFile, "\tRegister rRet;\n");
		PrintOut(context, outputFile, "\tRegister rDoRet;\n");

		PrintOut(context, outputFile, "\n");

		u64 instructionCount = BucketArrayCount(&proc->instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			IRInstruction inst = proc->instructions[instructionIdx];

			CPrintOutInstruction(context, outputFile, inst);
		}
		PrintOut(context, outputFile, "}\n\n");
	}

	CloseHandle(outputFile);

//set CommonCompilerFlags=-MTd -nologo -Gm- -GR- -Od -Oi -EHa- -W0 -wd4098 -wd4201 -wd4100 -wd4996 -wd4063 -wd4305 -FC -Z7 -I ..\external\

	PWSTR programFilesPathWstr;
	SHGetKnownFolderPath(FOLDERID_ProgramFilesX86, 0, NULL, &programFilesPathWstr);
	String programFilesPath = StupidStrToString(programFilesPathWstr, FrameAlloc);

	String visualStudioPath = TPrintF("%S\\Microsoft Visual Studio", programFilesPath);
	{
		String visualStudio2019Path = TPrintF("%S\\2019", visualStudioPath);
		String visualStudio2017Path = TPrintF("%S\\2017", visualStudioPath);
		if (GetFileAttributes(StringToCStr(visualStudio2019Path, FrameAlloc)) != INVALID_FILE_ATTRIBUTES)
			visualStudioPath = visualStudio2019Path;
		else if (GetFileAttributes(StringToCStr(visualStudio2017Path, FrameAlloc)) != INVALID_FILE_ATTRIBUTES)
			visualStudioPath = visualStudio2017Path;
		else
		{
			// Get anything starting with 20...
			String wildcard = TPrintF("%S\\20*", visualStudioPath);
			WIN32_FIND_DATAA foundData = {};
			HANDLE foundFile = FindFirstFileA(StringToCStr(wildcard, FrameAlloc), &foundData);
			if (foundFile != INVALID_HANDLE_VALUE)
				visualStudioPath = TPrintF("%S\\%s", visualStudioPath, foundData.cFileName);
		}
	}

	String msvcPath = {};
	{
		String buildToolsPath = TPrintF("%S\\BuildTools", visualStudioPath);
		String enterprisePath = TPrintF("%S\\Enterprise", visualStudioPath);
		String professionalPath = TPrintF("%S\\Professional", visualStudioPath);
		String communityPath = TPrintF("%S\\Community", visualStudioPath);
		if (GetFileAttributes(StringToCStr(buildToolsPath, FrameAlloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = buildToolsPath;
		else if (GetFileAttributes(StringToCStr(enterprisePath, FrameAlloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = enterprisePath;
		else if (GetFileAttributes(StringToCStr(professionalPath, FrameAlloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = professionalPath;
		else if (GetFileAttributes(StringToCStr(communityPath, FrameAlloc)) != INVALID_FILE_ATTRIBUTES)
			msvcPath = communityPath;
		msvcPath = TPrintF("%S\\VC\\Tools\\MSVC", msvcPath);

		String wildcard = StringConcat(msvcPath, "\\*"_s);
		WIN32_FIND_DATAA foundData = {};
		HANDLE findHandle = FindFirstFileA(StringToCStr(wildcard, FrameAlloc), &foundData);
		if (findHandle != INVALID_HANDLE_VALUE)
		{
			while (foundData.cFileName[0] == '.')
				FindNextFileA(findHandle, &foundData);
			msvcPath = TPrintF("%S\\%s", msvcPath, foundData.cFileName);
		}
	}

	String subsystemArgument;
	if (context->config.windowsSubsystem)
		subsystemArgument = "/subsystem:WINDOWS "_s;
	else
		subsystemArgument = "/subsystem:CONSOLE "_s;
	String windowsSDKPath = "C:\\Program Files (x86)\\Windows Kits\\10"_s;
	String windowsSDKVersion = "10.0.18362.0"_s;
	String commandLine = TPrintF(
			"%S\\bin\\Hostx64\\x64\\cl.exe "
			"out.c "
			"/nologo "
			"/Fobin\\ "
			"/W0 "
			"/Z7 "
			"/I \"%S\\include\" "
			"/I \"%S\\include\\%S\\ucrt\" "
			"/I \"%S\\include\\%S\\shared\" "
			"/I \"%S\\include\\%S\\um\" "
			"/I \"%S\\include\\%S\\winrt\" "
			"/I \"%S\\include\\%S\\cppwinrt\" "
			"/link "
			"%S "
			"User32.lib "
			"/libpath:\"%S\\lib\\x64\" "
			"/libpath:\"%S\\lib\\%S\\ucrt\\x64\" "
			"/libpath:\"%S\\lib\\%S\\um\\x64\" "
			"/out:bin/out.exe%c",
			msvcPath,
			msvcPath,
			windowsSDKPath, windowsSDKVersion,
			windowsSDKPath, windowsSDKVersion,
			windowsSDKPath, windowsSDKVersion,
			windowsSDKPath, windowsSDKVersion,
			windowsSDKPath, windowsSDKVersion,
			subsystemArgument,
			msvcPath,
			windowsSDKPath, windowsSDKVersion,
			windowsSDKPath, windowsSDKVersion,
			0
			);

	STARTUPINFO startupInfo = {};
	PROCESS_INFORMATION processInformation = {};
	startupInfo.cb = sizeof(STARTUPINFO);
	if (!CreateProcessA(
			NULL,
			(LPSTR)commandLine.data,
			NULL,
			NULL,
			false,
			0,
			NULL,
			NULL,
			&startupInfo,
			&processInformation
			))
	{
		Print("Failed to call cl.exe (%d)\n", GetLastError());
		CRASH;
	}
	WaitForSingleObject(processInformation.hProcess, INFINITE);
	CloseHandle(processInformation.hProcess);
	CloseHandle(processInformation.hThread);
}
