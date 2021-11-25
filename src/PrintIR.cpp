inline String PIRRegisterToString(s64 registerIdx)
{
	if (registerIdx == IRSPECIALREGISTER_STACK_BASE)
		return "rBP"_s;
	else if (registerIdx == IRSPECIALREGISTER_RETURN)
		return "rRet"_s;
	else if (registerIdx == IRSPECIALREGISTER_SHOULD_RETURN)
		return "rDoRet"_s;
	else
		return TPrintF("r%lld", registerIdx);
}

void PrintIRValue(Context *context, IRValue value)
{
	if (value.valueType == IRVALUETYPE_MEMORY_REGISTER)
	{
		Print("[$%S", PIRRegisterToString(value.memory.baseRegister));
		if (value.memory.offset)
			Print("+0x%llx", value.memory.offset);
		Print("]");
	}
	else if (value.valueType == IRVALUETYPE_MEMORY_VARIABLE)
	{
		Print("[&%S", value.memory.baseVariable->name);
		if (value.memory.offset)
			Print("+0x%llx", value.memory.offset);
		Print("]");
	}
	else if (value.valueType == IRVALUETYPE_REGISTER)
		Print("$%S", PIRRegisterToString(value.registerIdx));
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		Print("0x%llx", value.immediate);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_FLOAT)
		Print("%f", value.immediateFloat);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_STRING)
		Print("%S", value.immediateString);
	else
		Print("???");


	Print(" : %S", TypeInfoToString(context, value.typeTableIdx));
}

void PrintIRInstructionOperator(IRInstruction inst)
{
	switch (inst.type)
	{
	case IRINSTRUCTIONTYPE_ADD:
		Print("+");
		break;
	case IRINSTRUCTIONTYPE_SUBTRACT:
	case IRINSTRUCTIONTYPE_SUBTRACT_UNARY:
		Print("-");
		break;
	case IRINSTRUCTIONTYPE_MULTIPLY:
		Print("*");
		break;
	case IRINSTRUCTIONTYPE_DIVIDE:
		Print("/");
		break;
	case IRINSTRUCTIONTYPE_MODULO:
		Print("%");
		break;
	case IRINSTRUCTIONTYPE_SHIFT_LEFT:
		Print("<<");
		break;
	case IRINSTRUCTIONTYPE_SHIFT_RIGHT:
		Print(">>");
		break;
	case IRINSTRUCTIONTYPE_OR:
		Print("||");
		break;
	case IRINSTRUCTIONTYPE_AND:
		Print("&&");
		break;
	case IRINSTRUCTIONTYPE_BITWISE_OR:
		Print("|");
		break;
	case IRINSTRUCTIONTYPE_BITWISE_XOR:
		Print("^");
		break;
	case IRINSTRUCTIONTYPE_BITWISE_AND:
		Print("&");
		break;
	case IRINSTRUCTIONTYPE_EQUALS:
		Print("==");
		break;
	case IRINSTRUCTIONTYPE_GREATER_THAN:
		Print(">");
		break;
	case IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS:
		Print(">=");
		break;
	case IRINSTRUCTIONTYPE_LESS_THAN:
		Print("<");
		break;
	case IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS:
		Print("<=");
		break;
	case IRINSTRUCTIONTYPE_NOT:
		Print("!");
		break;
	case IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS:
		Print("address of ");
		break;
	case IRINSTRUCTIONTYPE_CONVERT_INT_TO_F32:
	case IRINSTRUCTIONTYPE_CONVERT_F64_TO_F32:
		Print("to F32 ");
		break;
	case IRINSTRUCTIONTYPE_CONVERT_INT_TO_F64:
	case IRINSTRUCTIONTYPE_CONVERT_F32_TO_F64:
		Print("to F32 ");
		break;
	case IRINSTRUCTIONTYPE_CONVERT_F32_TO_INT:
	case IRINSTRUCTIONTYPE_CONVERT_F64_TO_INT:
		Print("to int ");
		break;
	default:
		CRASH;
		Print("<?>");
	}
}

void PrintIRInstruction(Context *context, IRInstruction inst)
{
	if (inst.type >= IRINSTRUCTIONTYPE_UNARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_UNARY_END)
	{
		PrintIRValue(context, inst.unaryOperation.out);
		Print(" := ");
		PrintIRInstructionOperator(inst);
		PrintIRValue(context, inst.unaryOperation.in);
		Print("\n");
	}
	else if (inst.type >= IRINSTRUCTIONTYPE_BINARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_BINARY_END)
	{
		PrintIRValue(context, inst.binaryOperation.out);
		Print(" := ");
		PrintIRValue(context, inst.binaryOperation.left);
		Print(" ");
		PrintIRInstructionOperator(inst);
		Print(" ");
		PrintIRValue(context, inst.binaryOperation.right);
		Print("\n");
	}
	else switch (inst.type)
	{
	case IRINSTRUCTIONTYPE_LABEL:
	{
		Print("%S:\n", inst.label);
	} break;
	case IRINSTRUCTIONTYPE_NOP:
	{
		Print("NOP\n", inst.comment);
	} break;
	case IRINSTRUCTIONTYPE_COMMENT:
	{
		Print("// \"%S\"\n", inst.comment);
	} break;
	case IRINSTRUCTIONTYPE_JUMP:
	{
		Print("jump \"%S\"\n", inst.jump.label);
	} break;
	case IRINSTRUCTIONTYPE_JUMP_IF_ZERO:
	{
		Print("if !");
		PrintIRValue(context, inst.conditionalJump.condition);
		Print(" jump %S\n", inst.conditionalJump.label);
	} break;
	case IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO:
	{
		Print("if ");
		PrintIRValue(context, inst.conditionalJump.condition);
		Print(" jump %S\n", inst.conditionalJump.label);
	} break;
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL:
	{
		if (inst.procedureCall.out.valueType != IRVALUETYPE_INVALID)
		{
			PrintIRValue(context, inst.procedureCall.out);
			Print(" := ");
		}
		StaticDefinition *procStaticDef = FindStaticDefinitionByProcedure(context,
				inst.procedureCall.procedure);
		if (procStaticDef)
			Print("call %S(", procStaticDef->name);
		else
			Print("call 0x%X(", inst.procedureCall.procedure);

		for (int i = 0; i < inst.procedureCall.parameters.size; ++i)
		{
			if (i) Print(", ");
			PrintIRValue(context, inst.procedureCall.parameters[i]);
		}
		Print(")\n");
	} break;
	case IRINSTRUCTIONTYPE_PUSH_VARIABLE:
	{
		Print("push variable %S\n", inst.pushVariable.variable->name);
	} break;
	case IRINSTRUCTIONTYPE_PUSH_SCOPE:
	{
		Print("push scope\n");
	} break;
	case IRINSTRUCTIONTYPE_POP_SCOPE:
	{
		Print("pop scope\n");
	} break;
	case IRINSTRUCTIONTYPE_RETURN:
	{
		Print("return\n");
	} break;
	case IRINSTRUCTIONTYPE_ASSIGNMENT:
	{
		PrintIRValue(context, inst.assignment.dst);
		Print(" = ");
		PrintIRValue(context, inst.assignment.src);
		Print("\n");
	} break;
	case IRINSTRUCTIONTYPE_GET_PARAMETER:
	{
		PrintIRValue(context, inst.getParameter.dst);
		Print(" = param%lld\n", inst.getParameter.parameterIdx);
	} break;
	case IRINSTRUCTIONTYPE_GET_TYPE_INFO:
	{
		PrintIRValue(context, inst.getTypeInfo.out);
		Print(" = &_typeInfo%lld\n", inst.getTypeInfo.typeTableIdx);
	} break;
	case IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS:
	{
		PrintIRValue(context, inst.assignment.dst);
		Print(" = ^");
		PrintIRValue(context, inst.assignment.src);
		Print("\n");
	} break;
	case IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY:
	{
		Print("memcpy(");
		PrintIRValue(context, inst.memcpy.dst);
		Print(", ");
		PrintIRValue(context, inst.memcpy.src);
		Print(", ");
		PrintIRValue(context, inst.memcpy.size);
		Print(")\n");
	} break;
	case IRINSTRUCTIONTYPE_PATCH:
	{
		PrintIRInstruction(context, *inst.patch.first);
		PrintIRInstruction(context, *inst.patch.second);
	} break;
	case IRINSTRUCTIONTYPE_PATCH_MANY:
	{
		for (int i = 0; i < inst.patchMany.instructions.size; ++i)
			PrintIRInstruction(context, inst.patchMany.instructions[i]);
	} break;
	default:
	{
		Print("???INST\n");
	}
	}
}

#if PRINT_IR
void PrintIRInstructions(Context *context)
{
	const int padding = 20;
	const u64 procedureCount = BucketArrayCount(&context->procedures);
	for (int procedureIdx = 0; procedureIdx < procedureCount; ++procedureIdx)
	{
		Procedure *proc = &context->procedures[procedureIdx];
		if (proc->isExternal)
			continue;

		String returnTypeStr = TypeInfoToString(context, proc->returnTypeTableIdx);

		StaticDefinition *staticDef = FindStaticDefinitionByProcedure(context, proc);
		if (staticDef)
			Print("proc %S(", staticDef->name);
		else
			Print("proc 0x%X(", proc);

		for (int paramIdx = 0; paramIdx < proc->parameters.size; ++paramIdx)
		{
			if (paramIdx) Print(", ");
			String typeStr = TypeInfoToString(context, proc->parameters[paramIdx].variable->typeTableIdx);
			Print("param%d : %S", paramIdx, typeStr);
		}
		Print(")");
		if (proc->returnTypeTableIdx != TYPETABLEIDX_VOID)
			Print(" -> %S", returnTypeStr);
		Print("\n");

		const u64 instructionCount = BucketArrayCount(&proc->instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			IRInstruction inst = proc->instructions[instructionIdx];

			if (inst.type == IRINSTRUCTIONTYPE_LABEL)
			{
				Print("%S: ", inst.label->name);

				IRInstruction nextInst = proc->instructions[instructionIdx + 1];
				if (nextInst.type != IRINSTRUCTIONTYPE_LABEL &&
					nextInst.type != IRINSTRUCTIONTYPE_PUSH_SCOPE &&
					nextInst.type != IRINSTRUCTIONTYPE_POP_SCOPE &&
					nextInst.type != IRINSTRUCTIONTYPE_NOP)
				{
					for (s64 i = inst.label->name.size + 2; i < padding; ++i)
						Print(" ");

					++instructionIdx;
					if (instructionIdx >= instructionCount)
						break;
					inst = proc->instructions[instructionIdx];
				}
				else
				{
					Print("\n");
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
					Print(" ");
			}

			PrintIRInstruction(context, inst);
		}
	}
	Print("\n");
}
#endif
