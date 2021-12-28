inline String PIRValueToStr(Context *context, u32 valueIdx)
{
	Value v = context->values[valueIdx];
	if (v.name)
		return TPrintF("$\"%S\"", v.name);
	else
		return TPrintF("$v%d", valueIdx);
}

void PrintIRValue(Context *context, IRValue value)
{
	if (value.valueType == IRVALUETYPE_MEMORY)
	{
		Print("[%S", PIRValueToStr(context, value.valueIdx));
		if (value.memory.offset)
			Print("+0x%llx", value.memory.offset);
		Print("]");
	}
	else if (value.valueType == IRVALUETYPE_VALUE)
	{
		Print("%S", PIRValueToStr(context, value.valueIdx));
	}
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		Print("0x%llx", value.immediate);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_FLOAT)
		Print("%f", value.immediateFloat);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_STRING)
		Print("\"%S\"", context->stringLiterals[value.immediateStringIdx]);
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
	case IRINSTRUCTIONTYPE_BITWISE_NOT:
		Print("~");
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
		Print("\t// %S\n", inst.comment);
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
		String name = GetProcedure(context,
				inst.procedureCall.procedureIdx)->name;
		Print("call %S(", name);

		for (int i = 0; i < inst.procedureCall.parameters.size; ++i)
		{
			if (i) Print(", ");
			PrintIRValue(context, inst.procedureCall.parameters[i]);
		}
		Print(")\n");
	} break;
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL_INDIRECT:
	{
		if (inst.procedureCall.out.valueType != IRVALUETYPE_INVALID)
		{
			PrintIRValue(context, inst.procedureCall.out);
			Print(" := ");
		}
		Print("call %S(", PIRValueToStr(context, inst.procedureCall.procPointerValueIdx));

		for (int i = 0; i < inst.procedureCall.parameters.size; ++i)
		{
			if (i) Print(", ");
			PrintIRValue(context, inst.procedureCall.parameters[i]);
		}
		Print(")\n");
	} break;
	case IRINSTRUCTIONTYPE_PUSH_VALUE:
	{
		Print("push value %S\n", PIRValueToStr(context, inst.pushValue.valueIdx));
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
	case IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS:
	{
		PrintIRValue(context, inst.assignment.dst);
		Print(" = ^");
		PrintIRValue(context, inst.assignment.src);
		Print("\n");
	} break;
	case IRINSTRUCTIONTYPE_COPY_MEMORY:
	{
		Print("copyMemory(");
		PrintIRValue(context, inst.copyMemory.dst);
		Print(", ");
		PrintIRValue(context, inst.copyMemory.src);
		Print(", ");
		PrintIRValue(context, inst.copyMemory.size);
		Print(")\n");
	} break;
	default:
	{
		Print("???INST\n");
	}
	}
}

void PrintIRInstructions(Context *context)
{
	const int padding = 20;
	const u64 procedureCount = BucketArrayCount(&context->procedures);
	for (int procedureIdx = 1; procedureIdx < procedureCount; ++procedureIdx)
	{
		Procedure *proc = GetProcedure(context, procedureIdx);
		TypeInfoProcedure procTypeInfo = context->typeTable[proc->typeTableIdx].procedureInfo;

		String returnTypeStr = TypeInfoToString(context, procTypeInfo.returnTypeTableIdx);

		String name = GetProcedure(context, procedureIdx)->name;
		Print("proc %S(", name);

		for (int paramIdx = 0; paramIdx < proc->parameterValues.size; ++paramIdx)
		{
			if (paramIdx) Print(", ");
			u32 paramValueIdx = proc->parameterValues[paramIdx];
			Value paramValue = context->values[paramValueIdx];
			String typeStr = TypeInfoToString(context, paramValue.typeTableIdx);
			Print("%S : %S", paramValue.name, typeStr);
		}
		Print(")");
		if (procTypeInfo.returnTypeTableIdx != TYPETABLEIDX_VOID)
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
