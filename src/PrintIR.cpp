void PrintIRValue(Context *context, IRValue value)
{
	if (value.dereference)
		Print("[");

	if (value.valueType == IRVALUETYPE_REGISTER)
		if (value.registerIdx == IRSPECIALREGISTER_RETURN)
			Print("$rRet");
		else if (value.registerIdx == IRSPECIALREGISTER_SHOULD_RETURN)
			Print("$rDoRet");
		else
			Print("$r%d", value.registerIdx);
	else if (value.valueType == IRVALUETYPE_STACK_OFFSET)
		Print("stack+0x%llx", value.stackOffset);
	else if (value.valueType == IRVALUETYPE_DATA_OFFSET)
		Print("%S+0x%llx", value.dataOffset.variable->name, value.dataOffset.offset);
	else if (value.valueType == IRVALUETYPE_PARAMETER)
		Print("param%hhd", value.parameterIdx);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		Print("0x%x", value.immediate);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_FLOAT)
		Print("%f", value.immediateFloat);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_STRING)
		Print("%S", value.immediateString);
	else if (value.valueType == IRVALUETYPE_SIZEOF)
		Print("sizeof(%lld)", value.sizeOfTypeTableIdx);
	else if (value.valueType == IRVALUETYPE_TYPEOF)
		Print("typeof(%lld)", value.sizeOfTypeTableIdx);
	else
		Print("???");

	if (value.dereference)
		Print("]");

	//Print(" : %S", TypeInfoToString(context, value.typeTableIdx));
}

void PrintIRInstruction(Context *context, IRInstruction inst)
{
	if (inst.type == IRINSTRUCTIONTYPE_LABEL)
	{
		Print("%S: ", inst.label);
	}
	else if (inst.type == IRINSTRUCTIONTYPE_NOP)
	{
		Print("NOP", inst.comment);
	}
	else if (inst.type == IRINSTRUCTIONTYPE_COMMENT)
	{
		Print("// \"%S\"", inst.comment);
	}
	else if (inst.type == IRINSTRUCTIONTYPE_JUMP)
	{
		Print("jump \"%S\"", inst.jump.label);
	}
	else if (inst.type == IRINSTRUCTIONTYPE_JUMP_IF_ZERO)
	{
		Print("if !");
		PrintIRValue(context, inst.conditionalJump.condition);
		Print(" jump %S", inst.conditionalJump.label);
	}
	else if (inst.type == IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO)
	{
		Print("if ");
		PrintIRValue(context, inst.conditionalJump.condition);
		Print(" jump %S", inst.conditionalJump.label);
	}
	else if (inst.type == IRINSTRUCTIONTYPE_PROCEDURE_CALL)
	{
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
		Print(")");
	}
	else if (inst.type == IRINSTRUCTIONTYPE_RETURN)
	{
		Print("return");
	}
	else if (inst.type == IRINSTRUCTIONTYPE_ASSIGNMENT)
	{
		PrintIRValue(context, inst.assignment.dst);
		Print(" = ");
		PrintIRValue(context, inst.assignment.src);
	}
	else if (inst.type >= IRINSTRUCTIONTYPE_UNARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_UNARY_END)
	{
		PrintIRValue(context, inst.unaryOperation.out);
		Print(" := ");
		PrintIRInstructionOperator(inst);
		PrintIRValue(context, inst.unaryOperation.in);
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
	}
	else if (inst.type == IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY)
	{
		Print("memcpy(");
		PrintIRValue(context, inst.memcpy.dst);
		Print(", ");
		PrintIRValue(context, inst.memcpy.src);
		Print(", ");
		PrintIRValue(context, inst.memcpy.size);
		Print(")");
	}
	else
	{
		Print("???INST");
	}
	Print("\n");
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
				if (nextInst.type != IRINSTRUCTIONTYPE_LABEL)
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
