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
				Print("%S: ", inst.label);

				IRInstruction nextInst = proc->instructions[instructionIdx + 1];
				if (nextInst.type != IRINSTRUCTIONTYPE_LABEL)
				{
					for (s64 i = inst.label.size + 2; i < padding; ++i)
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

			if (inst.type == IRINSTRUCTIONTYPE_COMMENT)
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
					Print("call %S", procStaticDef->name);
				else
					Print("call 0x%X", inst.procedureCall.procedure);
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
	}
	Print("\n");
}
#endif
